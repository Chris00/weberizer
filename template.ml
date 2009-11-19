(* Parse an HTML file with template annotations and outputs an OCaml
   module that allows to fill the "holes" and output the final file.

   ml:content="<ident>"
   ml:strip="true"   ml:strip="if empty"
   ml:replace="<ident>"
   ${<ident>}
   ${function args}
*)

open Format
open Neturl

type html = Nethtml.document list

(* Helper functions
 ***********************************************************************)

let is_lowercase c = 'a' <= c && c <= 'z'
let is_valid_char c =
  ('0' <= c && c <= '9') || is_lowercase c || ('A' <= c && c <= 'Z') || c = '_'

let rec is_digit_or_letter s i len =
  i >= len || (is_valid_char s.[i] || is_digit_or_letter s (i+1) len)

(* Check that the string is a valid OCaml identifier. *)
let valid_ocaml_id s =
  let len = String.length s in
  len > 0 && is_lowercase s.[0] && is_digit_or_letter s 1 len

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(* Return the *)
let rec skip_spaces s i len =
  if i >= len then len
  else if is_space s.[i] then skip_spaces s (i + 1) len
  else i

(* Split the string [s] at spaces (one or several contiguous spaces).
   The way to have a block with spaces or an empty argument is to
   quote it with double quotes. *)
let rec split_on_spaces s =
  let len = String.length s in
  let i = skip_spaces s 0 len in
  get_split_string s i i len
and get_split_string s i0 i len =
  if i >= len then
    if i0 >= len then [] else [String.sub s i0 (len - i0)]
  else if is_space s.[i] then
    let v = String.sub s i0 (i - i0) in
    let i = skip_spaces s (i + 1) len in
    v :: get_split_string s i i len
  else if s.[i] = '"' then get_quoted_string s (i + 1) (i + 1) len
  else get_split_string s i0 (i + 1) len
and get_quoted_string s i0 i len =
  if i >= len then failwith(sprintf "Unterminated quoted string in %S" s)
  else if s.[i] = '"' && s.[i-1] <> '\\' then
    let v = String.sub s i0 (i - i0) in
    let i = skip_spaces s (i + 1) len in
    v :: get_split_string s i i len
  else get_quoted_string s i0 (i + 1) len

let rec index_nospace s i =
  if i >= String.length s then i
  else if is_space s.[i] then index_nospace s (i+1)
  else i

let rec index_last_nospace s i0 i =
  if i < i0 then i
  else if is_space s.[i] then index_last_nospace s i0 (i - 1)
  else i

let strip_spaces s =
  let i_last = String.length s - 1 in
  let i0 = index_nospace s 0 in
  let i1 = index_last_nospace s i0 i_last in
  if i0 = 0 && i1 = i_last then s
  else String.sub s i0 (i1 - i0 + 1)

let rec is_prefix_loop p s i len_p =
  i >= len_p || (p.[i] = s.[i] && is_prefix_loop p s (i + 1) len_p)

let is_prefix p s =
  let len_p = String.length p in
  len_p <= String.length s  &&  is_prefix_loop p s 0 len_p


(* Parse strings
 *************************************************************************)

module Var =
struct
  (* Type of variables in HTML templates. *)
  type ty = HTML | String | Fun_html | Fun

  let type_to_string = function
    | HTML -> "html"
    | String -> "string"
    | Fun_html -> "fun_html"
    | Fun -> "fun"

  let type_code = function
    | HTML -> "html"
    | String -> "string"
    | Fun_html -> "(string list -> html)"
    | Fun -> "(string list -> string)"

  (* Characteristics of a variable *)
  type t = { mutable ty: ty;             (* var type *)
           }

  (* name -> t *)
  type set = (string, t) Hashtbl.t

  let make () = (Hashtbl.create 10 : set)
  let ty h v = (Hashtbl.find h v).ty

  let write_code_eval fm v args =
    fprintf fm "(eval t t.%s [" v;
    List.iter (fun a -> fprintf fm "%S;" a) args;
    fprintf fm "])"

  let write_code_html fm h v args = match ty h v with
    | HTML -> fprintf fm "eval t t.%s" v
    | String -> fprintf fm "[Nethtml.Data(eval t t.%s)]" v
    | Fun_html -> write_code_eval fm v args;
    | Fun ->
        fprintf fm "[Nethtml.Data(";
        write_code_eval fm v args;
        fprintf fm ")]"

  let write_code_empty fm h v = match ty h v with
    | HTML | Fun_html -> fprintf fm "[]"
    | String | Fun -> fprintf fm "\"\""

  (* Write a binding if needed to avoid multiple evaluations of a function.
     @return the variable name to use. *)
  let binding_no = ref 0
  let write_binding fm h v args = match ty h v with
    | HTML | String -> "eval t t." ^ v
    | Fun_html | Fun ->
        incr binding_no;
        let n = !binding_no in
        fprintf fm "let ocaml_template__%i = " n;
        write_code_eval fm v args;
        fprintf fm " in@\n";
        "ocaml_template__" ^ string_of_int n

  (* Add a new variable.  In case of conflicting types, use the
     "lower" type compatible with both. *)
  let add (h:set) v ty =
    try
      let v' = Hashtbl.find h v in
      match v'.ty, ty with
      | (HTML | String), (Fun_html | Fun) | (Fun_html | Fun), (HTML | String) ->
          failwith(sprintf "The identifier %S cannot be used both as a \
		variable and a function" v)
      | HTML, String | String, HTML -> v'.ty <- String
      | Fun_html, Fun | Fun, Fun_html -> v'.ty <- Fun
      | HTML, HTML | String, String | Fun_html, Fun_html | Fun, Fun -> ()
    with Not_found ->
      Hashtbl.add h v { ty = ty }

  let iter (h:set) f = Hashtbl.iter f h

  (* Iterates on the keys in alphabetical order. *)
  let iter_ab (h:set) f =
    (*  *)
    let l = Hashtbl.fold (fun v t l -> (v, t) :: l) h [] in
    let l = List.sort (fun (v1,_) (v2,_) -> String.compare v1 v2) l in
    List.iter (fun (v,t) -> f v t) l
end

type string_or_var =
  | String of string                    (* literal string *)
  | Var of string                       (* Var(ident) *)
  | Fun of string * string list         (* Fun(ident, args) *)
type subst_string = string_or_var list

let rec parse_string_range add_string add_var acc s i0 i len_s =
  if i >= len_s then
    let len = i - i0 in
    if len = 0 then acc else add_string acc (String.sub s i0 len)
  else if i + 1 < len_s && s.[i] = '$' && s.[i+1] = '{' then
    let len = i - i0 in
    if len = 0 then parse_var add_string add_var acc s (i+2) (i+2) len_s
    else (
      let acc = add_string acc (String.sub s i0 len) in
      parse_var add_string add_var acc s (i+2) (i+2) len_s
    )
  else
    parse_string_range add_string add_var acc s i0 (i+1) len_s
and parse_var add_string add_var acc s i0 i len_s =
  if i >= len_s then
    invalid_arg(sprintf "Missing '}' to close the variable %S"
                  (String.sub s i0 (len_s - i0)))
  else if s.[i] = '}' then (
    let acc = add_var acc (String.sub s i0 (i - i0)) in
    parse_string_range add_string add_var acc s (i+1) (i+1) len_s
  )
  else parse_var add_string add_var acc s i0 (i+1) len_s


let decode_var h v =
  match split_on_spaces v with
  | [] | "" :: _ -> invalid_arg "Empty variables are not allowed"
  | [v] ->
      if valid_ocaml_id v then (Var.add h v Var.String; Var v)
      else invalid_arg(sprintf "Variable %S is not a valid OCaml identifier" v)
  | v :: args ->
      if valid_ocaml_id v then (Var.add h v Var.Fun; Fun(v, args))
      else invalid_arg(sprintf "Function name %S is not valid" v)

let parse_string h s =
  let add_string l s = String s :: l in
  let add_var l v = decode_var h v :: l in
  List.rev(parse_string_range add_string add_var [] s 0 0 (String.length s))


(* Parse Nethtml document : search variables
 ***********************************************************************)

type strip = [ `No | `Yes | `If_empty ]

type document =
  | Element of string * (string * subst_string) list * document list
  | Data of subst_string
  | Content of string * (string * subst_string) list
      * strip * string * string list
      (* Content(el, args, strip default, var, args) : content replacement *)

(* Accumulator keeping given OCaml arguments *)
type ocaml_args = {
  mutable content: string; (* var name or "" *)
  mutable args: string list; (* possible function arguments *)
  mutable strip: strip;
}

let is_ocaml_arg s =
  String.length s > 3 && s.[0] = 'm' && s.[1] = 'l' && s.[2] = ':'

(* [split_args h ml [] all] go through the arguments [all], record the
   "ml:*" arguments in [ml] and returns the other arguments.  These
   arguments possibly contain variables which will be recorded in [h]. *)
let rec split_args parse_string ml args all = match all with
  | [] -> args
  | (arg, v) :: tl ->
      if is_ocaml_arg arg then (
        begin
          let a = String.sub arg 3 (String.length arg - 3) in
          if a = "content" then
            match split_on_spaces v with
            | v :: args when valid_ocaml_id v ->
                ml.content <- v;  ml.args <- args
            | _ -> failwith(sprintf "The variable name %S is not valid" v)
          else if a = "strip" then
            let v = strip_spaces v in
            ml.strip <- (if v = "ifempty" || v = "if empty" then `If_empty
                        else `Yes)
          else if a = "replace" then
            match split_on_spaces v with
            | v :: args when valid_ocaml_id v ->
                ml.content <- v;  ml.args <- args;  ml.strip <- `Yes
            | _ -> failwith(sprintf "The variable name %S is not valid" v)
        end;
        split_args parse_string ml args tl
      )
      else split_args parse_string ml ((arg, parse_string v) :: args) tl

let rec parse_element h html = match html with
  | Nethtml.Data(s) -> Data(parse_string h s)
  | Nethtml.Element(el, args, content) ->
      let ml = { content = "";  args = [];  strip = `No } in
      let args = split_args (parse_string h) ml [] args in
      if ml.content <> "" then (
        Var.add h ml.content (if ml.args = [] then Var.HTML else Var.Fun_html);
        Content(el, args, ml.strip, ml.content, ml.args)
      )
      else
        Element(el, args, parse_html h content)

and parse_html h html = List.map (parse_element h) html


(* Output to a static module
 ***********************************************************************)

let write_string_or_var fh s = match s with
  | String s -> fprintf fh "%S" s
  | Var v -> fprintf fh "eval t t.%s" v
  | Fun(f, args) ->
      fprintf fh "eval t t.%s " f;
      List.iter (fun v -> fprintf fh "%S " v) args

let write_subst_string fh s = match s with
  | [] -> ()
  | [s] -> write_string_or_var fh s
  | [s1; s2] ->
      write_string_or_var fh s1; fprintf fh "@ ^ ";
      write_string_or_var fh s2
  | [s1; s2; s3] ->
      write_string_or_var fh s1; fprintf fh "@ ^ ";
      write_string_or_var fh s2; fprintf fh "@ ^ ";
      write_string_or_var fh s3
  | _ ->
      fprintf fh "@[String.concat \"\" [";
      List.iter (fun s -> write_string_or_var fh s; fprintf fh ";@ ") s;
      fprintf fh "]@]@,"

let write_args fh args =
  fprintf fh "@[<1>[";
  List.iter (fun (n,v) ->
               fprintf fh "(%S, " n;
               write_subst_string fh v;
               fprintf fh ");@ "
            ) args;
  fprintf fh "]@]"

let rec write_rendering_fun fm h tpl =
  fprintf fm "@[<2>let render t =@\n";
  write_rendering_list fm h tpl;
  fprintf fm "@]@\n"
and write_rendering_list fm h tpl =
  fprintf fm "@[<1>[";
  List.iter (fun tpl -> write_rendering_node fm h tpl) tpl;
  fprintf fm "]@]@,"
and write_rendering_node fm h tpl = match tpl with
  | Data s ->
      fprintf fm "Nethtml.Data(";
      write_subst_string fm s;
      fprintf fm ");@ ";
  | Element(el, args, content) ->
      fprintf fm "@[<2>Nethtml.Element(%S,@ " el;
      write_args fm args;
      fprintf fm ",@ ";
      write_rendering_list fm h content;
      fprintf fm ");@]@ "
  | Content(el, args, strip, var, fun_args) ->
      (* We are writing a list.  If this must be removed, concatenate
         with left and right lists.  FIXME: this is not ideal and
         maybe one must move away from Nethtml representation? *)
      match strip with
      | `No ->
          fprintf fm "@[<2>Nethtml.Element(%S,@ " el;
          write_args fm args;
          fprintf fm ",@ ";
          Var.write_code_html fm h var fun_args;
          fprintf fm ");@]@ "
      | `Yes ->
          fprintf fm "]@ @@ ";
          Var.write_code_html fm h var fun_args;
          fprintf fm "@ @@ ["
      | `If_empty ->
          fprintf fm "]@ @@ @[<1>(";
          let bound_var = Var.write_binding fm h var fun_args in
          fprintf fm "if %s = " bound_var;
          Var.write_code_empty fm h var;
          fprintf fm " then []@ else @[<2>[Nethtml.Element(%S,@ " el;
          write_args fm args;
          fprintf fm ",@ %s)]@])@]@ @@ [" bound_var
;;

let read_html fname =
  let fh = open_in fname in
  let tpl = (Nethtml.parse_document (Lexing.from_channel fh)
               ~dtd:Nethtml.relaxed_html40_dtd) in
  close_in fh;
  tpl

let compile_html ?trailer_ml ?trailer_mli ?(hide=[]) ?module_name fname =
  let module_name = match module_name with
    | None -> (try Filename.chop_extension fname with _ -> fname)
    | Some n -> n (* FIXME: check valid module name *) in
  let tpl = read_html fname in
  (* Parse *)
  let h = Var.make() in
  let tpl = parse_html h tpl in
  (* Output implementation *)
  let fh = open_out (module_name ^ ".ml") in
  let fm = formatter_of_out_channel fh in
  fprintf fm "(* Module generated from the template %s. *)\n@\n" fname;
  fprintf fm "type html = Nethtml.document list\n\n";
  fprintf fm "type t = {\n";
  Var.iter h (fun v t ->
                fprintf fm "  %s: %s delay;\n" v (Var.type_code t.Var.ty);
             );
  fprintf fm "}\nand 'a delay = Val of 'a | Delay of (t -> 'a);;\n\n";
  fprintf fm "let eval t v = match v with Val a -> a | Delay f -> f t\n";
  (* See [Var.type_to_string] for the names: *)
  fprintf fm "let default_html = Val []\n";
  fprintf fm "let default_string = Val \"\"\n";
  fprintf fm "let default_fun_html = (fun _ -> Val [])\n";
  fprintf fm "let default_fun = Val(fun _ -> \"\")\n";
  fprintf fm "let empty = {\n";
  Var.iter h begin fun v t ->
    fprintf fm "  %s = default_%s;\n" v (Var.type_to_string t.Var.ty);
  end;
  fprintf fm "}\n\n";
  Var.iter h (fun v _ ->
                fprintf fm "let %s t v = { t with %s = Val v }\n" v v
             );
  (* Submodule to access the values independently of the
     representation of a template.  Use an abstract type to force the
     use of the [Set] module to be able to access the values through
     [Get] functions. The coercing submodule is named [Variable] to
     have readable error messages. *)
  fprintf fm "\n@[<2>module Variable : sig@\ntype get@\n\
              @[<2>module Get : sig@\n";
  Var.iter h begin fun v t ->
    fprintf fm "val %s : get -> %s@\n" v (Var.type_code t.Var.ty)
  end;
  fprintf fm "end@]@\n@[<2>module Set : sig@\n";
  Var.iter h begin fun v t ->
    fprintf fm "val %s : t -> (get -> %s) -> t@\n" v (Var.type_code t.Var.ty)
  end;
  fprintf fm "end@]@\nend = struct@\ntype get = t@\n";
  fprintf fm "@[<2>module Get = struct@\n";
  Var.iter h (fun v _ -> fprintf fm "let %s t = eval t t.%s@\n" v v);
  fprintf fm "end@]@\n@[<2>module Set = struct@\n";
  Var.iter h (fun v _ ->
                fprintf fm "let %s t f = { t with %s = Delay f }@\n" v v
             );
  fprintf fm "end@]@\nend@]@\nopen Variable@\n@\n";
  write_rendering_fun fm h tpl;
  begin match trailer_ml with
  | None -> ()
  | Some txt ->
      fprintf fm "(* ---------- Trailer -------------------- *)@\n%s" txt
  end;
  fprintf fm "@?"; (* flush *)
  close_out fh;
  (* Output interface *)
  let fh = open_out (module_name ^ ".mli") in
  let fm = formatter_of_out_channel fh in
  fprintf fm "(* Module interface generated from the template %s. *)\n\n" fname;
  fprintf fm "type html = Nethtml.document list\n\n";
  fprintf fm "type t\n  (** Immutable template. *)\n\n";
  fprintf fm "val empty : t\n";
  fprintf fm "  (** Empty (unfilled) template. *)\n";
  fprintf fm "val render : t -> html@\n";
  fprintf fm "  (** Renders the template as an HTML document. *)\n\n";
  Var.iter_ab h begin fun v t ->
    if not(List.mem v hide) then
      fprintf fm "val %s : t -> %s -> t\n" v (Var.type_code t.Var.ty)
  end;
  begin match trailer_mli with
  | None -> ()
  | Some txt -> fprintf fm "\n\n%s" txt
  end;
  fprintf fm "@?"; (* flush *)
  close_out fh


(* Compile an HTML file, possibly with some extra code in .html.ml
 *************************************************************************)

let content_of_file file =
  let fh = open_in file in
  let buf = Buffer.create 500 in
  try
    while true do
      Buffer.add_string buf (input_line fh);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    close_in fh;
    Buffer.contents buf

(* Return the content of [file] if it exists or [None] otherwise. *)
let maybe_content file =
  if Sys.file_exists file then Some(content_of_file file) else None

(* Looks for variable names to hide in the mli, declared with "@hide
   var" or "@replace var". *)
let hide_re = Str.regexp "(\\* *@hide +\\([a-zA-Z_]+\\) *\\*) *\n?"
let vars_to_hide mli =
  match mli with
  | None -> [], mli
  | Some mli ->
      let i = ref 0 in
      let acc = ref [] in
      try
        while true do
          i := Str.search_forward hide_re mli !i;
          acc := Str.matched_group 1 mli :: !acc;
          incr i;
        done;
        assert false
      with Not_found ->
        !acc, Some(Str.global_replace hide_re "" mli)

let compile f =
  let trailer_ml = maybe_content (f ^ ".ml") in
  let trailer_mli = maybe_content (f ^ ".mli") in
  let hide, trailer_mli = vars_to_hide trailer_mli in
  compile_html ?trailer_ml ?trailer_mli ~hide f


(* Parsing with direct substitution
 ***********************************************************************)

module Binding =
struct
  type data =
    | Html of html
    | String of string
    | Fun_html of (string list -> html)
    | Fun of (string list -> string)

  type t = (string, data) Hashtbl.t

  let make () = Hashtbl.create 20
  let copy = Hashtbl.copy

  let string b var s = Hashtbl.add b var (String s)
  let html b var h = Hashtbl.add b var (Html h)
  let fun_html b var f = Hashtbl.add b var (Fun_html f)
  let fun_string b var f = Hashtbl.add b var (Fun f)

  exception Std_Not_found = Not_found
  exception Not_found of string

  let find b var =
    try Hashtbl.find b var
    with Std_Not_found -> raise(Not_found var)

  let fail_not_a_fun var =
    invalid_arg(sprintf "%S is bound to a variable but used \
		as a function in the HTML template" var)

  let subst_to_string b var args =
    match find b var with
    | String s -> (match args with [] -> s | _ -> fail_not_a_fun var)
    | Fun f -> f args
    | Html _ | Fun_html _ -> invalid_arg(sprintf "A string is expected but \
		 %S returns HTML" var)

  let subst_to_html b var args =
    match find b var with
    | String s -> (match args with
                  | [] -> [Nethtml.Data s]
                  | _ -> fail_not_a_fun var)
    | Html h -> h
    | Fun_html f -> f args
    | Fun f -> [Nethtml.Data(f args)]
end

let subst_var b v =
  match split_on_spaces v with
  | [] | "" :: _ -> invalid_arg "Empty variables are not allowed"
  | v :: args ->
      if valid_ocaml_id v then Binding.subst_to_string b v args
      else invalid_arg(sprintf "Function name %S is not valid" v)

let subst_string bindings s =
  let buf = Buffer.create 100 in
  let add_string _ s = Buffer.add_string buf s in
  let add_var _ v = Buffer.add_string buf (subst_var bindings v) in
  parse_string_range add_string add_var () s 0 0 (String.length s);
  Buffer.contents buf

let subst_html bindings s =
  let add_string l s = Nethtml.Data s :: l in
  let add_var l v = Nethtml.Data(subst_var bindings v) :: l in
  List.rev(parse_string_range add_string add_var [] s 0 0 (String.length s))

let rec subst_element bindings html = match html with
  | Nethtml.Data s -> subst_html bindings s
  | Nethtml.Element(el, args, content0) ->
      let ml = { content = "";  args = [];  strip = `No } in
      let args = split_args (subst_string bindings) ml [] args in
      if ml.content <> "" then
        let content = Binding.subst_to_html bindings ml.content ml.args in
        match ml.strip with
        | `No -> [Nethtml.Element(el, args, content)]
        | `Yes -> content
        | `If_empty -> (if content = [] then []
                       else [Nethtml.Element(el, args, content)])
      else [Nethtml.Element(el, args, subst bindings content0)]

and subst bindings html =
  List.concat(List.map (subst_element bindings) html)


let read ?bindings fname =
  let html = read_html fname in
  match bindings with
  | None -> html
  | Some b -> subst b html


(* Utilities
 ***********************************************************************)

let write_html ?(doctype=true) html fname =
  let oc = new Netchannels.output_channel (open_out fname) in
  if doctype then
    oc#output_string
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">";
  Nethtml.write oc html ~dtd:Nethtml.html40_dtd;
  oc#close_out()


(* [body_of doc] returns the content of the <body> (if any) of [doc]. *)
let rec get_body_of_element = function
  | Nethtml.Data _ -> []
  | Nethtml.Element("body", args, content) -> content
  | Nethtml.Element(_, _, content) -> get_body_of content
and get_body_of content = List.concat (List.map get_body_of_element content)

let body_of html =
  let body = get_body_of html in
  if body = [] then html else body

module Path =
struct
  type t = {
    base : string;
    from_base : string;
    rev_from_base : string list;
    to_base : string;
    filename : string;
  }

  let make base =
    let len = String.length base in
    let base =
      if len > 0 && base.[len - 1] = '/' then String.sub base 0 (len - 1)
      else base in
    { base = base;
      from_base = "";  rev_from_base = [];  to_base = "";
      filename = "" }

  let filename p = p.filename
  let from_base p = p.from_base
  let from_base_split p = List.rev (filename p :: p.rev_from_base)
  let to_base p = p.to_base

  let to_base_split p =
    (* Beware that the split version of "../" is [".."; ""] (but the
       split of ".." is [".."]). *)
    if p.rev_from_base = [] then []
    else List.fold_left (fun p _ -> ".." :: p) [""] p.rev_from_base

  (* Use "/" to separate components because they are supported on
     windows and are mandatory for HTML paths *)
  let concat dir file =
    if dir = "" then file else dir ^ "/" ^ file

  let full_path p = concat p.base (from_base p)

  let full p =
    let f = filename p in
    if f = "" then full_path p else concat (full_path p) f

  (*
   * Titles for navigation.
   *)

  let rec concat_data c =
    String.concat "" (List.map data c)
  and data = function
    | Nethtml.Data s -> s
    | Nethtml.Element(_, _, c) -> concat_data c

  (* Retrieve the <title> of HTML.  We use "^" to concatenate titles
     because we expect few of them (only one). *)
  let rec get_title acc html =
    List.fold_left get_title_el acc html
  and get_title_el acc e = match e with
    | Nethtml.Element("title", _, content) -> concat_data content ^ acc
    | Nethtml.Element(_, _, content) -> get_title acc content
    | Nethtml.Data _ -> acc

  (* Retrieve the <title> of fname (returns [""] if none is found). *)
  let title_of_file fname =
    try get_title "" (read_html fname)
    with Sys_error _ -> ""

  let lang_re = Str.regexp "\\([a-zA-Z_ ]+\\)\\(\\.\\([a-z]+\\)\\).html"
  let base_and_lang_of_filename f =
    if Str.string_match lang_re f 0 then
      Str.matched_group 1 f, Str.matched_group 2 f
    else (try Filename.chop_extension f with _ -> f), ""

  let language p =
    let f = filename p in
    if Str.string_match lang_re f 0 then Str.matched_group 3 f else ""

  (* Add a relative path from the directory pointed by [p] to each
     path component. *)
  let rec add_to_dir_loop ~base ~basep acc child p = match p with
    | [] -> (base, basep, child ^ "../") :: acc
    | d :: tl ->
        let to_d = child ^ "../" in
        add_to_dir_loop ~base ~basep ((d, d, to_d) :: acc) to_d tl

  let add_to_dir ~base ~basep p = match p with
    | [] -> [(base, basep, "./")]
    | final :: tl -> add_to_dir_loop ~base ~basep [(final, final, "./")] "./" tl

  (* Add a full path the each component of [p] to be able to see
     whether there is an index file in the component dir. *)
  let rec add_from_base_loop acc from_base p = match p with
    | [] -> acc
    | (name, d, to_d) :: subdirs ->
        let from_base = from_base ^ "/" ^ d in
        add_from_base_loop ((name, from_base, to_d) :: acc) from_base subdirs

  let add_from_base = function
    | [] -> []
    | ((_, basep, _) as base) :: tl -> add_from_base_loop [base] basep tl

  let navigation p ~base =
    if filename p = "" then invalid_arg "Template.Path.navigation: no filename";
    let fbase, lang = base_and_lang_of_filename (filename p) in
    (* Assume [p.base] does not end with "/". *)
    let nav = add_from_base(add_to_dir ~base ~basep:p.base p.rev_from_base) in
    let final_file =
      if fbase = "index" then []
      else (let title = title_of_file (full p) in
            [(if title = "" then String.capitalize fbase else title), ""]) in
    let index = "/index" ^ lang ^ ".html" in
    (* Look at index file, if any, to decide the name of the path components. *)
    List.fold_left begin fun acc (name, path, rev) ->
      let title = title_of_file (path ^ index) in
      ((if title = "" then String.capitalize name else title), rev) :: acc
    end final_file nav

  (*
   * Links for translations
   *)

  let translations p =
    []

  (*
   * Recursively browse dirs
   *)

  let concat_dir p dir =
    assert(filename p = ""); (* [p] is supposed to represent a dir *)
    { p with
        from_base = concat p.from_base dir;
        rev_from_base = dir :: p.rev_from_base;
        to_base = concat ".." p.to_base;
        (* FIXME: will end with "/", wanted?? certainly used! *)
    }

  let concat_file p fname =
    assert(filename p = ""); (* [p] is supposed to represent a dir *)
    { p with filename = fname; }

  let rec iter_files ~filter_dir ~filter_file p f =
    let full_path = full_path p in
    let files = Sys.readdir full_path in
    for i = 0 to Array.length files - 1 do
      let file = files.(i) in
      if file <> "" (* should not happen *) && file.[0] <> '.' (* hidden *)
      then begin
        if Sys.is_directory (concat full_path file) then
          let p = concat_dir p file in
          (if filter_dir p then iter_files ~filter_dir ~filter_file p f)
        else
          let p = concat_file p file in
          if filter_file p then f p
      end
    done
end

let rec mkdir_if_absent ?(perm=0o750) dir =
  (* default [perm]: group read => web server *)
  if not(Sys.file_exists dir) then begin
    mkdir_if_absent ~perm (Filename.dirname dir);
    Unix.mkdir dir perm
  end

let iter_html ?(default_lang="Fr") ?(filter=(fun _ -> true)) base f =
  if not(Sys.is_directory base) then invalid_arg "Template.iter_files";
  let output_dir = String.lowercase default_lang in
  let filter_dir p = Path.from_base p <> output_dir
  and filter_file p =
    Filename.check_suffix (Path.filename p) ".html" && filter p in
  mkdir_if_absent output_dir;
  Path.iter_files ~filter_file ~filter_dir (Path.make base) begin fun p ->
    let html = f p in
    let dir = Filename.concat output_dir (Path.from_base p) in
    mkdir_if_absent dir;
    write_html html (Filename.concat dir (Path.filename p))
  end


let quote_quot_re = Str.regexp_string "\"";;
let arg_to_string (a,v) =
  let v = Str.global_replace quote_quot_re "&quot;" v in
  a ^ "=\"" ^ v ^ "\""

(* See http://javascript.about.com/library/blnoscript.htm for ideas on
   how to get rid of <noscript>. *)
let email_id = ref 0
let email ?(args=[]) ?content e =
  let at = String.index e '@' in
  let local_part = String.sub e 0 at in
  let at = at + 1 in
  let host_query = String.sub e at (String.length e - at) in
  let host = (try String.sub host_query 0 (String.index host_query '?')
              with Not_found -> host_query) in
  let args = String.concat " " (List.map arg_to_string args) in
  incr email_id;
  let id = Printf.sprintf "ocaml_%i" !email_id in
  let javascript = Printf.sprintf
    "local = %S;\n\
     h = %S;\n\
     document.getElementById(%S).innerHTML = \
     '<a href=\"mailto:' + local + '@' + h + \"\\\" %s>%s<\\/a>\";"
    local_part host_query id args
    (match content with
     | None -> "\" + local + '@' + h + \""
     | Some c ->
        let buf = Buffer.create 100 in
        let ch = new Netchannels.output_buffer buf in
        Nethtml.write ch c;
        ch#close_out();
        String.escaped (Buffer.contents buf)) in
  let noscript = match content with
    | None -> [Nethtml.Data(local_part);
              Nethtml.Element("abbr", ["title", "(at) &rarr; @"],
                              [Nethtml.Data "(at)"]);
              Nethtml.Data host]
    | Some c -> c @ [Nethtml.Data " &#9001;";
                    Nethtml.Data(local_part);
                    Nethtml.Element("abbr", ["title", "(at) &rarr; @"],
                                    [Nethtml.Data "(at)"]);
                    Nethtml.Data host;
                    Nethtml.Data "&#9002;" ] in
  [Nethtml.Element("span", ["id", id], noscript);
   Nethtml.Element("script", ["type", "text/javascript"],
                   [Nethtml.Data("<!--;\n" ^ javascript ^ "\n//-->") ])]

let is_email (a, e) =
  a = "href"
  && String.length e > 7 && e.[0] = 'm' && e.[1] = 'a' && e.[2] = 'i'
  && e.[3] = 'l' && e.[4] = 't' && e.[5] = 'o' && e.[6] = ':'

(* Concatenate all Data in [l].  If another node is present, raise [Failure]. *)
let concat_content_data l =
  let l = List.map (function
                    | Nethtml.Data s -> s
                    | Nethtml.Element _ -> failwith "concat_content_data") l in
  String.concat "" l

(* Check whether the content of the link is the link mail address. *)
let content_is_email txt email =
  let len_txt = String.length txt in
  is_prefix txt email
  && (len_txt = String.length email || email.[len_txt] = '?')

let rec protect_emails html =
  List.concat(List.map protect_emails_element html)
and protect_emails_element = function
  | Nethtml.Data _ as e -> [e] (* emails in text are not modified *)
  | Nethtml.Element("a", args, content) as e ->
      let emails, args = List.partition is_email args in
      (match emails with
       | [] -> [e]
       | [(_, addr)] ->
           let addr = String.sub addr 7 (String.length addr - 7) in
           let content =
             try
               let txt = concat_content_data content in
               if content_is_email txt addr then None else Some content
             with Failure _ -> None in
           email ~args ?content addr
       | _ -> failwith("Several email addresses not allowed"
                      ^ String.concat ", " (List.map snd emails)))
  | Nethtml.Element(el, args, content) ->
      [Nethtml.Element(el, args, protect_emails content)]

let is_href (a, _) = a = "href"

let apply_relative_href base ((e, url) as arg) =
  let url = parse_url url ~base_syntax:ip_url_syntax in
  try (e, string_of_url(apply_relative_url base url))
  with Malformed_URL -> arg

let rec apply_relative_url base html =
  List.map (apply_relative_url_element base) html
and apply_relative_url_element base = function
  | Nethtml.Element("a", args, content) ->
      let href, args = List.partition is_href args in
      let href = List.map (apply_relative_href base) href in
      Nethtml.Element("a", href @ args, content)
  | Nethtml.Element(e, args, content) ->
      Nethtml.Element(e, args, apply_relative_url base content)
  | Nethtml.Data _ as e -> e

let relative_url_are_from_base p html =
  let base = make_url ~path:(Path.to_base_split p) ip_url_syntax in
  apply_relative_url base html
