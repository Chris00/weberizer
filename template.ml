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
let rec split_on_spaces s = get_split_string s 0 0 (String.length s)
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


(* Parse strings
 *************************************************************************)

module Var =
struct
  (* Type of variables in HTML templates. *)
  type ty = HTML | String | Fun

  let type_to_string = function
    | HTML -> "html"
    | String -> "string"
    | Fun -> "string list -> string"

  (* Characteristics of a variable *)
  type t = { mutable ty: ty;             (* var type *)
           }

  (* name -> t *)
  type set = (string, t) Hashtbl.t

  let make () = (Hashtbl.create 10 : set)
  let ty h v = (Hashtbl.find h v).ty

  let to_html h v = match ty h v with
    | HTML -> "t." ^ v
    | String -> "[Nethtml.Data(t." ^ v ^ ")]"
    | Fun -> assert false (* funs are only in strings *)

  let is_empty_code h v = match ty h v with
    | HTML -> "t." ^ v ^ " = []"
    | String -> "t." ^ v ^ " = \"\""
    | Fun -> assert false (* funs are only in strings *)

  (* Add a new variable.  In case of conflicting types, use the
     "lower" type compatible with both. *)
  let add (h:set) v ty =
    try
      let v' = Hashtbl.find h v in
      match v'.ty, ty with
      | (HTML | String), Fun | Fun, (HTML | String) ->
          failwith(sprintf "The identifier %S cannot be used both as a \
		variable and a function" v)
      | HTML, String | String, HTML -> v'.ty <- String
      | HTML, HTML | String, String | Fun, Fun -> ()
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

let decode_var h v =
  match split_on_spaces v with
  | [] | "" :: _ -> invalid_arg "Empty variables are not allowed"
  | [v] ->
      if valid_ocaml_id v then (Var.add h v Var.String; Var v)
      else invalid_arg(sprintf "Variable %S is not a valid OCaml identifier" v)
  | v :: args ->
      if valid_ocaml_id v then (Var.add h v Var.Fun; Fun(v, args))
      else invalid_arg(sprintf "Function name %S is not valid" v)

let rec parse_string_range h s i0 i len_s : subst_string =
  if i >= len_s then
    let len = i - i0 in
    if len = 0 then [] else [String(String.sub s i0 len)]
  else if i + 1 < len_s && s.[i] = '$' && s.[i+1] = '{' then
    let len = i - i0 in
    if len = 0 then parse_var h s (i+2) (i+2) len_s
    else String(String.sub s i0 len) :: parse_var h s (i+2) (i+2) len_s
  else
    parse_string_range h s i0 (i+1) len_s
and parse_var h s i0 i len_s =
  if i >= len_s then
    invalid_arg(sprintf "Missing '}' to close the variable %S"
                (String.sub s i0 (len_s - i0)))
  else if s.[i] = '}' then
    decode_var h (String.sub s i0 (i - i0))
    :: parse_string_range h s (i+1) (i+1) len_s
  else parse_var h s i0 (i+1) len_s

let parse_string h s = parse_string_range h s 0 0 (String.length s)


(* Parse Nethtml document : search variables
 ***********************************************************************)

type strip = [ `No | `Yes | `If_empty ]

type document =
  | Element of string * (string * subst_string) list * document list
  | Data of subst_string
  | Content of string * (string * subst_string) list * strip * string
      (* Content(el, args, strip default, var) : content replacement *)

(* Accumulator keeping given OCaml arguments *)
type ocaml_args = {
  mutable content: string; (* var name or "" *)
  mutable strip: strip;
}

let is_ocaml_arg s =
  String.length s > 3 && s.[0] = 'm' && s.[1] = 'l' && s.[2] = ':'

(* [split_args h ml [] all] go through the arguments [all], record the
   "ml:*" arguments in [ml] and returns the other arguments.  These
   arguments possibly contain variables which will be recorded in [h]. *)
let rec split_args h ml args all = match all with
  | [] -> args
  | (arg, v) :: tl ->
      if is_ocaml_arg arg then (
        begin
          let a = String.sub arg 3 (String.length arg - 3) in
          if a = "content" then
            if valid_ocaml_id v then ml.content <- v
            else failwith(sprintf "The variable name %S is not valid" v)
          else if a = "strip" then
            let v = strip_spaces v in
            ml.strip <- (if v = "ifempty" || v = "if empty" then `If_empty
                        else `Yes)
          else if a = "replace" then
            if valid_ocaml_id v then (ml.content <- v; ml.strip <- `Yes)
            else failwith(sprintf "The variable name %S is not valid" v)
        end;
        split_args h ml args tl
      )
      else split_args h ml ((arg, parse_string h v) :: args) tl

let rec parse_element h html = match html with
  | Nethtml.Data(s) -> Data(parse_string h s)
  | Nethtml.Element(el, args, content) ->
      let ml = { content = "";  strip = `No } in
      let args = split_args h ml [] args in
      if ml.content <> "" then (
        Var.add h ml.content Var.HTML;
        Content(el, args, ml.strip, ml.content)
      )
      else
        Element(el, args, parse h content)

and parse h html = List.map (parse_element h) html


(* Output to a static module
 ***********************************************************************)

let write_string_or_var fh s = match s with
  | String s -> fprintf fh "%S" s
  | Var v -> fprintf fh "t.%s" v
  | Fun(f, args) ->
      fprintf fh "t.%s " f;
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
  fprintf fm "@]\n"
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
  | Content(el, args, strip, var) ->
      (* We are writing a list.  If this must be removed, concatenate
         with left and right lists.  FIXME: this is not ideal and
         maybe one must move away from Nethtml representation? *)
      match strip with
      | `No ->
          fprintf fm "@[<2>Nethtml.Element(%S,@ " el;
          write_args fm args;
          fprintf fm ",@ %s);@]@ " (Var.to_html h var)
      | `Yes ->
          fprintf fm "]@ @@ %s@ @@ [" (Var.to_html h var)
      | `If_empty ->
          fprintf fm "]@ @@ @[<1>(if %s then []@ \
	    else @[<2>[Nethtml.Element(%S,@ " (Var.is_empty_code h var) el;
          write_args fm args;
          fprintf fm ",@ %s)]@])@]@ @@ [" (Var.to_html h var)
;;

let read_html fname =
  let fh = open_in fname in
  let tpl = (Nethtml.parse_document (Lexing.from_channel fh)
               ~dtd:Nethtml.relaxed_html40_dtd) in
  close_in fh;
  tpl

let compile ?module_name fname =
  let module_name = match module_name with
    | None -> (try Filename.chop_extension fname with _ -> fname)
    | Some n -> n (* FIXME: check valid module name *) in
  let tpl = read_html fname in
  (* Parse *)
  let h = Var.make() in
  let tpl = parse h tpl in
  (* Output implementation *)
  let fh = open_out (module_name ^ ".ml") in
  let fm = formatter_of_out_channel fh in
  fprintf fm "(* Module generated from the template %s. *)\n@\n" fname;
  fprintf fm "type html = Nethtml.document list\n\n";
  fprintf fm "type t = {\n";
  Var.iter h (fun v t ->
                fprintf fm "  %s: %s;\n" v (Var.type_to_string t.Var.ty);
             );
  fprintf fm "}\n\n";
  fprintf fm "let default_html = []\n";
  fprintf fm "let default_string = \"\"\n";
  fprintf fm "let default_fun _ = ()\n";
  fprintf fm "let empty = {\n";
  Var.iter h begin fun v t ->
    fprintf fm "  %s = default_%s;\n" v (Var.type_to_string t.Var.ty);
  end;
  fprintf fm "}\n\n";
  Var.iter h (fun v _ ->
                fprintf fm "let %s t v = { t with %s = v }\n" v v
             );
  write_rendering_fun fm h tpl;
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
    fprintf fm "val %s : t -> %s -> t\n" v (Var.type_to_string t.Var.ty)
  end;
  fprintf fm "@?"; (* flush *)
  close_out fh


(* Utilities
 ***********************************************************************)

let write_html html fname =
  let oc = new Netchannels.output_channel (open_out fname) in
  Nethtml.write oc html;
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

let rec iter_files_in_dir filter root rel_dir f =
  let dir = Filename.concat root rel_dir in
  let files = Sys.readdir dir in
  for i = 0 to Array.length files - 1 do
    let file = files.(i) in
    if file <> "" (* should not happen *) && file.[0] <> '.'
      && filter rel_dir file then begin
      if Sys.is_directory(Filename.concat dir file) then
        iter_files_in_dir filter root (Filename.concat rel_dir file) f
      else f rel_dir file
    end
  done

let filter_default rel_dir f =
  Filename.check_suffix f ".html" || Filename.check_suffix f ".php"

let iter_files ?(filter=filter_default) root f =
  if Sys.is_directory root then iter_files_in_dir filter root "." f
  else f (Filename.dirname root) (Filename.basename root)

let rec revert_path p =
  let dir = Filename.dirname p in
  if dir = "." then (if Filename.basename p = "." then "" else "../")
  else Filename.concat ".." (revert_path dir)

let email e =
  let at = String.index e '@' in
  let local_part = String.sub e 0 at in
  let at = at + 1 in
  let host = String.sub e at (String.length e - at) in
  [Nethtml.Element("script", ["type", "text/javascript"],
                   [Nethtml.Data(Printf.sprintf "<!--
                     local = %S\n\
                     document.write('<a href=\"mailto:' + local + '@%s\">' \
                     + local + '@%s</a>')\n\
                     //-->" local_part host host)
                   ]);
   Nethtml.Element("noscript", [],
                   [Nethtml.Data(local_part);
                    Nethtml.Element("abbr", ["title", "(at) -> @"],
                                    [Nethtml.Data "(at)"]);
                    Nethtml.Data host])
  ]


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

