
(** Simple templating library. *)

type html = Nethtml.document list

val compile_html :
  ?trailer_ml:string -> ?trailer_mli:string -> ?hide:string list ->
  ?module_name:string -> string -> unit
  (** [compile_html fname] reads the HTML template file [fname]
      (typically a file with extension ".html") and creates an OCaml
      module with functions to fill the variables of the template.
      The module will be in files [module_name].ml and
      [module_name].mli.

      @param module_name the name of the generated module.  By
      default, it is the basename of [fname] without extension.

      @param trailer_ml additional code to be appended to the .ml
      file.  This code can use the functions of the interface to set
      variables of the templates.  You set a variable [v] using the
      value of a variable [v'], you should use the construction [Set.v
      tpl (fun t -> ... Get.v' t ...)] (which returns a copy of [tpl]
      with [v] set) to ensure that the value of [v'] at the time o
      rendering is used and not the one present in [tpl] when [v] is
      set.  This is important to maintain the independence of
      variables which may be set in any order (documenting that a
      variable depends on others will lead to confusion and errors).
      You should of course take care not to create dependency loops.

      @param trailer_mli additional code to be appended to the .mli file.
      @param hide variables of the template that will not be present
      in the module interface.  This is only interesting if these
      variables are used in [trailer_ml] functions. *)

val compile : string -> unit
  (** [compile fname] does the same as [compile_html] except that
      trailer code is taken from [fname].ml and [fname].mli for the
      implementation and interface respectively.  Moreover, to hide
      the signature of a template variable, say "var", one can add a
      comment [(* @hide var *)] in [fname].mli.  Special annotations
      are added to the generated module implementation and interface
      so errors point back to [fname].ml and [fname].mli respectively. *)


module Binding :
sig
  type t
    (** Mutable value holding a correspondence of a variable name to
        its value. *)

  exception Not_found of string
    (** [Not_found var] is raised if the variable [var] is not found
        in the binding. *)

  val make : unit -> t
  val copy : t -> t

  val string : t -> string -> string -> unit
    (** [string b var s] add to the binding [var] -> [s] to [b]. *)
  val html : t -> string -> html -> unit
    (** [html b var h] add to the binding [var] -> [h] to [b]. *)
  val fun_html : t -> string -> (string list -> html) -> unit
    (** [fun_html b var f] add to the binding [var] -> [f] to [b]. *)
  val fun_string : t -> string -> (string list -> string) -> unit
    (** [fun_string b var f] add to the binding [var] -> [f] to [b]. *)
end

val subst : Binding.t -> html -> html
  (** [subst b html] return [html] where all variables are substituted
      according to the bindings [b]

      @raise Invalid_argument if variable names are not valid or
      associated values do not correspond to their usage. *)

val read : ?bindings:Binding.t -> string -> html
  (** [read fname] reads the file [fname] and returns its content in a
      structured form.

      @param bindings if provided, perform the substitutions it
      mandates.  Otherwise, the "raw" HTML is returned (this is the
      default). *)


(** {1 Utilities} *)

val write_html : ?doctype:bool -> html -> string -> unit
  (** [write_html html fname] writes the textual representation of the
      [html] to the file [fname].

      @param doctype whether to output a doctype (default: [true]). *)

val body_of : html -> html
  (** [body_of html] returns the body of the HTML document or the
      entire document if no body is found. *)

module Path :
sig
  type t
    (** Path relative to a base directory. *)

  val from_base : t -> string
    (** The (normalized) path to the filename (the filename being
        excluded) relative to the base directory.  Returns [""] if we
        are in the base directory. *)

  val from_base_split : t -> string list
    (** The path to the filename (including it) relative to the base
        directory splitted into its components (see [Neturl] for the
        precise format). *)

  val filename : t -> string
    (** The filename the path points to.  The path designates a
        directory if and only if[filename] returns [""]. *)

  val to_base : t -> string
    (** The path from the directory of the filename to the base
        directory.  One can see it as the "inverse" of [from_base]. *)

  val to_base_split : t -> string list
    (** The path from the directory of the filename to the base
        directory.  One can see it as the "inverse" of [from_base_split]. *)

  val full : t -> string
    (** Returns a path that can be used to open the file (or query the
        directory). *)

  val language : t -> string
    (** [language p] returns the language of the file pointed by [p]
        or [""] if none is present (default language ir drectory).
        The filename is expected to be of the type [name.<lang>.html]. *)

  val description : t -> string
    (** [description p] returns the descriptive name for the file
        pointed by [p]. *)

  val navigation : t -> (string * string) list
    (** [navigation p] returns the navigation information for the path
        [p].  It consists of a list of pairs [(name, path)] where
        [name] is a descriptive name of that directory of the path and
        [path] is the relative link to go from the location pointed by
        [p] to the directory.  If [filename p] is of the form
        index.*.html, then only its directory is included in the
        navigation information.

        Descriptive names are based on the name of the directory or,
        if an index.<lang>.html file is present it is taken as its
        title (if any).  <lang> is determined according to the file
        pointed by [p] (if of the form name.<lang>.html). *)
end

val iter_html : ?default_lang:string -> ?filter:(Path.t -> bool) ->
  string -> (Path.t -> html) -> unit
  (** [iter_html base f] iterates [f file] on all HTML files under
      [base] (the argument of [f] is guaranteed to be a path to a
      file).  @raise Invalid_argument if [base] is not a directory.

      @param filter examine the file of dir iff the condition [filter
      rel_dir f] holds on the relative path [rel_dir] from [root] and
      final file or dir [f].  Default: accept all [.html] and [.php]
      files.  Files and dirs starting with a dot are {i always}
      excluded. *)

val relative_url_are_from_base : Path.t -> html -> html
  (** [relative_url_are_from_base path html] prefix all relative URLs
      in [html] so that they are specified according to the directory
      given by [path] instead of the base path. *)

val email : ?args:(string * string) list -> ?content:html -> string -> html
  (** [email e] return some HTML/javascript code to protect the email
      [e] from SPAM harvesters.  The email [e] may end with "?..." in
      order to specify options, e.g. [?subject=...].

      @param args arguments to the <a> HTML tag.  Default: [[]].

      @param content Tells whether the content of the email link is
      the email itself (no [content] specified, the default) or some
      other data. *)

val protect_emails : html -> html
  (** [protect_emails html] changes all emails hrefs in [html] in
      order to make it more difficult for spammers to harvest them. *)
