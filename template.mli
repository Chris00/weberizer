
(** Simple templating library. *)

type html = Nethtml.document list

val compile_html :
  ?trailer_ml:string -> ?trailer_mli:string -> ?hide:string list ->
  ?module_name:string -> string -> unit
  (** [compile_html fname] reads the HTML template file [fname] and
      creates an OCaml module with functions to fill the variables of
      the template.  The module will be in files [module_name].ml and
      [module_name].mli.

      @param module_name the name of the generated module.  By
      default, it is the basename of [fname] without extension.

      @param trailer_ml additional code to be appended to the .ml
      file.  This code can use the functions of the interface to set
      variables of the templates.  To access the value of a variable,
      say "var", stored in the template [t], use [Get.var t].  If you
      create a function [f] that set a value depending on other ones,
      you should use [Set.var t (lazy expr)], where [expr] contains
      the [Get.var] functions.  This way, the values of the accessed
      variables will be the last set ones (as opposed to the ones
      present in the template when [f] is issued which would require
      to document the hidden dependencies properly).  This is
      important to be able to set the template value in any order.

      @param trailer_mli additional code to be appended to the .mli file.
      @param hide variables of the template that will not be present
      in the module interface.  This is only interesting if these
      variables are used in [trailer_ml] functions. *)

val compile : string -> unit
  (** [compile_file fname] does the same as [compile_html] except that
      trailer code is taken from [fname].ml and [fname].mli for the
      implementation and interface respectively.  Moreover, to hide
      the signature of a template variable, say "var", one must add a
      comment [(* @hide var *)] in [fname].mli *)


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

val write_html : html -> string -> unit
  (** [write_html html fname] writes the textual representation of the
      [html] to the file [fname]. *)

val body_of : html -> html
  (** [body_of html] returns the body of the HTML document or the
      entire document if no body is found. *)

val iter_files : ?filter:(string -> string -> bool) ->
  string -> (string -> string -> unit) -> unit
  (** [iter_files root f] iterates [f rel_dir fname] on all files
      under [root] (that is [root] itself if it is a file and all
      files in [root] and its subdirectories if [root] is a directory)
      where [fname] is the base name of the file and [rel_path] its
      relative path to [root].

      @param filter examine the file of dir iff the condition [filter
      rel_dir f] holds on the relative path [rel_dir] from [root] and
      final file or dir [f].  Default: accept all [.html] and [.php]
      files.  Files and dirs starting with a dot are {i always}
      excluded. *)

val revert_path : string -> string
  (** [revert_path p] returns a relative path [q] so that cd [p]
      followed by cd [q] is equivalent to staying in the current
      directory (assuming [p] exists). *)

val email : ?args:(string * string) list -> ?content:html -> string -> html
  (** [email e] return some HTML/javascript code to protect the email
      [e] from SPAM harvesters.  The email [e] may end with "?..." in
      order to specify options, e.g. [?subject=...].

      @param args arguments to the <a> HTML tag.  Default: [[]].

      @param content Tells whether the content of the email link is
      the email itself (no [content] specified, the default) or some
      other data. *)

val apply_relative_url : Neturl.url -> html -> html
  (** [apply_relative_url base html] prefix all relative URLs in
      [html] by [base]. *)

val protect_emails : html -> html
  (** [protect_emails html] changes all emails hrefs in [html] in
      order to make it more difficult for spammers to harvest them. *)
