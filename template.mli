
(** Simple templating library. *)

type html = Nethtml.document list

val compile : ?module_name:string -> string -> unit
  (** [compile fname] reads the HTML template file [fname] and creates
      an OCaml module with functions to fill the variables of the
      template.  The module will be in files [module_name].ml and
      [module_name].mli.

      @param module_name the name of the generated module.  By
      default, it is the basename of [fname] without extension. *)

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


(** {1 Utilities} *)

val read_html : string -> html
  (** [read_html fname] reads the file [fname] and returns its content
      in a structured form. *)

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

val email : string -> html
  (** [email e] return some HTML/javascript code to protect the email
      [e] from SPAM harvesters. *)

val apply_relative_url : Neturl.url -> html -> html
  (** [apply_relative_url base html] prefix all relative URLs in
      [html] by [base]. *)
