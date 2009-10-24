
(** Simple templating library. *)

type html = Nethtml.document list

val compile : ?module_name:string -> string -> unit
  (** [compile fname] reads the HTML template file [fname] and creates
      an OCaml module with functions to fill the variables of the
      template.  The module will be in files [module_name].ml and
      [module_name].mli.

      @param module_name the name of the generated module.  By
      default, it is the basename of [fname] without extension. *)


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
  string -> (string -> unit) -> unit
  (** [iter_files root f] iterates [f fname] on all files under [root]
      (that is [root] itself if it is a file and all files in [root]
      and its subdirectories if [root] is a directory) where [fname]
      is the name of the file relative to [root].

      @param filter examine the file of dir iff the condition [filter
      rel_dir f] holds on the relative path [rel_dir] from [root] and
      final file or dir [f].  Default: accept all [.html] and [.php]
      files.  Files and dirs starting with a dot are {i always}
      excluded. *)
