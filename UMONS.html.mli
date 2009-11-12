(** Some utilities related to the UMONS template. *)

val toolbar : string -> t
  (** [toobar lang] returns a template with the links of the toolbar
      ("Annuaire",...) filled with the appropriate links according to
      the language [lang] (must be "fr" or "en" or any other
      abbreviation of the main university site). *)

(* @hide navigation_bar *)
val navigation_of_path : t -> string -> string -> t
  (** [navigation_of_path t rel_path fname] add a navigation bar based
      on the path [rel_path]/[fname]. *)

(* @hide stylesheet *)
val stylesheet : t -> ?rel_base:bool -> string -> t
  (** [stylesheet tpl url] add the CSS file given by [url] to the
      stylesheet declarations.  If [rel_base] is true (the default),
      interpret [url] as a path relative to the base URL (as set in
      [tpl]. *)

(* @hide web_counter *)
val bbclone : t -> string -> string -> t
  (** [bbclone tpl rel_path fname] adds the PHP bbclone code in order
      to track accesses to the page. *)

(* @hide languages *)
val languages : t -> (string * string) list -> t
  (** [languages tpl langs] return (a vopy of) the template [tpl] with
      the language links set according to [langs].  [langs] is a list
      of [(lang, url)] where [lang] is the name of the language and
      [url] is the URL where the page can be found ([""] if it is the
      current pagea). *)
