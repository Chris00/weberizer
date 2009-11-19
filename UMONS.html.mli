(** Some utilities related to the UMONS template. *)

(* @hide toolbar *)
val toolbar : t -> Template.Path.t -> string -> t
  (** [toobar tpl path contact_link] returns a template with the links
      of the toolbar ("Annuaire",...) appropriately filled according
      to the language of the file pointed by the [path].
      [contact_link] is the link to the "contact" page.  The base URL
      is also set from [path]. *)

(* @hide navigation_bar *)
val navigation_of_path : t -> Template.Path.t -> t
  (** [navigation_of_path t path] add a navigation bar based on the
      [path]. *)

(* @hide stylesheet *)
val stylesheet : t -> ?rel_base:bool -> string -> t
  (** [stylesheet tpl url] add the CSS file given by [url] to the
      stylesheet declarations.  If [rel_base] is true (the default),
      interpret [url] as a path relative to the base URL (as set in
      [tpl]. *)

(* @hide web_counter *)
val bbclone : t -> Template.Path.t -> t
  (** [bbclone tpl rel_path fname] adds the PHP bbclone code in order
      to track accesses to the page. *)

(* @hide languages *)
val languages : t -> (string * string) list -> t
  (** [languages tpl langs] return (a vopy of) the template [tpl] with
      the language links set according to [langs].  [langs] is a list
      of [(lang, url)] where [lang] is the name of the language and
      [url] is the URL where the page can be found ([""] if it is the
      current pagea). *)
