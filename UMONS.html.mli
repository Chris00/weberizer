(** Some utilities related to the UMONS template. *)

(* @hide url_banner *)
val banner : t -> ?base:bool -> string -> t

(* @hide toolbar *)
val toolbar : t -> ?contact:string -> ?map:string -> ?base:string ->
  lang:string -> Template.Path.t -> t
(** [toobar tpl ~lang path] returns a template with the links
    of the toolbar ("Annuaire",...) appropriately filled according
    to the language of the file pointed by the [path].
    [contact_link] is the link to the "contact" page.  The base URL
    is also set from [path].
    It also sets a sensible default for the shortcut icon.

    @param base replace the base path extracted from [path].  *)

(* @hide navigation_bar *)
val navigation : t -> ?prefix:(string * string) list ->
  (string * string) list -> t
(** [navigation t nav] set the navigation bar from the information in
    [nav].  The couples of [nav] are of the form [(text, URL)]. *)

val navigation_of_path : t -> ?prefix:(string * string) list ->
  Template.Path.t -> t
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
(** [languages tpl langs] return (a copy of) the template [tpl] with
    the language links set according to [langs].  [langs] is a list
    of [(lang, url)] where [lang] is the name of the language and
    [url] is the (relative or absolute) URL where the page can be
    found ([""] if it is the current page). *)
