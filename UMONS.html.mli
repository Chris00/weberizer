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
