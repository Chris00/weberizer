(** Some utilities related to the UMONS template. *)

val navigation_bar : UMONS.t -> url_base:string ->
  Nethtml.document list -> UMONS.t
  (** [navigation_bar t ~url_base items] construct the navigation bar
      from the list of [items]. *)
