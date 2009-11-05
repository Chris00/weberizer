(** Some utilities related to the UMONS template. *)

(* @hide navigation_bar *)
val navigation_bar : t -> url_base:string -> Nethtml.document list -> t
  (** [navigation_bar t ~url_base items] construct the navigation bar
      from the list of [items]. *)
