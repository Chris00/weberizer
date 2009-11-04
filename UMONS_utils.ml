(* Specific functions for the UMONS template. *)

open Nethtml

let separation ~url_base =
  Element("span", ["class", "separation"],
          [Element("img", ["src", url_base ^ "images/right_arrow.png"], [])])

(* Add a separation in front of each element (there is already a
   prefix in the navigation bar => 1st element also needs a sep. *)
let rec add_separation sep l = match l with
  | [] -> l
  | a :: tl -> sep :: a :: add_separation sep tl

let navigation_bar tpl ~url_base items =
  let sep = separation ~url_base in
  UMONS.navigation_bar tpl (add_separation sep items)
