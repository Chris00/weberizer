(* Specific functions for the UMONS template. *)

open Printf
open Nethtml

let separation url_base =
  Element("span", ["class", "separation"],
          [Element("img", ["src", url_base ^ "images/right_arrow.png"], [])])

(* All paths start with "." which stands for the institute. *)
let rec transform_path institut sep p = match p with
  | [] -> []
  | a :: tl ->
      let a = if a = "." then institut else String.capitalize a in
      sep :: Data(a) :: transform_path institut sep tl

let navigation_of_path tpl rel_path fname =
  let institut = Get.title tpl in
  let sep = separation (Get.url_base tpl) in
  let p = Neturl.split_path rel_path in
  let p = (if fname = "index.html" || fname = "index.htm" then p
           else p @ [try Filename.chop_extension fname with _ -> fname]) in
  navigation_bar tpl (transform_path institut sep p)



let toolbar l =
  let tpl = lang empty l in
  let admin = sprintf "http://portail.umons.ac.be/%s/universite/admin" l in
  let tpl = url_annuaire tpl
    "http://telephone.umh.ac.be/reppersumons/REPPERSlist.asp" in
  let tpl = url_biblio tpl "http://w3.umh.ac.be/Bibli/sms.htm" in
  let tpl = url_elearning tpl
    "https://applications.umons.ac.be/moodleumh/course/category.php?id=2" in
  let tpl = url_plan tpl
    (admin ^ "/scrp/plancampus/Pages/CampusSciencesMédecineFTI-EII.aspx") in
  let tpl = url_emploi tpl (admin ^ "/drh/emploi/Pages/Emploi.aspx") in
  let tpl = url_agenda tpl (admin ^ "/scrp/Pages/Agenda.aspx") in
  tpl

