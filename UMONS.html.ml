(* Specific functions for the UMONS template. *)

open Printf
open Nethtml

let concat_path p f =
  if p = "" then f
  else if p.[String.length p - 1] = '/' then p ^ f
  else p ^ "/" ^ f

let separation url_base =
  Element("span", ["class", "separation"],
          [Element("img", ["src", url_base ^ "images/right_arrow.png"], [])])

(* For a path [p] possibly including a final file (which we are
   displaying), add the path to go to each directory of the path. *)
let rec add_rev_path p has_final_file = match p with
  | [] -> []
  | [fname] -> [(fname, "")]
  | [dir; fname] when has_final_file -> [(dir, "."); (fname, "")]
  | dir :: tl ->
      let p = add_rev_path tl has_final_file in
      (dir, concat_path "../" (snd(List.hd p))) :: p

(* All paths start with "." which stands for the institute. *)
let rec transform_path institut sep p = match p with
  | [] -> []
  | [(a, _)] -> [sep; Data(if a = "." then institut else String.capitalize a)]
  | (a, rev) :: tl ->
      let a = if a = "." then institut else String.capitalize a in
      let el = Element("a", ["href", rev], [Data a]) in
      sep :: el :: transform_path institut sep tl

let navigation_of_path tpl rel_path fname =
  Set.navigation_bar tpl begin fun t ->
    let institut = Get.title t in
    let sep = separation (Get.url_base t) in
    let p = Neturl.split_path rel_path in
    let p =
      if fname = "index.html" || fname = "index.htm" then add_rev_path p false
      else
        let p = p @ [try Filename.chop_extension fname with _ -> fname] in
        add_rev_path p true in
    transform_path institut sep p
  end


let stylesheet tpl ?(rel_base=true) url =
  Set.stylesheet tpl begin fun t ->
    let url = if rel_base then concat_path (Get.url_base t) url else url in
    let s =Element("link", ["rel", "stylesheet"; "type", "text/css";
                            "media","all"; "href", url],
                   []) in
    [s]
  end

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

