(* Specific functions for the UMONS template. *)

open Printf
open Nethtml

let concat_path p f =
  if p = "" then f
  else if p.[String.length p - 1] = '/' then p ^ f
  else p ^ "/" ^ f

let separation url_base =
  Element("span", ["class", "separation"],
          [Element("img", ["src", url_base ^ "images/right_arrow.png";
                           "alt", "&gt;"], [])])

let rec transform_path sep p = match p with
  | [] -> []
  | [(a, _)] -> [sep; Data a]
  | (a, rev) :: tl ->
      let el = Element("a", ["href", rev], [Data a]) in
      sep :: el :: transform_path sep tl

let navigation_of_path tpl p =
  Set.navigation_bar tpl begin fun t ->
    let sep = separation (Get.url_base t) in
    transform_path sep (Template.Path.navigation p ~base:(Get.title t))
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

let rec list_last_element = function
  | [] -> failwith "list_last_element"
  | [e] -> e
  | _ :: tl -> list_last_element tl

let bbclone tpl p =
  Set.web_counter tpl begin fun t ->
    let nav = Template.Path.navigation p ~base:(Get.title t) in
    let name, _ = list_last_element nav in
    (* HACK: "<?" is not supported by Nethtml, we use the fact that no
       escaping is done when printing Data values. *)
    [Data(sprintf "<?php
	define(\"_BBC_PAGE_NAME\", %S);
	define(\"_BBCLONE_DIR\", %S . \"../bbclone/\");
	define(\"COUNTER\", _BBCLONE_DIR.\"mark_page.php\");
	if (is_readable(COUNTER)) include_once(COUNTER);
	?>" name (Get.url_base t))]
  end

let separation_bar =
  Element("span", ["class", "separation-bar"], [Data "|"])

let html_of_language (lang, url) =
  if url = "" then Data lang
  else Element("a", ["href", url], [Data lang])

let rec html_of_languages = function
  | [] -> []
  | [lang] -> [html_of_language lang]
  | lang :: tl ->
      html_of_language lang :: separation_bar :: html_of_languages tl

let languages tpl langs =
  languages tpl (html_of_languages langs)
