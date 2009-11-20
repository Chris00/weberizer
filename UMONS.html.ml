(* Specific functions for the UMONS template. *)

open Printf
open Nethtml

let concat_path p f =
  if p = "" then f
  else if p.[String.length p - 1] = '/' then p ^ f
  else p ^ "/" ^ f

let separation_arrow url_base =
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
    let sep = separation_arrow (Get.url_base t) in
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

let separation_bar =
  Element("span", ["class", "separation-bar"], [Data "|"])

let html_of_item (name, url) =
  if url = "" then Data name
  else Element("a", ["href", url], [Data name])

let rec horizontal_toolbar = function
  | [] -> []
  | [item] -> [html_of_item item]
  | item :: tl -> html_of_item item :: separation_bar :: horizontal_toolbar tl

let toolbar_fr contact map =
  let admin = sprintf "http://portail.umons.ac.be/FR/universite/admin" in
  ["Annuaire", "http://telephone.umh.ac.be/reppersumons/REPPERSlist.asp";
   "Biblioth�ques", "http://w3.umh.ac.be/Bibli/sms.htm";
   "Cours en ligne",
   "https://applications.umons.ac.be/moodleumh/course/category.php?id=2";
   "Plan d'acc�s", map;
   "Contact", contact;
   "Emploi", (admin ^ "/drh/emploi/Pages/Emploi.aspx");
   "Agenda", (admin ^ "/scrp/Pages/Agenda.aspx") ]

let toolbar_en contact map =
  let admin = sprintf "http://portail.umons.ac.be/EN/universite/admin" in
  ["Directory", "http://telephone.umh.ac.be/reppersumons/REPPERSlist.asp";
   "Libraries", "http://w3.umh.ac.be/Bibli/sms.htm";
   "E-learning",
   "https://applications.umons.ac.be/moodleumh/course/category.php?id=2";
   "Directions", map;
   "Contact", contact;
   "Jobs", (admin ^ "/drh/emploi/Pages/Emploi.aspx");
   "Agenda", (admin ^ "/scrp/Pages/Agenda.aspx") ]

let toolbar tpl ?contact ?map p =
  let l = String.lowercase(Template.Path.language p) in
  let tpl = lang tpl l in
  let base = Template.Path.to_base p in
  let tpl = url_base tpl base in
  let contact = match contact with
    | None -> base ^ "contact/" | Some c -> c in
  let map = match map with
    | None -> base ^ "contact/acces.html#maps" | Some m -> m in
  let bar = match l with
    | "en" -> horizontal_toolbar (toolbar_en contact map)
    | "fr" | "" -> horizontal_toolbar (toolbar_fr contact map)
    | _ -> failwith "UMONS.toolbar: language not recognized" in
  toolbar tpl bar


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
        @apache_setenv('no-gzip', 1);
        @ini_set('zlib.output_compression', 0);
        @ini_set('implicit_flush', 1);
	define(\"_BBC_PAGE_NAME\", %S);
	define(\"_BBCLONE_DIR\", %S . \"../bbclone/\");
	define(\"COUNTER\", _BBCLONE_DIR.\"mark_page.php\");
	if (is_readable(COUNTER)) include_once(COUNTER);
	?>" name (Get.url_base t))]
  end

let languages tpl langs =
  languages tpl (horizontal_toolbar langs)
