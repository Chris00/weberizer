(* Specific functions for the UMONS template. *)

open Printf
open Nethtml

let banner tpl ?(base=true) img =
  Set.url_banner tpl begin fun t ->
    if base then Get.url_base t ^ "/images/" ^ img
    else img
  end

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

let navigation_of_path tpl ?(prefix=[]) p =
  Set.navigation_bar tpl begin fun t ->
    let sep = separation_arrow (Get.url_base t) in
    transform_path sep (prefix @ Template.Path.navigation p)
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
  ["Annuaire", "http://annuaire.umons.ac.be/";
   "Bibliothèques", admin ^ "/biblio/Pages/Catalogue.aspx";
   "Cours en ligne",
   "https://applications.umons.ac.be/moodle/course/category.php?id=2";
   "Plan d'accès", map;
   "Contact", contact;
   "Emploi", (admin ^ "/drh/emploi/Pages/Emploi.aspx");
   "Agenda", (admin ^ "/cerp/Pages/Agenda.aspx");
   "Actualités", "http://portail.umons.ac.be/FR/actualites/" ]

let toolbar_en contact map =
  (* FIXME: must replace FR by EN once the main site will be translated. *)
  let admin = sprintf "http://portail.umons.ac.be/FR/universite/admin" in
  ["Directory", "http://annuaire.umons.ac.be/";
   "Libraries", admin ^ "/biblio/Pages/Catalogue.aspx";
   "E-learning",
   "https://applications.umons.ac.be/moodle/course/category.php?id=2";
   "Directions", map;
   "Contact", contact;
   "Jobs", (admin ^ "/drh/emploi/Pages/Emploi.aspx");
   "Calendar", "http://portail.umons.ac.be/EN/University/admin/scrp/Pages/Agenda.aspx" ]

let toolbar tpl ?contact ?map ?base ~lang:l p =
  let l = String.lowercase l in
  let tpl = lang tpl l in
  let base = match base with
    | None -> Template.Path.to_base p
    | Some base -> base in
  let tpl = url_base tpl base in
  let contact = match contact with
    | None -> base ^ "contact/" | Some c -> c in
  let map = match map with
    | None -> base ^ "contact/acces.html#maps" | Some m -> m in
  let tpl = shortcut_icon tpl (base ^ "images/UMONS16x16.ico") in
  match l with
  | "en" ->
    let tpl = country tpl [Data "Belgium"] in
    let tpl = toolbar tpl (horizontal_toolbar (toolbar_en contact map)) in
    search_name tpl "Search"
  | "fr" ->
    let tpl = country tpl [Data "Belgique"] in
    let tpl = toolbar tpl (horizontal_toolbar (toolbar_fr contact map)) in
    search_name tpl "Rechercher"
  | _ -> failwith(Printf.sprintf "UMONS.toolbar: language %S not supported" l)


let rec list_last_element = function
  | [] -> failwith "list_last_element"
  | [e] -> e
  | _ :: tl -> list_last_element tl

let bbclone tpl p =
  Set.web_counter tpl begin fun t ->
    let nav = Template.Path.navigation p in
    let name, _ = list_last_element nav in
    (* HACK: "<?" is not supported by Nethtml, we use the fact that no
       escaping is done when printing Data values.  Do not escape
       [name] because it is encoded in UTF-8. *)
    [Data(sprintf "<?php
	@ob_end_flush();
	define(\"_BBC_PAGE_NAME\", \"%s\");
	define(\"_BBCLONE_DIR\", %S . \"../bb/\");
	define(\"COUNTER\", _BBCLONE_DIR.\"mark_page.php\");
	if (is_readable(COUNTER)) include_once(COUNTER);
	?>" name (Get.url_base t))]
  end

let languages tpl langs =
  let langs = List.map (fun (n,u) -> (String.capitalize n, u)) langs in
  languages tpl (horizontal_toolbar langs)
