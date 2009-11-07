(* Simple code to access Google translation service. *)

open Http_client.Convenience

type language =
  | Arabic | Bulgarian | Chinese | Croatian | Czech | Danish | Dutch
  | English | Finnish | French | German | Greek | Hindi | Italian
  | Japanese | Korean | Norwegian | Polish | Portugese | Romanian
  | Russian | Spanish | Swedish
;;

let string_of_language = function
  | Arabic -> "ar" | Bulgarian -> "bg" | Chinese -> "zh-CN"
  | Croatian -> "hr" | Czech -> "cs" | Danish -> "da"
  | Dutch -> "nl" | English -> "en" | Finnish -> "fi"
  | French -> "fr" | German -> "de" | Greek -> "el"
  | Hindi -> "hi" | Italian -> "it" | Japanese -> "ja"
  | Korean -> "ko" | Norwegian -> "no" | Polish -> "pl"
  | Portugese -> "pt" | Romanian -> "ro" | Russian -> "ru"
  | Spanish -> "es" | Swedish -> "sv"

let transl_re = Str.regexp "<div id=result_box dir=\"ltr\">\\([^<]*?\\)</div>"

let lang_pair l1 l2 =
  string_of_language l1 ^ "|" ^ string_of_language l2

let google ?(from_lang=French) ?(to_lang=English) txt =
  let params = [ "langpair", lang_pair from_lang to_lang;
                 "text", txt;
                 "ie", "iso-8859-1";  "oe", "iso-8859-1" ] in
  let html = http_post "http://translate.google.com/translate_t" params in
  ignore(Str.search_forward transl_re html 0);
  Netencoding.Url.decode (Str.matched_group 1 html)
