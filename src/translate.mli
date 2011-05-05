
(** Module that access the Google translation service. *)

(** Supported languages *)
type language =
  | Arabic | Bulgarian | Chinese | Croatian | Czech | Danish | Dutch
  | English | Finnish | French | German | Greek | Hindi | Italian
  | Japanese | Korean | Norwegian | Polish | Portugese | Romanian
  | Russian | Spanish | Swedish

val google : ?from_lang:language -> ?to_lang:language -> string -> string
  (** [translate txt] translate the texte [txt] from the language
      [from_lang] to the language [to_lang].  By default, [from_lang]
      is [French] and [to_lang] is [English]. *)
