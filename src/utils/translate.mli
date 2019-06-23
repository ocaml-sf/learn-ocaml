(** Functions for the internationalization of the user interface *)

(** Allow the translation of the strings in the ocaml file
	written down as [%i"something"] *)
val set_lang : unit -> unit

(** Allow the translation of the strings in the html file
    @param a list of ("the id of an html tag", [%i"something in this tag"]) *)
val set_string_translations : (string * string) list -> unit

(** Allow the translation of the strings in the html file
    @param a list of ("the id of an html tag", [%i"something in this tag title"]) *)
val set_title_translations : (string * string) list -> unit
