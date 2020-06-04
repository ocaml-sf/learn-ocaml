val sync_dir : string ref
val mutex_json_token : Lwt_mutex.t
val cast_list : 'a -> [> `List of 'a ]
val to_json_string : string -> Yojson.Basic.t
val token_to_string : Learnocaml_data.Token.t list -> string list
val get_fichier : string -> unit -> Yojson.Basic.t
val get_token : unit -> string list
val transformation_liste : string list -> Yojson.Basic.t list
val ecrire_index :  string list -> unit Lwt.t
val creer_index : unit Lwt.t
val ajouter_token : string -> unit -> unit Lwt.t
val test : unit -> Learnocaml_data.Token.t list Lwt.t
