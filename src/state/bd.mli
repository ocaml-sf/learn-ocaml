val sync_dir : string ref
val mutex_json_token : Lwt_mutex.t
val cast_list : 'a -> [> `List of 'a ]
val string_to_json : string -> Yojson.Basic.t
val token_to_string : Learnocaml_data.Token.t list -> string list
val get_file : string -> unit -> Yojson.Basic.t Lwt.t
val get_tokens : unit -> Learnocaml_data.Token.t list Lwt.t
val list_cast : string list -> Yojson.Basic.t list
val write_index :  string list -> unit Lwt.t
val create_index : unit Lwt.t
val add_token : Learnocaml_data.Token.t -> unit -> unit Lwt.t
