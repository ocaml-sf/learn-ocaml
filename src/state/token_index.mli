(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Create or regenerate token index from sync/ and write sync/token.json.
    This step may take a long time (up to several minutes).  Automatically
    called (once and for all) by [get_tokens] or [add_token] if need be.
    The first argument denotes the sync directory path. *)
val create_index : string -> unit Lwt.t

(** Get the list of all tokens. *)
val get_tokens : string -> Learnocaml_data.Token.t list Lwt.t

(** Add a registered token in the index. *)
val add_token : Learnocaml_data.Token.t -> string -> unit Lwt.t
