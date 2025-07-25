(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Modular authentication framework for Learn-OCaml. *)

open Learnocaml_data

val generate_hmac : string -> string -> string -> string

(** Authentication using LTI (Moodle) *)
module LtiAuth : sig
  type login_credentials = { user_id : string }
  type register_credentials = { user_id : string; nickname : string; csrf : string; hmac : string }
  type associate_credentials = { user_id : string; token : Token.t; csrf : string; hmac : string }

  val login : login_credentials -> (Token.t, string) result Lwt.t
  val register : register_credentials -> (Token.t, string) result Lwt.t
  val associate : associate_credentials -> (Token.t, string) result Lwt.t

  val user_exists : string -> bool Lwt.t
  val get_token : string -> (Token.t, string) result Lwt.t

  val check_oauth : string -> (string * string) list -> string -> (string, string) result Lwt.t
end
