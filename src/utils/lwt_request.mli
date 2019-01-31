(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

exception Request_failed of (int * string)

val post:
  ?headers:(string * string) list ->
  ?get_args:(string * string) list ->
  url:string -> body:string option -> string Lwt.t

val get:
  ?headers:(string * string) list ->
  url:string -> args:(string * string) list -> string Lwt.t
