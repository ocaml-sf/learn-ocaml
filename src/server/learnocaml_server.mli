(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Configuration options *)

val port: int ref
val cert_key_files: (string * string) option ref
val base_url: string ref

val args: (Arg.key * Arg.spec * Arg.doc) list

(** Main *)

val check_running: unit -> int option
(** Returns the pid or an existing process listening on the tcp port *)

val kill_running: int -> unit
(** Kills the given process and waits for termination (fails upon
    reaching a timeout) *)

val launch: unit -> bool Lwt.t
(** Returns [false] if interrupted prematurely due to an error *)
