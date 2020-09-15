(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Configuration options *)

val port: int ref
val cert_key_files: (string * string) option ref
val root_url: string ref

val args: (Arg.key * Arg.spec * Arg.doc) list

(** Main *)

(* Returns [false] if interrupted prematurely due to an error *)
val launch: unit -> bool Lwt.t
