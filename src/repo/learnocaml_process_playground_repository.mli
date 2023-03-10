(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Configuration options *)

val playground_dir: string ref
val playground_index: string option ref

(** Main *)

(** [dest_dir] -> success *)
val main: string -> bool Lwt.t
