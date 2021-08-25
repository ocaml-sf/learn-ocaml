(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Configuration options *)

val exercises_dir : string ref

val exercises_index : string option ref

val exercises_filtered : Learnocaml_data.SSet.t ref

val dump_outputs : string option ref

val dump_reports : string option ref

val n_processes : int ref

(** Main *)

val main : string -> bool Lwt.t
(** [dest_dir] -> success *)
