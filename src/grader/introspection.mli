(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type 'a value =
  | Absent
  | Present of 'a
  | Incompatible of string
val get_value: Longident.t -> 'a Ty.ty -> 'a value

val print_value: Format.formatter -> 'a -> 'a Ty.ty -> unit
val sample_value: 'a Ty.ty -> 'a

val insert_in_env: string -> 'a Ty.ty -> 'a -> unit

val get_mod_ast: var_name:string -> string -> Parsetree.structure

val register_callback:  string -> 'a Ty.ty -> ('a -> unit) -> unit

val allow_introspection:
  divert:(string -> out_channel -> (string -> unit) -> (unit -> unit)) ->
  (module Introspection_intf.INTROSPECTION)
