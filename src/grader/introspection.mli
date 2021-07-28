(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type 'a value = Absent | Present of 'a | Incompatible of string

val get_value : Longident.t -> 'a Ty.ty -> 'a value

val print_value : Format.formatter -> 'a -> 'a Ty.ty -> unit

val sample_value : 'a Ty.ty -> 'a

val insert_in_env : string -> 'a Ty.ty -> 'a -> unit

val insert_mod_ast_in_env : var_name:string -> string -> unit

val create_ref : string -> 'a Ty.ty -> 'a -> unit -> 'a

val register_callback : string -> 'a Ty.ty -> ('a -> unit) -> unit

val allow_introspection :
  divert:(string -> out_channel -> (string -> unit) -> unit -> unit) -> unit
