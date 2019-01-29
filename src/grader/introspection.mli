(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

type 'a value =
  | Absent
  | Present of 'a
  | Incompatible of string
val get_value: Longident.t -> 'a Ty.ty -> 'a value

val print_value: Format.formatter -> 'a -> 'a Ty.ty -> unit
val sample_value: 'a Ty.ty -> 'a

val insert_in_env: string -> 'a Ty.ty -> 'a -> unit

val insert_mod_ast_in_env: var_name:string -> string -> unit
val create_ref: string -> 'a Ty.ty -> 'a -> unit -> 'a
val register_callback:  string -> 'a Ty.ty -> ('a -> unit) -> unit

val allow_introspection:
  divert:(string -> out_channel -> (string -> unit) -> (unit -> unit)) ->
  unit
