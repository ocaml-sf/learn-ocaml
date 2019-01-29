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

(** Signature for the types self-introspection module.

    Value of type ['a ty] might be build using the patched
    [ppx_metaquot] with the extension node [[%ty: int -> int ] : (int
    -> int) Ty.ty].

*)

module type INTROSPECTION = sig

  type 'a value =
    | Absent
    | Present of 'a
    | Incompatible of string

  val get_value: string -> 'a Ty.ty -> 'a value
  val print_value: Format.formatter -> 'a -> 'a Ty.ty -> unit
  (* expected first *)
  val compatible_type: string -> string -> unit value

  exception Excess

  val grab_stdout: unit -> unit
  val release_stdout: unit -> string

  val grab_stderr: unit -> unit
  val release_stderr: unit -> string

  val get_sampler: 'a Ty.ty -> (unit -> 'a)
  val get_printer: 'a Ty.ty -> (Format.formatter -> 'a -> unit)

  val parse_lid: string -> Longident.t

end
