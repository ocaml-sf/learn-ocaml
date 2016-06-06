(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
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

(** Runtime library for our custom 'ppx_metaquot'.

    let x : int ty = [% ty : int ]

*)

type repr
type 'a ty = Ty of repr

val obj: 'a ty -> Parsetree.core_type
val repr: Parsetree.core_type -> 'a ty
val print: 'a ty -> string
val domains: ('a -> 'b) ty -> 'a ty * 'b ty
val curry: 'a ty -> 'b ty -> ('a -> 'b) ty
