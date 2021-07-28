(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Runtime library for our custom 'ppx_metaquot'.

    let x : int ty = [% ty : int ]

*)

type repr

type 'a ty = Ty of repr

val obj : 'a ty -> Parsetree.core_type

val repr : Parsetree.core_type -> 'a ty

val print : 'a ty -> string

val domains : ('a -> 'b) ty -> 'a ty * 'b ty

val curry : 'a ty -> 'b ty -> ('a -> 'b) ty

val pair2 : 'a ty -> 'b ty -> ('a * 'b) ty

val pair3 : 'a ty -> 'b ty -> 'c ty -> ('a * 'b * 'c) ty

val pair4 : 'a ty -> 'b ty -> 'c ty -> 'd ty -> ('a * 'b * 'c * 'd) ty

val lst : 'a ty -> 'a list ty
