(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* This is trivial and dummy "encryption" for the tests and the solutions. *)

val encode: ?prefix:string -> string -> string
val decode: ?prefix:string -> string -> string
