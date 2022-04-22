(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

module type ARG = sig
  val val_prefix: string
  val inject_def: string -> string -> string Ppxlib.loc -> Ppxlib.expression
end

module Make (_: ARG): sig
  val expand: Ppxlib.structure -> Ppxlib.structure
end

(** Helper function extracting the module name from the location of a variable
   (only at top-level) *)
val modname: 'a Location.loc -> string
