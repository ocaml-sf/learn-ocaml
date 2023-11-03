(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

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

  val install_printer:
    Path.t -> Types.type_expr -> (Format.formatter -> Obj.t -> unit) -> unit

  val get_printer: 'a Ty.ty -> (Format.formatter -> 'a -> unit)

  val parse_lid: string -> Longident.t

  (**/**)
  (** Only for use by learnocaml's ppx *)
  (* The sampler type is actually [['x sampler ->]* t sampler] with ['x] all the
     type variables of [t]. It is dynamically checked at runtime, based on the
     cmi of the module that must be already loaded and opened. *)
  val register_sampler:
    string -> string -> string -> ('a -> 'b) -> unit
end

(** Interface of the module that gets automatically injected in the environment
    of the grader before the tests are run. *)
module type PRE_TEST = sig
  module Introspection: INTROSPECTION

  val code_ast: Parsetree.structure
  val results: Learnocaml_report.t option ref
  val set_progress: string -> unit
  val timeout: int option
end
