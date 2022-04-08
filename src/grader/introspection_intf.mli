(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
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

  val register_sampler: string -> ('a -> 'b) -> unit
  val get_sampler: 'a Ty.ty -> (unit -> 'a)

  val install_printer:
    Path.t -> Types.type_expr -> (Format.formatter -> Obj.t -> unit) -> unit

  val get_printer: 'a Ty.ty -> (Format.formatter -> 'a -> unit)

  val parse_lid: string -> Longident.t

end

(** Interface of the module that gets automatically injected in the environment
    before the Prelude is loaded. *)
module type LEARNOCAML_CALLBACK = sig
  val print_html: string -> unit
  val print_svg: string -> unit
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
