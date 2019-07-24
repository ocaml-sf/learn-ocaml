(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Library of heterogeneous, nonempty lists and their corresponding types.

    let p = [%prot: int -> bool]

    val p : ((int -> bool) Ty.ty, int -> unit, bool) Prot.prot = <abstr>
*)

(** The type of arguments, represented as heterogeneous lists.

    Usage: [arg 3 @@ arg "word" @@ last false]

    Alternatively: [3 @: "word" @:!! false]
*)
type ('arrow, 'uarrow, 'ret) args
val last :
  'a ->
  ('a -> 'ret, 'a -> unit, 'ret) args
val arg :
  'a ->
  ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
  ('a -> 'ar -> 'row, 'a -> 'ar -> 'urow, 'ret) args

val apply : ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret

(** The type of function prototypes.

    Usage:
    [arg_ty [%ty: int]
     @@ arg_ty [%ty: string] @@ last_ty [%ty: bool] [%ty: unit]]
*)
type ('arrow, 'uarrow, 'ret) prot
val last_ty :
  'a Ty.ty ->
  'ret Ty.ty ->
  (('a -> 'ret) Ty.ty, 'a -> unit, 'ret) prot
val arg_ty :
  'a Ty.ty ->
  (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot ->
  (('a -> 'ar -> 'row) Ty.ty, ('a -> 'ar -> 'urow), 'ret) prot

val ty_of_prot :
  (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot -> ('ar -> 'row) Ty.ty
val get_ret_ty :
  ('p -> 'a) Ty.ty -> ('p -> 'a, 'p -> 'c, 'ret) args -> 'ret Ty.ty

module type S = sig
  val typed_printer : 'a Ty.ty -> Format.formatter -> 'a -> unit
  val typed_sampler : 'a Ty.ty -> unit -> 'a
end

module Make : functor (M : S) -> sig
  val print :
    (('p -> 'a) Ty.ty, 'p -> 'c, 'r) prot ->
    Format.formatter -> ('p -> 'a, 'p -> 'c, 'r) args -> unit
  val get_sampler :
    (('p -> 'a) Ty.ty, 'p -> 'c, 'r) prot ->
    unit -> ('p -> 'a, 'p -> 'c, 'r) args
end

val apply_args_1 :
  ('a -> 'b) -> ('a -> 'c, 'a -> unit, 'c) args -> 'b
val apply_args_2 :
  ('a -> 'b -> 'c) -> ('a -> 'b -> 'd, 'a -> 'b -> unit, 'd) args -> 'c
val apply_args_3 :
  ('a -> 'b -> 'c -> 'd) ->
  ('a -> 'b -> 'c -> 'e, 'a -> 'b -> 'c -> unit, 'e) args -> 'd
val apply_args_4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  ('a -> 'b -> 'c -> 'd -> 'f, 'a -> 'b -> 'c -> 'd -> unit, 'f) args -> 'e
