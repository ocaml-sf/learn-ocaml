(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** [Fun_ty] is used by [Test_lib] to implement n-ary graders.

    This module provides two GADTs: [args], representing arguments of
    n-ary functions as nonempty heterogeneous lists, and [fun_ty],
    representing function types in a more explicit way than ['a Ty.ty].

    This module also serves as a runtime library for the customized
    [ppx_metaquot] distributed with learn-ocaml, so that one may write:

    [let p = [%funty: int -> bool]], and get:

    [val p : ((int -> bool) Ty.ty, int -> unit, bool) fun_ty = <abstr>] *)

(** GADT for arguments of n-ary functions, implemented as nonempty
    heterogeneous lists.

    Usage: [arg 3 @@ arg "word" @@ last false]

    Alternatively: [3 @: "word" @:!! false] *)
type ('arrow, 'uarrow, 'ret) args

(** [last e], or equivalently [!! e], builds a one-element argument list. *)
val last :
  'a ->
  ('a -> 'ret, 'a -> unit, 'ret) args

(** [arg a l], or equivalently [a @: l], adds [a] in front of the
   argument list [l]. *)
val arg :
  'a ->
  ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
  ('a -> 'ar -> 'row, 'a -> 'ar -> 'urow, 'ret) args

(** Helper notation for [last]. *)
val (!!) :
  'a ->
  ('a -> 'ret, 'a -> unit, 'ret) args

(** Helper notation for [arg]. *)
val (@:) :
  'a ->
  ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
  ('a -> 'ar -> 'row, 'a -> 'ar -> 'urow, 'ret) args

(** [a @:!! l] is another notation for [a @: !! l] (with a space). *)
val (@:!!) :
  'a -> 'b ->
  ('a -> 'b -> 'ret, 'a -> 'b -> unit, 'ret) args

(** [apply f l] applies a n-ary function [f] to the arguments from [l]. *)
val apply :
  ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
  'ret

(** GADT for function types.

    This abstract type fulfills a similar aim as the ['a Ty.ty] type:
    provide a type-safe way to build terms representing an OCaml type
    (relying on [Parsetree.core_type]), the type of these terms being
    themselves parameterized by the type at stake, in order to
    constrain the type of other arguments involved in the considered
    (grader) expression.

    For ['a Ty.ty], this aim can be achived with the ppx expression
    [[%ty: int]], which builds an abstract term of type [int Ty.ty]
    (this term also gathering an appropriate encoding of the [int]
    type in terms of [Parsetree.core_type]).

    For implementing n-ary graders, this information is not sufficient
    in practice, notably as we need to make it explicit in the type
    annotation, what is the co-domain of the n-ary function at stake.

    This is achieved by the GADT
    [(('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty].

    There are two ways to build terms of this type:

    1. Use the two functions [arg_ty], [last_ty], and appropriate
       [[%ty: type]] expressions, for example:

        [arg_ty [%ty: int] @@
         last_ty [%ty: string] [%ty: bool]];

    2. Use directly the [[%funty: 'ar -> 'row]] construct, for example:

        [[%funty: int -> string -> bool]].

    For both cases in this example, we get a term of type:

    [((int -> string -> bool) Ty.ty, int -> string -> unit, bool) fun_ty],

    where the co-domain type [bool] is now explicit. *)
type ('arrow, 'uarrow, 'ret) fun_ty

(** [last_ty [%ty: a] [%ty: r]] builds a function type for [a -> r]. *)
val last_ty :
  'a Ty.ty ->
  'ret Ty.ty ->
  (('a -> 'ret) Ty.ty, 'a -> unit, 'ret) fun_ty

(** [arg_ty [%ty: a] [%funty: b ->...-> r]] builds a function type for
    [a -> b ->...-> r]. *)
val arg_ty :
  'a Ty.ty ->
  (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
  (('a -> 'ar -> 'row) Ty.ty, ('a -> 'ar -> 'urow), 'ret) fun_ty

(** [ty_of_fun_ty funty] returns a term of type [('ar -> 'row) Ty.ty],
    assuming [funty : (('ar -> 'row) Ty.ty, _, _) fun_ty]. *)
val ty_of_fun_ty :
  (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
  ('ar -> 'row) Ty.ty

(** [get_ret_ty ty l] returns a term of type ['ret Ty.ty] such that if
    [ty : ('ar -> 'row) Ty.ty] and [l] contains n arguments, ['ar -> 'row]
    is the arrow type of an n-argument function with co-domain ['ret]. *)
val get_ret_ty :
  ('ar -> 'row) Ty.ty -> ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret Ty.ty

module type S = sig
  val typed_printer : 'a Ty.ty -> Format.formatter -> 'a -> unit
  val typed_sampler : 'a Ty.ty -> unit -> 'a
end

(** [Make], used in [Test_lib], provides a generic printer and sampler
    for argument lists of n-ary functions, depending on their type. *)
module Make : functor (M : S) -> sig
  val print :
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
    Format.formatter -> ('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit
  val get_sampler :
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
    unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args
end

(** [apply_args_1], [apply_args_2], [apply_args3], [apply_args_4] are
    variants of the [apply] function, assuming a fixed number of args;
    they have thus a more precise type and are used in [Test_lib]. *)
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
