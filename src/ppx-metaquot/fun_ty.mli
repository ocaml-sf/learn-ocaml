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

(** [last e], or equivalently [!! e], builds a one-element argument list *)
val last :
  'a ->
  ('a -> 'ret, 'a -> unit, 'ret) args

(** [arg e l], or equivalently [e @: l], adds [e] in front of the
   argument list [l] *)
val arg :
  'a ->
  ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
  ('a -> 'ar -> 'row, 'a -> 'ar -> 'urow, 'ret) args

(** [apply f l] applies a n-ary function [f] to the arguments from [l] *)
val apply :
  ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
  'ret

(** GADT for function types.

    Given an arrow type ['a -> 'row], the following construct provides
    a more precise representation of this function type than
    [[%ty: 'a -> 'row] : ('a -> 'row) Ty.ty]:

    [[%funty: 'a -> 'row] : (('a -> 'row) Ty.ty, 'a -> 'urow, 'ret) fun_ty]

    In particular, the codomain type ['ret] is made explicit, so that
    if ['row = 'b -> 'c], we get ['urow = 'b -> unit] and ['ret = 'c].

    Usage: [arg_ty [%ty: int] @@ arg_ty [%ty: string] @@
            last_ty [%ty: bool] [%ty: unit]]

    Alternatively: [[%funty: int -> string -> bool -> unit]] *)
type ('arrow, 'uarrow, 'ret) fun_ty

(** [last_ty [%ty: a] [%ty: r]] builds a function type for [a -> r] *)
val last_ty :
  'a Ty.ty ->
  'ret Ty.ty ->
  (('a -> 'ret) Ty.ty, 'a -> unit, 'ret) fun_ty

(** [arg_ty [%ty: a] [%funty: b ->...-> r]] builds a function type for
    [a -> b ->...-> r] *)
val arg_ty :
  'a Ty.ty ->
  (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
  (('a -> 'ar -> 'row) Ty.ty, ('a -> 'ar -> 'urow), 'ret) fun_ty

(** [ty_of_fun_ty funty] returns a term of type [('ar -> 'row) Ty.ty],
    assuming [funty : (('ar -> 'row) Ty.ty, _, _) fun_ty] *)
val ty_of_fun_ty :
  (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
  ('ar -> 'row) Ty.ty

(** [get_ret_ty funty] returns a term of type ['ret Ty.ty], assuming
   [funty : (_ , _, 'ret) fun_ty] *)
val get_ret_ty :
  ('p -> 'a) Ty.ty -> ('p -> 'a, 'p -> 'c, 'ret) args -> 'ret Ty.ty


(** Signature [S] is intended to be instantiated in [Test_lib] with:
    [module M = struct
      let typed_printer ty ppf v = Introspection.print_value ppf v ty
      let typed_sampler = Introspection.get_sampler
    end] *)
module type S = sig
  val typed_printer :
    'a Ty.ty -> Format.formatter -> 'a -> unit
  val typed_sampler :
    'a Ty.ty -> unit -> 'a
end

(** [Make(M)] provides a generic printer and sampler for the arguments
    of n-ary functions specified using [args] and [fun_ty] GADTs *)
module Make : functor (M : S) -> sig
  val print :
    (('p -> 'a) Ty.ty, 'p -> 'c, 'r) fun_ty ->
    Format.formatter -> ('p -> 'a, 'p -> 'c, 'r) args -> unit
  val get_sampler :
    (('p -> 'a) Ty.ty, 'p -> 'c, 'r) fun_ty ->
    unit -> ('p -> 'a, 'p -> 'c, 'r) args
end

(** [apply_args_1], [apply_args_2], [apply_args3], [apply_args_4] are
    variants of the [apply] function, assuming a fixed number of args;
    they have thus a more precise type and are used in [Test_lib] *)
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
