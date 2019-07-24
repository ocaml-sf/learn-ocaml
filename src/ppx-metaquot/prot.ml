(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* Usage: arg 3 @@ arg "word" @@ last false *)
(* Alternatively: 3 @: "word" @:!! false *)
type (_, _, _) args =
  | Last : 'a -> ('a -> 'r, 'a -> unit, 'r) args
  | Arg : 'a * ('b, 'c, 'r) args -> ('a -> 'b, 'a -> 'c, 'r) args

let last x = Last x
let arg x r = Arg (x, r)

let rec apply : type p a c r. (p -> a) -> (p -> a, p -> c, r) args -> r = fun f x ->
  match x with
  | Last x -> f x
  | Arg (x, Last r) -> (f x) r
  | Arg (x, Arg (y, r)) -> apply (f x) (Arg (y, r))

(* Usage: arg_ty [%ty: int] @@ arg_ty [%ty: string] @@ last_ty [%ty: bool] [%ty: unit] *)
type (_, _, _) prot =
  | Last_ty : 'a Ty.ty * 'r Ty.ty -> (('a -> 'r) Ty.ty, 'a -> unit, 'r) prot
  | Arg_ty : 'a Ty.ty * (('b -> 'c) Ty.ty, 'b -> 'd, 'r) prot -> (('a -> 'b -> 'c) Ty.ty, 'a -> 'b -> 'd, 'r) prot

let last_ty x r = Last_ty (x, r)
let arg_ty x r = Arg_ty (x, r)

let rec ty_of_prot : type p a c r. ((p -> a) Ty.ty, p -> c, r) prot -> (p -> a) Ty.ty  = function
  | Last_ty (a, b) -> Ty.curry a b
  | Arg_ty (x, Last_ty (l, r)) -> Ty.curry x (Ty.curry l r)
  | Arg_ty (x, Arg_ty (y, r)) -> Ty.curry x (ty_of_prot (Arg_ty (y, r)))

let rec get_ret_ty
        : type p a c r. (p -> a) Ty.ty -> (p -> a, p -> c, r) args -> r Ty.ty =
  fun ty x ->
  match x with
  | Last _ ->
     let _, ret_ty = Ty.domains ty in
     ret_ty
  | Arg (_, Last r) ->
     let _, ret_ty = Ty.domains ty in
     get_ret_ty ret_ty (Last r)
  | Arg (_, Arg (y, r)) ->
     let _, ret_ty = Ty.domains ty in
     get_ret_ty ret_ty (Arg (y, r))

module type S = sig
  val typed_printer : 'a Ty.ty -> Format.formatter -> 'a -> unit
  val typed_sampler : 'a Ty.ty -> unit -> 'a
end

module Make (M : S) = struct
  let rec print
          : type p a c r. ((p -> a) Ty.ty, p -> c, r) prot -> Format.formatter -> (p -> a, p -> c, r) args -> unit =
    fun t ppf x ->
    match t, x with
    | Last_ty (arg_ty, _), Last x ->
       Format.fprintf ppf "@ %a"
         (M.typed_printer arg_ty) x
    | Arg_ty (arg_ty, ret_ty), Arg (x, Last r) ->
       Format.fprintf ppf "@ %a%a"
         (M.typed_printer arg_ty) x
         (print ret_ty) (Last r)
    | Arg_ty (arg_ty, ret_ty), Arg (x, Arg (y, r)) ->
       Format.fprintf ppf "@ %a%a"
         (M.typed_printer arg_ty) x
         (print ret_ty) (Arg (y, r))
    | Last_ty (_, _), Arg (_, _) -> .

  let rec get_sampler
          : type p a c r. ((p -> a) Ty.ty, p -> c, r) prot -> unit -> (p -> a, p -> c, r) args =
    fun wit ->
    match wit with
    | Last_ty (x, _) ->
       let arg_sampler = M.typed_sampler x in
       (fun () -> Last (arg_sampler ()))
    | Arg_ty (x, Last_ty (l, r)) ->
       let arg_sampler = M.typed_sampler x in
       let ret_sampler = get_sampler (Last_ty (l, r)) in
       (fun () -> let arg = arg_sampler () in Arg (arg, ret_sampler ()))
    | Arg_ty (x, Arg_ty (y, r)) ->
       let arg_sampler = M.typed_sampler x in
       let ret_sampler = get_sampler (Arg_ty (y, r)) in
       (fun () -> let arg = arg_sampler () in Arg (arg, ret_sampler ()))
end

let apply_args_1 f = function Last x -> f x

let apply_args_2 f = function | Arg (x, Last y) -> f x y

let apply_args_3 f = function Arg (w, Arg (x, Last y)) -> f w x y

let apply_args_4 f = function Arg (w, Arg (x, Arg (y, Last z))) -> f w x y z
