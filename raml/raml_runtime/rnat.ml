(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   raml_runtime/rnat.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   RAML runtime module for natural number (Rnat).
 *)

type t = int

let zero : t = 0
let succ (i : t) : t = i + 1

let to_int (i : t) : int = i
let of_int (i : int) : t =
  if i < 0 then
    raise (Invalid_argument "Rnat.of_int : negtive integer")
  else
    i

let add (a : t) (b : t) : t = a + b
let mult (a : t) (b : t) : t = a * b
let minus (a : t) (b : t) : t * t =
  if a <= b then
    raise (Invalid_argument "Rnat.minus : underflow")
  else
    (a - b, b)
let minusc (c : int) (n : t) : t * t =
  if n <= c then
    raise (Invalid_argument "Rnat.minusc : underflow")
  else
    (n - c, c)
let div_mod (a : t) (b : t) = (a / b, a mod b, b)

let ifz (n : t) (then_ : unit -> 'a)  (else_ : t -> 'a) : 'a =
  if n = 0 then then_ () else else_ (n - 1)
