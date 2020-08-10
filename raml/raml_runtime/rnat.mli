(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   raml_runtime/rnat.mli
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Interface of RAML runtime module for natural number (Rnat).
 *)

type t = int

val zero : t
val succ : t -> t

val to_int : t -> int
val of_int : int -> t

val add : t -> t -> t
val mult : t -> t -> t
val minus : t -> t -> t * t
val minusc : int -> t -> t * t
val div_mod : t -> t -> t * t * t

val ifz : t -> (unit -> 'a) -> (t -> 'a) -> 'a
