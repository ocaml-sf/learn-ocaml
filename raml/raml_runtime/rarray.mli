(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   raml_runtime/rarray.mli
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Interface of RAML runtime module for arrays (Rarray).  This is a subset
 *   of the Array module in the standard library.
 *)

type 'a t

val make : Rnat.t -> 'a -> 'a t
val create : Rnat.t -> 'a -> 'a t  (* Deprecated: alias for [make] *)

val set : 'a t -> Rnat.t -> 'a -> unit
val get : 'a t -> Rnat.t -> 'a
val length : 'a t -> Rnat.t
