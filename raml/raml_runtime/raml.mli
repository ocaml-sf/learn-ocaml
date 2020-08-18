(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   raml_runtime/raml.mli
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Interface of top-level RAML runtime module.
 *)

(* val undefined : 'a *)
val tick : float -> unit
val ref_swap : 'a ref -> 'a -> 'a
val consume : 'a -> unit
