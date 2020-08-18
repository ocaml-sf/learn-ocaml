(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   raml_runtime/rarray.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   RAML runtime module for arrays (Rarray).  This is a wrapper of the
 *   Array module in the standard library.
 *)

type 'a t = 'a array

let make = Array.make
let create = Array.make

let set = Array.set
let get = Array.get
let length = Array.length
