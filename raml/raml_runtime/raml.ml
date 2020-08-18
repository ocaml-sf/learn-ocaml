(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   raml_runtime/raml.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Top-level RAML runtime module.
 *)

(* let undefined = raise (Failure "Raml.undefined") *)
let tick _ = ()

let ref_swap r new_val =
  let ret_val = !r in
  r := new_val;  ret_val

let consume _ = ()
