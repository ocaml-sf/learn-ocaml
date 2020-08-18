(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   simplify.mli
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Simplifying OCaml types and typed expressions into RAML types and
 *   expressions.
 *)

open Core.Std

open Expressions
open Rtypes

exception Eunsupported_type     of Location.t * Types.type_expr * string
exception Eunsupported_pattern  of Typedtree.pattern
exception Eunsupported_constant of Location.t * Asttypes.constant
exception Eunsupported_expr     of Typedtree.expression
exception Eunsupported_primitive of string
exception Eno_main_expression
exception Enot_an_instance      of raml_type * raml_type
exception Emonomorphic_var      of string * raml_type * Location.t
                                          * raml_type * Location.t

(* From a [Typedtree] expression, [simplify_expression] generates a simpler
   RAML version, a map from data constructors to their corresponding RAML types
   (must be [Tind]), and a map from data constuctors to the original OCaml name
   of the data type, so that the pretty printer can output a name instead of
   the full structure of the [Tind]. *)
val simplify_expression : Typedtree.expression ->
      typed_expression * raml_type String.Map.t * string String.Map.t

(* [simplify_structure] is similar to [simplify_expression] except it takes
   a module which should be treated as a program. *)
val simplify_structure : Typedtree.structure ->
      typed_expression * raml_type String.Map.t * string String.Map.t

(* [simplify_module] takes a module and return a list of name-RAML expression
   pairs, which is RAML's representation of a module.  The other two return
   values are the same as above. *)
val simplify_module : Typedtree.structure ->
      (string * typed_expression) list *
        raml_type String.Map.t * string String.Map.t

