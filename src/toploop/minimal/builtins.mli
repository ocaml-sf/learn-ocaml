(* $Id: builtins.mli,v 1.3 2004/09/24 00:51:15 garrigue Exp $ *)

open Typs;;

val handlers : (exn -> bool) list ref;;
val handle : exn -> bool;;

val array : type_expr -> type_expr
val list : type_expr -> type_expr
val arr2 : type_expr -> type_expr -> type_expr -> type_expr
val arr : type_expr -> type_expr -> type_expr
val b : type_expr
val a : type_expr
val unit : type_expr
val bool : type_expr
val float : type_expr
val char : type_expr
val int : type_expr
val string : type_expr
;;

val builtins : (string * Obj.t * type_expr) list ref;;
val add_builtins : unit -> unit;;
