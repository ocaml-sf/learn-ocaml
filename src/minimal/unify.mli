(* $Id: unify.mli,v 1.3 2004/09/24 00:51:17 garrigue Exp $ *)

open Typs;;

exception Unify;;

val generic_level : int ;;

val begin_def : unit -> unit
val end_def : unit -> unit
;;

val reset_global_level : unit -> unit
val new_global_var : unit -> type_var
;;

val newvar : unit -> type_var
val repr : type_expr -> type_expr
;;

val map_type : (type_expr -> type_expr) -> type_expr -> type_expr
val do_type : (type_expr -> unit) -> type_expr -> unit
;;

val generalize : type_expr -> unit
val make_nongen : type_expr -> unit
;;

val subst : (type_var * type_expr) list -> type_expr -> type_expr ;;

val expand : type_expr -> type_expr ;;
  (* raises Not_found when cannot expand *)

val unify : type_expr -> type_expr -> unit
val filter_arrow : type_expr -> (type_expr * type_expr)
;;  (* both raise Unify *)
