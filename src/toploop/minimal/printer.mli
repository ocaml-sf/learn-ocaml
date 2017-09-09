(* $Id: printer.mli,v 1.2 2004/09/24 00:51:16 garrigue Exp $ *)

open Typs;;

val reset_vars : unit -> unit
val print_type : type_expr -> unit
;;

val print_scheme : type_expr -> unit;;

val print_value : type_expr -> Obj.t -> unit;;
