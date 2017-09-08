(* $Id: typechk.mli,v 1.3 2004/09/24 00:51:16 garrigue Exp $ *)

open Common;;
open Syntax;;
open Typs;;

type error ;;

exception Type_error of error * location ;;

val add_typedef: location -> typedef list -> unit ;;

val hide_type: location -> string -> unit ;;

val type_command: value_info StrMap.t -> command -> value_info StrMap.t
val type_expression: value_info StrMap.t -> expression -> type_expr
val report_error: error -> unit
;;
