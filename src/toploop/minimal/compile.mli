(* $Id: compile.mli,v 1.3 2004/09/24 00:51:15 garrigue Exp $ *)

open Common;;
open Untyped;;

exception Match_error of string ;;

val compile_expression: expression -> unit -> Obj.t
val compile_commands: command list -> ident list -> Obj.t list
val prealloc_idents: command list -> ident list
;;
