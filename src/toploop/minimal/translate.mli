(* $Id: translate.mli,v 1.2 2004/09/24 00:51:16 garrigue Exp $ *)

open Common;;
open Syntax;;

val expression :
	(string * ident) list -> expression -> Untyped.expression
val command :
	 (string * ident) list -> command ->
	   Untyped.command list * (string * ident) list
;;
