(* $Id: untyped.ml,v 1.5 2004/09/24 00:51:17 garrigue Exp $ *)

open Common;;

type pattern =
    UPid of ident
  | UPconst of constant
  | UPblock of int * pattern list
  | UPany
;;

type command =
    UEexpr of expression
  | UEval of pattern * expression * string	(* last is error message *)
  | UEfun of (ident * expression) list
  | UEvar of ident * expression

and expression =
    UEid of ident
  | UEconst of constant
  | UEblock of int * expression list
  | UEapply of expression * expression list
  | UEfunct of ident list * expression
  | UEseq of command list * expression
  | UEcase of expression * (pattern * expression) list * string	(* likewise *)
  | UEifthenelse of expression * expression * expression
  | UEset of ident * expression
  | UEfor of ident * expression * dirflag * expression * expression
  | UEwhile of expression * expression
;;
