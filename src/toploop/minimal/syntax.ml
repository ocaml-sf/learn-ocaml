(* $Id: syntax.ml,v 1.4 2004/09/24 00:51:16 garrigue Exp $ *)

open Common;;

type location = { first: int ; last: int} ;;

type type_expr = { st_desc: type_desc ; st_loc: location }
and type_desc =
    STvar    of string
  | STarrow  of type_expr * type_expr
  | STtuple  of type_expr list
  | STconstr of string * type_expr list
;;

type typedef =
    { sd_name: string ;
      sd_params: string list ;
      sd_kind: type_kind ;
      sd_loc: location }
and type_kind =
    SKabbrev of type_expr
  | SKvariant of (string * type_expr list) list
  | SKrecord of (string * type_expr * access) list
;;

type pattern = { sp_desc: pattern_desc ; sp_loc: location }
and pattern_desc =
    SPid of string
  | SPconst of constant
  | SPtuple of pattern list
  | SParray of pattern list
  | SPconstr of string * pattern
  | SPrecord of (string * pattern) list
  | SPany
  | SPtype of pattern * type_expr
;;

type expression = { mutable se_desc: expression_desc ; se_loc: location }
and expression_desc =
    SEid of string
  | SEconst of constant
  | SEtuple of expression list
  | SEarray of expression list
  | SErecord of (string * expression) list
  | SEapply of expression * expression list
  | SEfunct of pattern list * expression
  | SEseq of command list
  | SEcase of expression * (pattern * expression) list
  | SEifthenelse of expression * expression * expression
  | SEset of string * expression
  | SEgetfield of expression * string
  | SEsetfield of expression * string * expression
  | SEfor of string * expression * dirflag * expression * expression
  | SEwhile of expression * expression
  | SEtype of expression * type_expr

and command = { sc_desc: command_desc ; sc_loc: location }
and command_desc =
    SEexpr of expression
  | SEval of (pattern * expression) list
  | SEfun of (string * expression) list
  | SEvar of (string * expression) list
  | STtype of typedef list
  | SThide of string
;;
