(* $Id: types.ml,v 1.4 2004/09/24 00:51:16 garrigue Exp $ *)

open Common;;

type type_expr =
    Tvar of type_var
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstr of ident * type_expr list

and type_var =
    { mutable link: type_expr option ; mutable level: int }
;;

type value_info =
    { vi_type: type_expr ;
      vi_access: access }
;;

type type_info =
    { ti_params: type_var list ;
      ti_res: type_expr ;
      ti_kind: type_kind }
and type_kind =
    Kabbrev of type_expr
  | Kvariant of (string * type_expr list) list
  | Krecord of (string * type_expr * access) list
  | Kbasic
;;

type label_info =
    { li_params: type_var list ;
      li_arg: type_expr ;
      li_res: type_expr ;
      li_access: access ;
      li_total: int ;
      li_index: int }
;;

type constr_info =
    { ci_params: type_var list ;
      ci_args: type_expr list ;
      ci_res: type_expr ;
      ci_size: int ;
      ci_tag: int }
;;
