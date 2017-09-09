(* $Id: common.ml,v 1.5 2004/09/24 00:51:15 garrigue Exp $ *)

open Misc;;

type 'a option =
    None
  | Some of 'a
;;

type constant =
    Cint of int
  | Cchar of wchar
  | Cfloat of float
;;

type dirflag = Upto | Downto
and access = Mutable | Immutable | Forbidden
;;

type ident = { name: string ; index: int} ;;

let id_count = ref 0;;
let new_id name = incr id_count; {name = name; index = !id_count};;

let same_id id1 id2 = id1.index = id2.index
and compare_id id1 id2 = id1.index - id2.index
;;

module IdMap = Map.Make(struct type t = ident let compare = compare_id end)
module StrMap = Map.Make(struct type t = string let compare = compare end)
