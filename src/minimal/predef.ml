(* $Id: predef.ml,v 1.6 2004/09/24 00:51:16 garrigue Exp $ *)

open Misc;;
open Common;;
open Typs;;
open Define;;

(* types *)

let id_int = new_id "int"
and id_char = new_id "char"
and id_float = new_id "float"
and id_array = new_id "array"
and id_bool = new_id "bool"
and id_list = new_id "list"
;;

let basic_types =
  let tv = Unify.newvar () in
  [ "int", make_ti id_int [] Kbasic ;
    "char", make_ti id_char [] Kbasic ;
    "float", make_ti id_float [] Kbasic ;
    "array", make_ti id_array [tv] Kbasic ;
    "bool", make_ti id_bool [] (Kvariant["false", []; "true", []]);
    "list", make_ti id_list [tv]
            (Kvariant [ "[]", [];
			"::", [Tvar tv; Tconstr(id_list,[Tvar tv])] ]);
    "unit", make_ti (new_id "unit") [] (Kabbrev(Ttuple[]));
    "string", make_ti (new_id "string") []
              (Kabbrev(Tconstr(id_array,[Tconstr(id_char,[])])))
  ]
;;

List.iter (fun (name,info) -> add_type name info) basic_types ;;
