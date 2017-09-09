(* $Id: define.ml,v 1.3 2004/09/24 00:51:15 garrigue Exp $ *)

open Common;;
open Typs;;

(* types *)

let constructors = (Hashtbl.create 149 : (string, constr_info) Hashtbl.t) ;;

let labels = (Hashtbl.create 149 : (string, label_info) Hashtbl.t) ;;

let types = (Hashtbl.create 149 : (string, type_info) Hashtbl.t) ;;

let types_map = ref (IdMap.empty : type_info IdMap.t) ;;

let make_ti id params kind =
  { ti_params = params ;
    ti_res = Tconstr (id, List.map (fun tv -> Tvar tv) params) ;
    ti_kind = kind }
;;

let add_type name info =
  Hashtbl.add types name info;
  begin match info.ti_res with
    Tconstr (id, _) -> types_map := IdMap.add id info !types_map
  |	_ -> failwith "define__add_info"
  end;
  match info.ti_kind with
    Kabbrev _ -> ()
  | Kvariant l ->
      let _ =
	List.fold_left
          (fun (c,nc) (name,ty_args) ->
	    Hashtbl.add constructors name
	      { ci_params = info.ti_params ;
		ci_args = ty_args ;
		ci_res = info.ti_res ;
		ci_size = List.length ty_args ;
		ci_tag = if ty_args = [] then c else nc };
            if ty_args = [] then (succ c, nc) else (c, succ nc))
          (0,0) l
      in ()
  | Krecord l ->
      let total = List.length l in
      let _ =
	List.fold_left
          (fun n (name, ty_arg, access) ->
	    Hashtbl.add labels name
	      { li_params = info.ti_params ;
		li_arg = ty_arg ;
		li_res = info.ti_res ;
		li_access = access ;
		li_total = total ;
		li_index = n };
	    n + 1)
          0 l
      in ()
  | Kbasic -> ()
;;

(* types *)

let values =
  ref (StrMap.empty : value_info StrMap.t) ;;

let global_idents = ref [] ;;

let global_env =
  ref (IdMap.empty : Obj.t ref IdMap.t);;

let add_value id obj info =
  values := StrMap.add id.name info !values;
  global_env := IdMap.add id (ref obj) !global_env;
  global_idents := (id.name, id) :: !global_idents
;;
