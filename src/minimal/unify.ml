(* $Id: unify.ml,v 1.6 2004/09/24 00:51:17 garrigue Exp $ *)

open Common;;
open Typs;;
open Define;;

exception Unify;;

let current_level = ref 0
and global_level = ref 1
and generic_level = -1
;;

let begin_def () = incr current_level
and end_def () = decr current_level
;;

let reset_global_level () = global_level := !current_level + 1
and new_global_var () = {link = None; level = !global_level}
;;

let newvar () = {link = None; level = !current_level} ;;

let map_type f = function
    Tvar _ as ty -> ty
  | Tarrow (ty1, ty2) -> Tarrow (f ty1, f ty2)
  | Ttuple tyl -> Ttuple (List.map f tyl)
  | Tconstr (id, tyl) -> Tconstr (id, List.map f tyl)
;;

let do_type f = function
    Tvar _ -> ()
  | Tarrow (ty1, ty2) -> f ty1; f ty2
  | Ttuple tyl -> List.iter f tyl
  | Tconstr (_, tyl) -> List.iter f tyl
;;

let rec repr = function
    Tvar ({link = Some ty} as tv) ->
      let ty' = repr ty in
      if ty' != ty then tv.link <- Some ty';
      ty'
  | ty -> ty
;;

let rec generalize ty =
  match repr ty with
    Tvar tv ->
      if tv.level > !current_level then tv.level <- generic_level
  | ty ->
      do_type generalize ty
;;

let rec make_nongen ty =
  match repr ty with
    Tvar tv ->
      if tv.level > !current_level then tv.level <- !current_level
  | ty ->
      do_type make_nongen ty
;;

let rec subst s ty =
  match repr ty with
    Tvar tv as ty ->
      begin try	List.assq tv s with Not_found -> ty end
  | ty ->
      map_type (subst s) ty
;;

let rec occur tv ty =
  match repr ty with
    Tvar tv' ->
      if tv == tv' then raise Unify;
      if tv'.level > tv.level then tv'.level <- tv.level
  | ty -> do_type (occur tv) ty
;;

let expand = function
    Tconstr(id,tyl) ->
      let info = IdMap.find id !types_map in
      begin match info.ti_kind with
	Kabbrev ty ->
	  subst (List.combine info.ti_params tyl) ty
      |	_ ->
	  raise Not_found
      end
  | _ -> raise Not_found
;;

let rec unify ty1 ty2 =
  let ty1 = repr ty1 and ty2 = repr ty2 in
  if ty1 == ty2 then () else
  match (ty1,ty2) with
    Tvar tv1, Tvar tv2 ->
      if tv1.level > tv2.level then
	tv1.level <- tv2.level
      else
	tv2.level <- tv1.level;
      tv1.link <- Some ty2
  | Tvar tv, _ ->
      occur tv ty2; tv.link <- Some ty2
  | _, Tvar tv ->
      occur tv ty1; tv.link <- Some ty1
  | Tarrow (t1,t2), Tarrow (u1,u2) ->
      unify t1 u1; unify t2 u2
  | Ttuple tl1, Ttuple tl2 ->
      if List.length tl1 <> List.length tl2 then raise Unify;
      List.iter2 unify tl1 tl2
  | Tconstr (id1,tl1), Tconstr (id2,tl2) when same_id id1 id2 ->
      List.iter2 unify tl1 tl2
  | _ ->
      try
	unify (expand ty1) ty2
      with Not_found -> try
	unify ty1 (expand ty2)
      with Not_found ->
	raise Unify
;;

let filter_arrow ty =
  match repr ty with
    Tarrow (ty1, ty2) -> ty1, ty2
  | ty ->
      let ty1 = Tvar(newvar()) and ty2 = Tvar(newvar()) in
      unify ty (Tarrow (ty1, ty2));
      ty1, ty2
;;
