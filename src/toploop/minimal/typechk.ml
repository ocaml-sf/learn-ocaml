(* $Id: typechk.ml,v 1.13 2004/09/24 00:51:16 garrigue Exp $ *)

open Misc;;
open Common;;
open Syntax;;
open Typs;;
open Define;;
open Unify;;
open Predef;;
open Printer;;

type error =
    Message of string
  | Type_mismatch of string * type_expr * type_expr
  | Multiple_occurence of string * string
;;

exception Type_error of error * location ;;

let error_message loc s = raise (Type_error (Message s, loc)) ;;

let rec all_differ loc kind defkind = function
    [] -> ()
  | s :: l ->
      if List.mem s l then
	raise (Type_error
	       (Multiple_occurence
		((if kind = "" then s else kind ^ " " ^ s),
	         if defkind = "" then "definition"
		 else defkind ^ " definition"),
		loc))
      else all_differ loc kind defkind l
;;

let type_vars = ref ([] : (string * type_expr) list) ;;
let reset_type_vars () =
  type_vars := [];
  reset_global_level ()
;;

let rec type_expr def sty =
  match sty.st_desc with
    STvar s ->
      begin try
	List.assoc s !type_vars
      with Not_found ->
	if def then
	  error_message sty.st_loc ("unbound type variable '" ^ s);
	let ty = Tvar (new_global_var ()) in
	type_vars := (s,ty) :: !type_vars;
	ty
      end
  | STarrow (st1, st2) ->
      Tarrow (type_expr def st1, type_expr def st2)
  | STtuple stl ->
      Ttuple (List.map (type_expr def) stl)
  | STconstr (s, stl) ->
      begin try
	let info = Hashtbl.find types s in
	if List.length stl <> List.length info.ti_params then
	  error_message sty.st_loc ("wrong arity for type " ^ s);
	subst (List.combine info.ti_params (List.map (type_expr def) stl))
          info.ti_res
      with Not_found ->
	error_message sty.st_loc ("undefined type constructor " ^ s)
      end
;;

let new_constrs = ref [] and new_labels = ref [] ;;

let type_kind names vars kind =
  type_vars := List.combine names (List.map (fun tv -> Tvar tv) vars);
  match kind with
    SKabbrev sty ->
      Kabbrev (type_expr true sty)
  | SKvariant cl ->
      new_constrs := List.map fst cl @ !new_constrs;
      Kvariant (List.map
		(fun (s, stl) -> (s, List.map (type_expr true) stl))
		cl)
  | SKrecord fl ->
      new_labels := List.map (fun (s,_,_) -> s) fl @ !new_labels;
      Krecord (List.map
	       (fun (s, sty, access) -> (s, type_expr true sty, access))
	       fl)
;;

let add_typedef loc dl =
  all_differ loc "type" "type" (List.map (fun td -> td.sd_name) dl);
  (* add dummy types *)
  let idents =
    List.map
      (fun td ->
	all_differ td.sd_loc "type parameter" "type" td.sd_params;
	let params = List.map (fun _ -> newvar ()) td.sd_params
	and id = new_id td.sd_name in
	Hashtbl.add types td.sd_name (make_ti id params Kbasic);
	id)
      dl in
  (* remove dummy types and add real types *)
  new_constrs := []; new_labels := [];
  let orig_map = !types_map in
  try
    List.iter
      (fun td ->
	let info = Hashtbl.find types td.sd_name in
	let kind = type_kind td.sd_params info.ti_params td.sd_kind in
	Hashtbl.remove types td.sd_name;
	try
	  add_type td.sd_name { ti_params = info.ti_params ;
				ti_res = info.ti_res ;
				ti_kind = kind }
	with exn ->
	  Hashtbl.add types td.sd_name info;
	  raise exn)
      dl;
    all_differ loc "label" "type" !new_labels;
    all_differ loc "constructor" "type" !new_constrs;
    ()
  with exn ->
      types_map := orig_map;
      List.iter (fun td -> Hashtbl.remove types td.sd_name) dl;
      raise exn
;;

let hide_type loc s =
  try
    let info = Hashtbl.find types s in
    let id =
      match info.ti_res with
	Tconstr (id,_) -> id
      |	_ -> failwith "typechk__hyde_type"
    in
    begin match info.ti_kind with
      Kbasic -> error_message loc ("type " ^ s ^ " is already abstract")
    | Kabbrev _ -> ()
    | Kvariant cl ->
	if List.exists
	  (fun (s,_) -> (Hashtbl.find constructors s).ci_res != info.ti_res)
	  cl
	then error_message loc
	  "cannot hide type, some constructors were redefined";
	List.iter (fun (s,_) -> Hashtbl.remove constructors s) cl
    | Krecord fl ->
	if List.exists
	  (fun (s,_,_) -> (Hashtbl.find labels s).li_res != info.ti_res)
	  fl
	then error_message loc
	  "cannot hide type, some labels were redefined";
	List.iter (fun (s,_,_) -> Hashtbl.remove labels s) fl
    end;
    Hashtbl.remove types s;
    let info = { ti_params = info.ti_params;
		 ti_res = info.ti_res;
		 ti_kind = Kbasic } in
    Hashtbl.add types s info;
    types_map := IdMap.add id info !types_map
  with Not_found ->
    error_message loc ("type " ^ s ^ " is not defined")
;;

let newvar () = Tvar (newvar ()) ;;

let instanciate_scheme ty =
  let vars = ref [] in
  let rec inst ty =
    match repr ty with
      Tvar tv when tv.level = generic_level ->
	begin try
	  List.assq tv !vars
	with Not_found ->
	  let ty = newvar () in
	  vars := (tv,ty) :: !vars;
	  ty
	end
    | ty -> map_type inst ty
  in inst ty
;;

let instanciate_type info =
  subst (List.map (fun tv -> tv, newvar()) info.ti_params) info.ti_res
and instanciate_constr info =
  let s = List.map (fun tv -> tv, newvar()) info.ci_params in
  List.map (subst s) info.ci_args, subst s info.ci_res
and instanciate_label info =
  let s = List.map (fun tv -> tv, newvar()) info.li_params in
  subst s info.li_arg, subst s info.li_res
;;

let constant c =
  let name = match c with
    Cint _ -> "int"
  | Cchar _ -> "char"
  | Cfloat _ -> "float"
  in
  instanciate_type (List.assoc name basic_types)
;;

let unify_pat pat ty1 ty2 =
  try unify ty1 ty2
  with Unify ->
    raise (Type_error (Type_mismatch ("Pattern", ty1, ty2), pat.sp_loc))
;;

let rec pattern pat =
  match pat.sp_desc with
    SPid s ->
      begin try
	let info = Hashtbl.find constructors s in
	if info.ci_args <> [] then
	  error_message pat.sp_loc
	    ("constructor " ^ s ^ " requires an argument");
	let _, ty_res = instanciate_constr info in
	ty_res, []
      with Not_found ->
	let ty = newvar () in
	ty, [s, { vi_type = ty ; vi_access = Immutable }]
      end
  | SPconst c -> constant c, []
  | SPtuple l ->
      let tyl, bnds = pattern_list pat.sp_loc l in
      Ttuple tyl, bnds
  | SParray l ->
      let tyl, bnds = pattern_list pat.sp_loc l in
      let tv = newvar () in
      List.iter2 (fun pat ty -> unify_pat pat ty tv) l tyl;
      Tconstr(id_array, [tv]), bnds
  | SPconstr (s, spat) ->
      begin try
	let info = Hashtbl.find constructors s in
	let ty_args, ty_res = instanciate_constr info in
	match ty_args with
	  [] -> error_message pat.sp_loc
	    ("constructor " ^ s ^ " takes no argument")
	| [ty_arg] ->
	    let ty_pat, bnds = pattern spat in
	    unify_pat spat ty_pat ty_arg;
	    ty_res, bnds
	| _ ->
	    match spat.sp_desc with
	      SPtuple l when List.length l = List.length ty_args ->
		let ty_pats, bnds = pattern_list pat.sp_loc l in
		do_list3 unify_pat l ty_pats ty_args;
		ty_res, bnds
	    | _ ->
		error_message pat.sp_loc
		("wrong number of arguments for constructor " ^ s)
      with Not_found ->
	error_message pat.sp_loc ("undefined constructor " ^ s)
      end
  | SPrecord l ->
      let labl, patl = List.split l in
      let ty_args, bnds = pattern_list pat.sp_loc patl in
      let ty_fields, ty_recs =
	List.split (List.map
	       (fun lab ->
		 try
		   let info = Hashtbl.find labels lab in
		   instanciate_label info
		 with Not_found ->
		   error_message pat.sp_loc ("undefined label " ^ lab))
	       labl)
      and ty_res = newvar () in
      List.iter (unify_pat pat ty_res) ty_recs;
      do_list3 unify_pat patl ty_args ty_fields;
      (ty_res, bnds)
  | SPany -> newvar (), []
  | SPtype (pat, sty) ->
      let ty_pat, bnds = pattern pat
      and ty_res = type_expr false sty in
      unify_pat pat ty_pat ty_res;
      (ty_res, bnds)
	
and pattern_list loc patl =
  let tyl,bnds = List.split (List.map pattern patl) in
  let bnds =
    List.fold_right
      (fun bnd bnds ->
	List.iter 
	  (fun (s,_) ->
	    if List.mem_assoc s bnds then
	      raise (Type_error
		     (Multiple_occurence ("variable " ^ s, "pattern"), loc)))
	  bnd;
	bnd @ bnds)
      bnds []
  in tyl, bnds
;;

let is_constructor e =
  match e.se_desc with
    SEid s ->
      begin
 	try let _ = Hashtbl.find constructors s in true
      	with Not_found -> false
      end
  | _ -> false
;;

let rec is_nonexpansive e =
  match e.se_desc with
    SEid _ | SEconst _ -> true
  | SEtuple l -> List.for_all is_nonexpansive l
  | SEarray l -> false
  | SErecord l ->
      let labl, el = List.split l in
      List.for_all (fun lab -> (Hashtbl.find labels lab).li_access <> Mutable)
      	      labl
      & List.for_all is_nonexpansive el
  | SEapply (e, [e1]) when is_constructor e -> is_nonexpansive e1
  | SEapply _ -> false
  | SEfunct _ -> true
  | SEseq l -> List.for_all cmd_nonexpansive l
  | SEcase (e, cases) ->
      is_nonexpansive e & List.for_all is_nonexpansive (List.map snd cases)
  | SEifthenelse (e1,e2,e3) -> List.for_all is_nonexpansive [e1;e2;e3]
  | SEset _ | SEsetfield _ -> false
  | SEgetfield (e, _) -> is_nonexpansive e
  | SEfor (_, e1, _, e2, e3) -> List.for_all is_nonexpansive [e1;e2;e3]
  | SEwhile (e1, e2) -> is_nonexpansive e1 & is_nonexpansive e2
  | SEtype (e, _) -> is_nonexpansive e

and cmd_nonexpansive cmd =
  match cmd.sc_desc with
    SEexpr e -> is_nonexpansive e
  | SEval l -> List.for_all is_nonexpansive (List.map snd l)
  | SEfun _ -> true
  | SEvar l -> List.for_all is_nonexpansive (List.map snd l)
  | STtype _ | SThide _ -> failwith "typecheck__cmp_nonexpansive"
;;

let unify_exp exp ty1 ty2 =
  try unify ty1 ty2
  with Unify ->
    raise (Type_error (Type_mismatch ("Expression", ty1, ty2), exp.se_loc))
;;

let rec expression bnds e =
  match e.se_desc with
    SEid s when is_constructor e ->
      let info = Hashtbl.find constructors s in
      if info.ci_args <> [] then
	error_message e.se_loc
	  ("wrong number of arguments for constructor " ^ s);
      let _, ty_res = instanciate_constr info in
      ty_res
  | SEid s ->
      begin try
	let info = StrMap.find s bnds in
	if info.vi_access = Forbidden then
	  error_message e.se_loc (s ^ " cannot be accessed here");
	instanciate_scheme info.vi_type
      with Not_found ->
	error_message e.se_loc ("unbound identifier " ^ s)
      end
  | SEconst c -> constant c
  | SEtuple l ->
      Ttuple (List.map (expression bnds) l)
  | SEarray el ->
      let tv = newvar () in
      List.iter (type_expect bnds tv) el;
      Tconstr(id_array, [tv])
  | SErecord l ->
      let labl, el = List.split l in
      let ty_fields, ty_rec =
	List.split (List.map
	       (fun lab ->
		 try
		   let info = Hashtbl.find labels lab in
		   instanciate_label info
		 with Not_found ->
		   error_message e.se_loc ("undefined label " ^ lab))
	       labl)
      and ty_res = newvar () in
      if List.length labl <> (Hashtbl.find labels (List.hd labl)).li_total then
	error_message e.se_loc "some labels are missing";
      List.iter (unify_exp e ty_res) ty_rec;
      List.iter2 (type_expect bnds) ty_fields el;
      ty_res
  | SEapply ({se_desc=SEid s} as e1, el) when is_constructor e1 ->
      let info = Hashtbl.find constructors s in
      let ty_args, ty_res = instanciate_constr info in
      begin match ty_args, el with
	[],_ ->
	  error_message e1.se_loc ("constructor " ^ s ^ " takes no argument")
      | [ty_arg], [e] ->
	  type_expect bnds ty_arg e;
	  ty_res
      | _, [{se_desc = SEtuple el}]
	  when List.length el = List.length ty_args ->
	    List.iter2 (type_expect bnds) ty_args el;
	    ty_res
      | _ ->
	  error_message e1.se_loc
	    ("wrong number of arguments for constructor " ^ s)
      end
  | SEapply ({se_desc = SEid ("+"|"-"|"*"|"/"|"~" as op)} as e1, el)
    when el <> [] & (List.length el = 1 or op <> "~") & List.length el <= 2 ->
      let ty_args = List.map (expression bnds) el in
      let float = Tconstr(id_float,[])
      and int = Tconstr(id_int,[]) in
      let ty_res =
      	if List.exists (fun ty -> repr ty = float) ty_args then begin
	  e1.se_desc <- SEid (op ^ "."); float
	end else int
      in
      List.iter2 (fun e ty -> unify_exp e ty ty_res) el ty_args;
      if op = "~"  or List.length el = 2 then
 	ty_res
      else
	Tarrow (ty_res, ty_res)
  | SEapply (e1, el) ->
      let ty1 = expression bnds e1 in
      let _ =
	try filter_arrow ty1 with Unify ->
	  error_message e1.se_loc "this expression is not a function"
      in
      let ty_args, ty_res =
	try List.fold_left
	  (fun (ty_args, ty) _ ->
	    let ty_arg, ty_res = filter_arrow ty in
	    (ty_arg :: ty_args, ty_res))
	  ([],ty1) el
	with Unify ->
	  error_message e1.se_loc "too many arguments for this function"
      in
      List.iter2 (type_expect bnds) (List.rev ty_args) el;
      ty_res
  | SEfunct (patl, e1) ->
      let _, {sp_loc = {last = last}} = split_last patl in
      let ty_args, new_bnds =
	pattern_list {first = (List.hd patl).sp_loc.first ; last = last}
		     patl in
      let bnds = ref
          (List.fold_left (fun b (k,d) -> StrMap.add k d b) bnds new_bnds) in
      StrMap.iter
        (fun s info ->
	  if info.vi_access = Mutable then
	    bnds := StrMap.add s
		    { vi_type = info.vi_type; vi_access = Forbidden }
		    !bnds)
        !bnds;
      let ty_res = expression !bnds e1 in
      List.fold_right (fun ty1 ty2 -> Tarrow (ty1, ty2)) ty_args ty_res
  | SEseq cl ->
      let cl', res = split_last cl in
      begin match res.sc_desc with
	SEexpr e1 ->
	  let bnds = List.fold_left command bnds cl' in
	  expression bnds e1
      |	_ ->
	  let _ = List.fold_left command bnds cl
	  in Ttuple []
      end
  | SEcase (e1, cases) ->
      let ty_arg = expression bnds e1
      and ty_res = newvar () in
      List.iter
        (fun (pat, e) ->
	  let ty_pat, new_bnds = pattern pat in
	  unify_pat pat ty_pat ty_arg;
	  let bnds =
            List.fold_left (fun b (k,d) -> StrMap.add k d b) bnds new_bnds in
	  type_expect bnds ty_res e)
        cases;
      ty_res
  | SEifthenelse (e1, e2, e3) ->
      type_expect bnds (List.assoc "bool" basic_types).ti_res e1;
      let ty_res = expression bnds e3 in
      type_expect bnds ty_res e2;
      ty_res
  | SEset (s, e1) ->
      begin try
	let info = StrMap.find s bnds in
	if info.vi_access = Immutable then
	  error_message e.se_loc (s ^ " is not a mutable variable");
	if info.vi_access = Forbidden then
	  error_message e.se_loc (s ^ " cannot be accessed here");
	type_expect bnds info.vi_type e1;
	Ttuple[]
      with Not_found ->
	error_message e.se_loc ("unbound identifier " ^ s)
      end
  | SEgetfield (e1, s) ->
      begin try
	let info = Hashtbl.find labels s in
	let ty_arg, ty_rec = instanciate_label info in
	type_expect bnds ty_rec e1;
	ty_arg
      with Not_found ->
	error_message e.se_loc ("undefined label " ^ s)
      end
  | SEsetfield (e1, s, e2) ->
      begin try
	let info = Hashtbl.find labels s in
	if info.li_access <> Mutable then
	  error_message e.se_loc("field " ^ s ^ " is not mutable");
	let ty_arg, ty_res = instanciate_label info in
	type_expect bnds ty_res e1;
	type_expect bnds ty_arg e2;
	Ttuple[]
      with Not_found ->
	error_message e.se_loc ("undefined label " ^ s)
      end
  | SEfor (s, e1, _, e2, e3) ->
      let type_int = (List.assoc "int" basic_types).ti_res in
      type_expect bnds type_int e1;
      type_expect bnds type_int e2;
      let bnds =
	StrMap.add s { vi_type = type_int ; vi_access = Immutable } bnds in
      statement bnds e3;
      Ttuple[]
  | SEwhile (e1, e2) ->
      type_expect bnds (List.assoc "bool" basic_types).ti_res e1;
      statement bnds e2;
      Ttuple[]
  | SEtype (e1, sty) ->
      let ty = type_expr false sty in
      type_expect bnds ty e1;
      ty

and type_expect bnds ty_expected e =
  match ty_expected, e.se_desc with
    Ttuple tyl, SEtuple el ->
      List.iter2 (type_expect bnds) tyl el
  |  _ ->
      let ty = expression bnds e in
      unify_exp e ty ty_expected

and statement bnds e =
  let ty = expression bnds e in
  begin match repr ty with
    Tarrow _ ->
      Printf.eprintf
        "> Char %d-%d : Beware, this function is partially applied.\n"
        e.se_loc.first e.se_loc.last;
      flush stderr
  | _ -> ()
  end; ty

and command bnds cmd = 
  let new_bnds =
    match cmd.sc_desc with
      SEexpr e -> let _ = statement bnds e in []
    | SEval l ->
	let patl, el = List.split l in
	begin_def ();
	let ty_patl, new_bnds = pattern_list cmd.sc_loc patl in
	List.iter2 (type_expect bnds) ty_patl el;
	end_def ();
	List.iter2
          (fun ty_pat e -> if not (is_nonexpansive e) then make_nongen ty_pat)
          ty_patl el;
	List.iter generalize ty_patl;
	new_bnds
    | SEfun l ->
	let names, el = List.split l in
	all_differ cmd.sc_loc "" "function" names;
	begin_def ();
	let tmp_bnds =
	  List.map (fun s -> s,{vi_type = newvar(); vi_access = Immutable})
            names in
	let bnds =
          List.fold_left (fun b (k,d) -> StrMap.add k d b) bnds tmp_bnds in
	let new_bnds =
	  List.map2
	    (fun (s,info) e ->
	      let ty = expression bnds e in
	      unify_exp e ty info.vi_type;
	      s,{vi_type = ty; vi_access = Immutable})
	    tmp_bnds el
	in
	end_def ();
	List.iter (fun (_,info) -> generalize info.vi_type) new_bnds;
	new_bnds
    | SEvar l ->
	let names = List.map fst l in
	all_differ cmd.sc_loc "" "variable" names;
	List.map
	  (fun (s, e) -> s,{vi_type = expression bnds e; vi_access = Mutable})
	  l
    | STtype _ ->
	error_message cmd.sc_loc "cannot define a type inside an expression"
    | SThide _ ->
	error_message cmd.sc_loc "cannot hide a type inside an expression"
  in
  List.fold_left (fun b (k,d) -> StrMap.add k d b) bnds new_bnds
;;

let type_command values cmd =
  reset_type_vars ();
  command values cmd

and type_expression values e =
  reset_type_vars ();
  begin_def ();
  let ty = expression values e in
  end_def ();
  if is_nonexpansive e then generalize ty;
  ty
;;

open Format;;

let report_error = function
    Message s -> print_string s
  | Type_mismatch (kind, ty1,ty2) ->
      Printer.reset_vars ();
      printf "@ @[<hov 2>%s has type@ %a@ where@ %a was expected@]"
        (String.capitalize kind)
        (fun _ -> print_type) ty1 (fun _ -> print_type) ty2
  | Multiple_occurence (defkind, s) ->
      printf "@ @[<hov 2>%s occurs multiply in@ %s@]"
        (String.capitalize defkind) s
;;
