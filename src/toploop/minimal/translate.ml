(* $Id: translate.ml,v 1.5 2004/09/24 00:51:16 garrigue Exp $ *)

open Misc;;
open Common;;
open Syntax;;
open Typs;;
open Define;;
open Untyped;;

let rec pattern pat =
  match pat.sp_desc with
    SPid s ->
      begin try
	let info = Hashtbl.find constructors s in
	if info.ci_size <> 0 then
	  failwith ("In translate__pattern, " ^ s ^
		    " : need arguments");
	UPconst (Cint info.ci_tag), []
      with Not_found ->
	let id = new_id s in UPid id, [s,id]
      end
  | SPconst c -> UPconst c, []
  | SPtuple [] -> UPconst (Cint 0), []
  | SPtuple l ->
      let ul, bnds = List.split (List.map pattern l) in
      UPblock (0, ul), List.flatten bnds
  | SParray l ->
      let ul, bnds = List.split (List.map pattern l) in
      UPblock (0, ul), List.flatten bnds
  | SPconstr (s,pat) ->
      begin try
	let info = Hashtbl.find constructors s in
	if info.ci_size = 0 then
	  failwith ("In translate__pattern, " ^ s ^
		    " : cannot be applied")
	else if info.ci_size = 1 then
	  let upat, bnds = pattern pat in
	  UPblock (info.ci_tag, [upat]), bnds
	else match pat.sp_desc with
	  SPtuple l when List.length l = info.ci_size ->
	    let upatl, bnds = List.split (List.map pattern l) in
	    UPblock (info.ci_tag, upatl), List.flatten bnds
	| _ ->
	    failwith
	    ("In translate__pattern, " ^ s ^ " : wrong number of arguments")
      with
	Not_found ->
	  failwith ("In translate__pattern, " ^ s ^ " : unkwnown constructor")
      end
  | SPrecord l ->
      let info = Hashtbl.find labels (fst (List.hd l)) in
      let arr = Array.create info.li_total UPany in
      let bnds = List.map
        (fun (s, p1) ->
	  let up1, bnds = pattern p1
	  and info = Hashtbl.find labels s in
	  arr.(info.li_index) <- up1;
	  bnds)
        l in
      (UPblock (0, Array.to_list arr), List.flatten bnds)
  | SPany -> UPany, []
  | SPtype (pat, _) -> pattern pat
;;

let current_def = ref "";;

let rec command bnds cmd =
  match cmd.sc_desc with
    SEexpr e ->
      [UEexpr (expression bnds e)], bnds
  | SEval l ->
      let patl, el = List.split l in
      let uel = List.map (expression bnds) el
      and upatl, new_bnds = List.split (List.map pattern patl) in
      List.map2 (fun pat (upat, ue) -> UEval (upat, ue, !current_def))
	   patl (List.combine upatl uel),
      List.flatten new_bnds @ bnds
  | SEfun l ->
      let sl, el = List.split l in
      let ids = List.map new_id sl in
      let bnds = List.combine sl ids @ bnds in
      let uel = List.map2
 	(fun s e ->
	  let old = !current_def in
	  if old = "" then current_def := s;
 	  let ue = expression bnds e in
	  current_def := old; ue)
 	sl el in
      [UEfun (List.combine ids uel)], bnds
  | SEvar l ->
      let sl, el = List.split l in
      let ids = List.map new_id sl
      and uel = List.map (expression bnds) el in
      let bnds = List.combine sl ids @ bnds in
      [UEfun (List.combine ids uel)], bnds
  | STtype _ | SThide _ ->
      failwith "translate__command"

and expression bnds se =
  match se.se_desc with
    SEid s ->
      begin try
	let info = Hashtbl.find constructors s in
	if info.ci_size <> 0 then
	  failwith ("In translate__expression, " ^ s ^
		    " : need arguments");
	UEconst (Cint info.ci_tag)
      with Not_found -> try
	UEid (List.assoc s bnds)
      with Not_found ->
	failwith ("In translate__expression : unbound identifier " ^ s)
      end
  | SEconst c -> UEconst c
  | SEtuple [] -> UEconst (Cint 0)
  | SEtuple l -> UEblock (0, List.map (expression bnds) l)
  | SEarray l -> UEblock (0, List.map (expression bnds) l)
  | SErecord l ->
      let arr = Array.create (List.length l) (UEconst(Cint 0)) in
      List.iter
        (fun (s,e1) ->
	  let ue1 = expression bnds e1
	  and info = Hashtbl.find labels s in
	  arr.(info.li_index) <- ue1)
        l;
      UEblock (0, Array.to_list arr)
  | SEapply ({se_desc = SEid s} as e0, [e]) ->
      begin try
	let info = Hashtbl.find constructors s in
	if info.ci_size = 0 then
	  failwith ("In translate__expression, " ^ s ^
		    " : cannot be applied")
	else if info.ci_size = 1 then
	  UEblock (info.ci_tag, [expression bnds e])
	else begin match e.se_desc with
	  SEtuple l when List.length l = info.ci_size ->
	    let uel = List.map (expression bnds) l in
	    UEblock (info.ci_tag, uel)
	| _ ->
	    failwith ("In translate__expression, " ^ s ^
		      " : wrong number of arguments")
	end
      with Not_found ->
	UEapply (expression bnds e0, [expression bnds e])
      end
  | SEapply (e, l) ->
      UEapply (expression bnds e, List.map (expression bnds) l)
  | SEfunct (patl, e) ->
      let upatl, new_bnds = List.split (List.map pattern patl) in
      let ue = expression (List.flatten new_bnds @ bnds) e in
      let make_match pat = function
	  UPid id -> id, []
	| upat ->
	    let id = new_id "param" in
	    (id, [UEval (upat, UEid id, !current_def)])
      in
      let ids, vals = List.split (List.map2 make_match patl upatl) in
      UEfunct (ids, UEseq (List.flatten vals, ue))
  | SEseq l ->
      let cmds, last = split_last l in
      let ucmds, bnds =
	List.fold_left
	  (fun (ucmds, bnds) cmd ->
	    let ucmd, bnds = command bnds cmd in
	    (ucmds @ ucmd, bnds))
	  ([], bnds) cmds in
      begin match last.sc_desc with
	SEexpr e -> UEseq (ucmds, expression bnds e)
      |	_ ->
	  let ucmd, _ = command bnds last in
	  UEseq (ucmds @ ucmd, UEconst (Cint 0))
      end
  | SEcase (e, cases) ->
      let ue = expression bnds e in
      let ucases = List.map
	(fun (pat, e) ->
	  let upat, new_bnds = pattern pat in
	  (upat, expression (new_bnds @ bnds) e))
	cases in
      UEcase (ue, ucases, !current_def)
  | SEifthenelse (e1, e2, e3) ->
      UEifthenelse (expression bnds e1, expression bnds e2, expression bnds e3)
  | SEset (s, e) ->
      begin try
	UEset (List.assoc s bnds, expression bnds e)
      with Not_found ->
	failwith ("In translate__expression : unbound identifier " ^ s)
      end
  | SEgetfield (e1, s) ->
      let ue1 = expression bnds e1
      and info = Hashtbl.find labels s in
      UEapply (UEid (List.assoc "$." bnds), [ue1;UEconst(Cint info.li_index)])
  | SEsetfield (e1, s, e2) ->
      let ue1 = expression bnds e1
      and ue2 = expression bnds e2
      and info = Hashtbl.find labels s in
      UEapply (UEid (List.assoc "$.<-" bnds),
               [ue1;UEconst(Cint info.li_index);ue2])
  | SEfor (s, e1, dir, e2, e3) ->
      let ue1 = expression bnds e1
      and ue2 = expression bnds e2 in
      let id = new_id s in
      UEfor (id, ue1, dir, ue2, expression ((s,id)::bnds) e3)
  | SEwhile (e1, e2) ->
      UEwhile (expression bnds e1, expression bnds e2)
  | SEtype (e, _) ->
      expression bnds e
;;
