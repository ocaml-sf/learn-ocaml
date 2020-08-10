(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   annotations.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   This file defiens type annotations for annotated types and operations on annotations.
 *)

open Core

open Rconfig
open Toolbox
open Rtypes
open Solver
open Indices
open Sharingcoef



(*Type annotations where 'a is the type of variables.*)
type 'a type_anno =
    { tan_type : raml_type
    ; tan_deg : int
    ; tan_map : index -> 'a
    }

type type_stack = raml_type list

(*Type annotations for a context and a list of arguments *)
type 'a context_anno =
    { can_tstack : type_stack
    ; can_context : type_context
    ; can_deg : int
    ; can_map : cindex -> 'a
    }

let tanno_map 
    : ('a -> 'b) -> 'a type_anno -> 'b type_anno =
  fun f anno ->
    { anno with
      tan_map = fun ind -> f (anno.tan_map ind)
    }

(* Look-up an the element associent with an index in a type_anno and context_anno*)
(* This is a great place to find bugs: Always use tan_find and can_find when looking
   up indices.*)
let tan_find anno ind =
  assert (valid_index anno.tan_type ind)
  ; assert (tracked_index ~deg:anno.tan_deg anno.tan_type ind)
  ; anno.tan_map ind

let can_find canno cind =
  assert (valid_cindex canno.can_tstack canno.can_context cind)
  ; assert (tracked_cindex ~deg:canno.can_deg canno.can_tstack canno.can_context cind)
  ; canno.can_map cind

let can_find_zero canno =
  let zero_cind = zero_cindex (canno.can_tstack, canno.can_context) in
  can_find canno zero_cind


(* The actual operations on type/context annotation emmit linear constraints as site effects. *) 
(* As a result, they are encapsulated in a functor that is a function of a solver module that *)
(* accepts the constraints. *)
module Make (Solver: SOLVER) (Amode : AMODE)=
struct
  module S = Solver
  type var = S.var

  exception Anno_exn of string

  (*Primary data structures for index maps*)
  module Mi = Ind_map
  module Mc = Cind_map

  
  (* Create a fresh type annotation.  The optional argument qzero is an annotation for the zero *)
  (* index.  If given, the annotation for the zero_index is not fresh but qzero. *)
  let fresh_tanno ?qzero deg t = 
    let empty = Mi.empty in
    let add_ind =
      match qzero with
	| Some q ->
	  fun ind acc ->
	    let var = 
	      if is_zero_index ind then
		q
	      else
		Solver.fresh_var () 
	    in
	    Mi.set acc ind var
	| None ->
	  fun ind acc -> Mi.set acc ind (Solver.fresh_var ())
    in
    let tanno = indices_max_deg ~empty ~add_ind t deg in
    { tan_type = t
    ; tan_deg = deg
    ; tan_map = Map.find_exn tanno
    }


  (* A fresh context annotation.  See fresh_tanno for more info. *)
  let fresh_canno ?qzero deg ts context =
    let empty = Mc.empty in
    let add_ind =
      match qzero with
	| Some q ->
	  fun cind acc ->
	    let var = 
	      if is_zero_cindex cind then
		q
	      else
		Solver.fresh_var () 
	    in
	    Mc.set acc cind var
	| None ->
	  fun cind acc -> Mc.set acc cind (Solver.fresh_var ())
    in
    let canno = cindices_max_deg ~empty ~add_ind ts context deg in
    { can_context = context
    ; can_tstack = ts
    ; can_deg = deg
    ; can_map = Map.find_exn canno
    }


  (* Emission of different constraints to the solver. *)
  let cost_constr =
    match Amode.mode with
    | Mupper
    | Mconstant ->
      fun q1 q2 cost ->
        Solver.add_constr_list ~lower:cost ~upper:cost [(q1,1.0); (q2,-.1.0)]
    | Mlower ->
      (* This weakening is needed for PASS THROUGH of potential in examples like mergesort. *)
      fun q1 q2 cost ->
        Solver.add_constr_list ~upper:cost [(q1,1.0); (q2,-.1.0)]

  let geq_constr q1 q2  = 
    Solver.add_constr_list ~lower:0.0 [(q1,1.0); (q2,-.1.0)]

  let eq_constr q1 q2  = 
    Solver.add_constr_list ~lower:0.0 ~upper:0.0 [(q1,1.0); (q2,-.1.0)]

  let amode_constr =
    match Amode.mode with
    | Mupper ->
      fun p q -> Solver.add_constr_list ~lower:0.0 [(p,1.0); (q,-.1.0)]
    | Mlower ->
      fun p q -> Solver.add_constr_list ~upper:0.0 [(p,1.0); (q,-.1.0)]
    | Mconstant ->
      fun p q -> Solver.add_constr_list ~lower:0.0 ~upper:0.0 [(p,1.0); (q,-.1.0)]

  let amode_sum_constr =
    match Amode.mode with
    | Mupper ->
      fun ps qs ->
        let constrs_p = List.map ps (fun p -> (p, 1.0)) in
        let constrs_q = List.map qs (fun q -> (q,-.1.0)) in
        Solver.add_constr_list ~lower:0.0 (constrs_p@constrs_q)
    | Mlower ->
      fun ps qs ->
        let constrs_p = List.map ps (fun p -> (p, 1.0)) in
        let constrs_q = List.map qs (fun q -> (q,-.1.0)) in
        Solver.add_constr_list ~upper:0.0 (constrs_p@constrs_q)
    | Mconstant ->
      fun ps qs ->
        let constrs_p = List.map ps (fun p -> (p, 1.0)) in
        let constrs_q = List.map qs (fun q -> (q,-.1.0)) in
        Solver.add_constr_list ~lower:0.0 ~upper:0.0 (constrs_p@constrs_q)

  let eq_sum_coeff_constr p constrs_q =
    Solver.add_constr_list ~lower:0.0 ~upper:0.0 ((p, -1.0)::constrs_q)

  let eq_sum_constr ps qs =
    let constrs_p = List.map ps (fun p -> (p, 1.0)) in
    let constrs_q = List.map qs (fun q -> (q,-.1.0)) in
    Solver.add_constr_list ~lower:0.0 ~upper:0.0 (constrs_p@constrs_q)

  let zero_constr q  = 
    Solver.add_constr_list ~lower:0.0 ~upper:0.0 [(q,1.0)]


  let zero_canno ?qzero deg ts context =
    let zero_var = Solver.fresh_var () in
    let () = zero_constr zero_var in
    let qzero =
      match qzero with
      | Some q -> q
      | None -> zero_var
    in
    let can_map cind =
      if is_zero_cindex cind then
	qzero
      else 
	zero_var
    in
    { can_context = context
    ; can_tstack = ts
    ; can_deg = deg
    ; can_map
    }

  
  let constant_tanno cost targ rtanno =
    let deg = rtanno.tan_deg in
    assert (is_const_type targ);
    let qres_0 = tan_find rtanno (zero_index rtanno.tan_type) in
    let tan_map =
      if cost = 0.0 then
	fun i -> qres_0
      else 
	let qarg_0 = Solver.fresh_var () in
	let _ = cost_constr qarg_0 qres_0 cost in
	fun i -> qarg_0
    in
    { tan_type = targ
    ; tan_deg = deg
    ; tan_map
    }


  let constant_canno ?tstack:(tstack=[]) ?context:(context=String.Map.empty) cost tanno =
    let deg = tanno.tan_deg in
    assert
      begin
	let cinds = 
	  cindices_max_deg ~empty:[] ~add_ind:List.cons 
	    tstack context deg
	in
	match cinds with
	  | [zero] -> true  (*there is only the zero index *)
	  | _ -> false 
      end;
    let qres_0 = tan_find tanno (zero_index tanno.tan_type) in
    let can_map =
      if cost = 0.0 then
	fun i -> qres_0
      else 
	let qarg_0 = Solver.fresh_var () in
	let _ = cost_constr qarg_0 qres_0 cost in
	fun i -> qarg_0
    in
    { can_context = context
    ; can_tstack = tstack
    ; can_deg = deg
    ; can_map
    }


  let add_cost canno cost =
    if cost = 0.0 then
      canno
    else
      let zero_cind = zero_cindex (canno.can_tstack, canno.can_context) in
      let qarg_0 = Solver.fresh_var () in
      let qres_0 = can_find canno zero_cind in
      let _ = cost_constr qarg_0 qres_0 cost in
      let can_map cind =
	if is_zero_cindex cind then
	  qarg_0
        else 
	  can_find canno cind
      in
      {canno with can_map}


  let add_cost_t tanno cost =
    if cost = 0.0 then
      tanno
    else
      let zero_ind = zero_index tanno.tan_type in
      let qarg_0 = Solver.fresh_var () in
      let qres_0 = tan_find tanno zero_ind in
      let _ = cost_constr qarg_0 qres_0 cost in
      let tan_map ind =
	if is_zero_index ind then
	  qarg_0
        else 
	  tan_find tanno ind
      in
      {tanno with tan_map}


  let canno_of_tanno ?xt tanno =
    let deg = tanno.tan_deg in
    let tstack = 
      match tanno.tan_type with
	| Ttuple ts -> ts
	| _ -> raise (Anno_exn "Expecting tuple type.")
    in
    assert (tstack <> []);
    let context = 
      match xt with
	| Some (x,t_x) ->
	  assert (is_const_type t_x);
	  String.Map.singleton x t_x
	| None ->
	  String.Map.empty
    in
    let can_map (is,ci) =
      assert begin
	match xt with
	  | Some (x,_) -> Map.keys ci = [x]
	  | None -> Map.keys ci = []
      end;
      assert (map_all_zero ci);
      tan_find tanno (Ituple is)
    in
    { can_deg = deg
    ; can_context = context
    ; can_tstack = tstack
    ; can_map
    }


  let var_canno rtanno x =
    let deg = rtanno.tan_deg in
    let rtype = rtanno.tan_type in
    let can_map (is,ci) =
      assert (is = []);
      assert (Map.keys ci = [x]);
      let i = Map.find_exn ci x in
      tan_find rtanno i
    in
    { can_context = String.Map.singleton x rtype
    ; can_tstack = []
    ; can_deg = deg
    ; can_map
    }


  let canno_let (tstack, context_out) deg context_in canno_map =
    assert (Map.for_all canno_map (function | Right canno -> canno.can_tstack = [] | Left q -> true) );
    assert (Map.for_all canno_map (function | Right canno -> Map.equal (=) canno.can_context context_in | Left q -> true) );
    let unique ~key =
      function 
	| `Left q1 -> Some q1
	| `Right q2 -> Some q2
	| `Both (_,_) -> raise (Anno_exn "Overlapping contexts in let.")
    in
    let context = Map.merge context_out context_in unique in
    let cmap = 
      let cj_zero =
	Map.map context_in (fun t -> zero_index t)
      in
      let f ~key:(is,ci) ~data:opt_canno cmap =
	match opt_canno with
	  | Right canno ->
	    let deg = canno.can_deg in
	    assert (deg > 0);
	    let add_ind (js,cj) cmap =
	      assert (js = []);
	      let q = can_find canno (js,cj) in
	      let ind = (is, Map.merge ci cj unique) in
	      Map.set cmap ind q
	    in
	    cindices_max_deg ~empty:cmap ~add_ind
	      [] context_in deg
	  | Left q ->
	    assert (cdegree (is,ci) = deg);
	    let ci' = Map.merge ci cj_zero unique in
	    Map.set cmap (is,ci') q
      in
      Map.fold canno_map ~init:Mc.empty ~f
    in
    { can_deg = deg
    ; can_tstack = tstack
    ; can_context = context
    ; can_map = Map.find_exn cmap
    }


  let make_memo_cmap () =
    let memo = ref Mc.empty in
    fun cind ->
      match Mc.find !memo cind with
	| Some var -> var
	| None ->
	  let q = Solver.fresh_var () in
	  memo := Mc.set !memo cind q;
	  q


  let make_memo_tmap () =
    let memo = ref Mi.empty in
    fun cind ->
      match Mi.find !memo cind with
	| Some var -> var
	| None ->
	  let q = Solver.fresh_var () in
	  memo := Mi.set !memo cind q;
	  q


  let atype_make_memo_cmap =
    match Amode.mode with
    | Mupper ->
      fun () -> make_memo_cmap ()
    | Mlower
    | Mconstant ->
      let zero_q = Solver.fresh_var () in
      let () = zero_constr zero_q in
      fun () -> fun ind -> zero_q

  
  let atype_make_memo_tmap =
    match Amode.mode with
    | Mupper ->
      fun () -> make_memo_tmap ()
    | Mlower
    | Mconstant ->
      let zero_q = Solver.fresh_var () in
      let () = zero_constr zero_q in
      fun () -> fun ind -> zero_q


  let stack_variable canno x t_x = 
    let deg = canno.can_deg in
    let (can_context, can_map) =
      match Map.find canno.can_context x with
      | Some _ ->
	let can_map (inds,ci) = 
	  match inds with
	  | i_x::is -> can_find canno (is, Map.set ci x i_x)
	  | _ -> raise (Anno_exn "Dead code.")
	in
	(Map.remove canno.can_context x, can_map)

      | None ->
	let fresh_map = atype_make_memo_cmap () in
	let can_map (inds,ci) = 
 	  match inds with
	  | i_x::is -> 
	    if is_zero_index i_x then
	      can_find canno (is, ci)
	    else
	      fresh_map (inds,ci)
	  | _ -> raise (Anno_exn "Dead code.")
	in
	(canno.can_context, can_map)
    in
    { can_context
    ; can_tstack = t_x::canno.can_tstack
    ; can_deg = deg
    ; can_map
    }


  let unstack_variables canno xs ts =
    assert (canno.can_tstack = ts);
    let deg = canno.can_deg in
    let can_context = Toolbox.map_add_list canno.can_context xs ts in
    let split_imap ci =
      let init = ([],ci) in
      let f x (is,ci) =
	let i_x = Map.find_exn ci x in
	let ci' = Map.remove ci x in
	(i_x::is,ci')
      in
      List.fold_right xs ~f ~init
    in
    let can_map (is,ci) =
      assert (is = []);
      let ind = split_imap ci in
      can_find canno ind
    in
    { can_context
    ; can_tstack = []
    ; can_deg = deg
    ; can_map
    }


  let combine_tannos tan1 tan2 =
    let deg = tan1.tan_deg in
    let t = tan1.tan_type in
    assert (tan2.tan_type = tan1.tan_type);
    assert (tan2.tan_deg = deg);
    let tan = fresh_tanno deg t in
    let add_ind ind () =
      let () = amode_constr (tan_find tan ind) (tan_find tan1 ind) in
      let () = amode_constr (tan_find tan ind) (tan_find tan2 ind) in ()
    in
    indices_max_deg ~empty:() ~add_ind t deg
    ; tan


  let add_tannos ?tan tan1 tan2 =
    let low_deg = tan1.tan_deg in
    let high_deg = tan2.tan_deg in
    assert (low_deg <= high_deg);
    let t = tan1.tan_type in
    assert (t = tan2.tan_type);
    match tan with
      | None -> 
	let tan_map =
	  let add_ind ind tmap =
	    let q = Solver.fresh_var () in
	    let q1 = tan_find tan1 ind in
	    let q2 = tan_find tan2 ind in
	    let () = eq_sum_constr [q] [q1;q2] in
	    Mi.set tmap ind q
	  in
	  let tmap = indices_max_deg ~empty:Mi.empty ~add_ind t low_deg in
	  fun ind -> 
	    if degree ind <= low_deg then
	      Mi.find_exn tmap ind
	    else
	      tan_find tan2 ind
	in
	{ tan2 with tan_map }
	| Some tan -> 
	  assert (tan.tan_deg = tan2.tan_deg);
	  assert (tan.tan_type = t);
	  let add_ind ind () =
	    let q =  tan_find tan ind in
	    let q2 = tan_find tan2 ind in
	    if degree ind <= low_deg then
	      let q1 = tan_find tan1 ind in
	      eq_sum_constr [q] [q1;q2]
	    else
	      eq_sum_constr [q] [q2]	      
	  in
	  let () = indices_max_deg ~empty:() ~add_ind t high_deg in
	  tan


  let bind_arguments canno = 
    let tstack = canno.can_tstack in
    let deg = canno.can_deg in
    let tmap = (* zero-out context *)
      let context = canno.can_context in
      let add_ind (is,ci) tmap = 
	let q = can_find canno (is,ci) in
	if map_all_zero ci then
	  Mi.set tmap (Ituple is) q
	else
	  let _ = zero_constr q in
	  tmap
      in
      cindices_max_deg ~empty:Mi.empty ~add_ind tstack context deg
    in
    { tan_deg = deg
    ; tan_type = Ttuple tstack
    ; tan_map = Mi.find_exn tmap
    }


  let add_const backward = 

    let find_zero tanno =  
      tan_find tanno (zero_index tanno.tan_type)
    in

    let map_new_zero tmap q ind =
      if is_zero_index ind then
	q
      else
	tmap ind
    in

    fun rtanno metric ->
      let q_new = Solver.fresh_var () in
      let q_zero = find_zero rtanno in
      let rtanno' = 
	{ rtanno with
	  tan_map = map_new_zero rtanno.tan_map q_new
	}
      in
      let atanno = backward rtanno' metric in
      let p_new = Solver.fresh_var () in
      let p_zero = find_zero atanno in
      let atanno' = 
	{ atanno with
	  tan_map = map_new_zero atanno.tan_map p_new
        }
      in
      let c = Solver.fresh_var () in
      (* Ex c : p_new >= p_zero + c and q_new + c >= qzero *) 
      let () = amode_sum_constr [p_new] [p_zero;c] in
      let () = amode_sum_constr [q_new;c] [q_zero] in
      atanno'


  let combine_cannos_let canno1 canno2 =
    assert (canno1.can_tstack = []);
    let deg = canno1.can_deg in
    assert (deg = canno2.can_deg);
    let unique ~key =
      function 
	| `Left q1 -> Some q1
	| `Right q2 -> Some q2
	| `Both (_,_) -> raise (Anno_exn "Overlapping contexts in let.")
    in
    let context1 = canno1.can_context in
    let context2 = canno2.can_context in
    let context = Map.merge context1 context2 unique in
    let ci_zero1 =
      Map.map context1 (fun t -> zero_index t)
    in
    let ci_zero2 =
      Map.map context2 (fun t -> zero_index t)
    in
    let is_zero2 =
      List.map canno2.can_tstack (fun t -> zero_index t)
    in
    let can_map =
      let cmap = 
	let add_ind (is,ci) cmap =
	  let q = can_find canno2 (is,ci) in
	  let ci' = Map.merge ci ci_zero1 unique in
	  Map.set cmap (is,ci') q
	in
	cindices_max_deg ~empty:Mc.empty ~add_ind canno2.can_tstack context2 deg
      in
      let cmap =  (* zero_index is overwritten by q_0 in canno1 *)
	let add_ind (is,ci) cmap =
	  assert (is = []);
	  let q = can_find canno1 (is,ci) in
	  let ci' = Map.merge ci ci_zero2 unique in
	  Map.set cmap (is_zero2, ci') q
	in
	cindices_max_deg ~empty:cmap ~add_ind [] context1 deg
      in
      let memo_map = atype_make_memo_cmap () in
      fun ind ->
	match Map.find cmap ind with
	  | Some q -> q
	  | None -> memo_map ind
    in
    { can_context = context
    ; can_tstack = canno2.can_tstack
    ; can_deg = deg
    ; can_map 
    }
	

  (* the additional variable must be of a type with only zero index *)
  let combine_cannos_if ?add_var cans = 
    let (deg, tstack) = 
      match cans with
      | canno1::cannos ->
	let tstack1 = canno1.can_tstack in
	let deg1 = canno1.can_deg in
	assert 
	  begin
	    let f canno =
	      (deg1 = canno.can_deg)
	      && (tstack1 = canno.can_tstack)
	    in
	    List.for_all cannos f
	  end;
	(deg1, tstack1)
      | _ -> raise (Anno_exn "Dead code.")
    in
    let context =
      let resolve ~key:_ = 
	function | `Left t
	         | `Right t -> Some t
	         | `Both (t1,t2) -> 
	           assert (t1 = t2);
	           Some t1
      in
      let f context canno =
	Map.merge context canno.can_context resolve
      in
      let cont = List.fold cans ~init:String.Map.empty ~f in
      match add_var with
      | None -> cont
      | Some (x,t) -> 
	assert (is_const_type t);
	Map.set cont x t
    in
    let missing_zeros = (*zero indices for the missing vars*)
      let filter canno ~key:x ~data:t_x = 
	match Map.find canno.can_context x with
	| Some _ -> None
	| None -> Some (zero_index t_x)
      in
      List.map cans (fun canno -> Map.filter_mapi context (filter canno))
    in
    let final_can_map = make_memo_cmap () in
    let () =
      let resolve ~key:_ = 
	function `Left i -> Some i
	       | `Right i -> Some i
	       | `Both (i1,i2) -> 
	         raise (Anno_exn "Overlapping variables when merging indices.")
      in
      let f canno m_zero = 
	let tstack = canno.can_tstack in
	let context = canno.can_context in
	let add_ind (is,ci) () =
	  let ci' = Map.merge m_zero ci resolve in
	  let p = final_can_map (is,ci') in
	  let q = can_find canno (is,ci) in
	  amode_constr p q
	in
	cindices_max_deg ~empty:() ~add_ind tstack context deg
      in
      List.iter2_exn cans missing_zeros ~f
    in
    { can_context = context
    ; can_tstack = tstack
    ; can_deg = deg
    ; can_map = final_can_map
    }

  
  let nat_shift_t rtanno =
    let deg = rtanno.tan_deg in
    assert (rtanno.tan_type = Tnat);
    let add_ind i cmap =
      match i with
	| Inat n ->
	  let qs =
	    let tmap = tan_find rtanno in
	    if n < deg then
	      [tmap (Inat n); tmap (Inat (n+1))]
	    else
	      [tmap (Inat n)]
	  in
	  let p_ind = Ituple [Inat n] in
	  assert (not (Map.mem cmap p_ind));
	  begin 
	    match qs with
	      | [q] ->
		Map.set cmap p_ind q
	      | _ ->
		let p = Solver.fresh_var () in
		let () = amode_sum_constr [p] qs in
		Map.set cmap p_ind p
	  end
	| _ -> raise (Anno_exn "Dead code.")
    in 
    let cmap = indices_max_deg ~empty:Mi.empty ~add_ind Tnat deg in
    { tan_type = Ttuple [Tnat]
    ; tan_deg = deg
    ; tan_map = Map.find_exn cmap
    }

  let amode_patch_canno =
    match Amode.mode with
    | Mupper ->
      fun ?(unconstraint=false) yts canno -> canno
    | Mlower
    | Mconstant ->
      fun ?(unconstraint=false) yts canno ->
        let other_map =
          if unconstraint then
            let memo_map = make_memo_cmap () in
            fun ind -> memo_map ind
          else
            let q_zero = Solver.fresh_var () in
            let () = zero_constr q_zero in
            fun ind -> q_zero
        in
        let context = canno.can_context in
        let yts_missing =
          List.filter yts (fun (y,t) -> not (Map.mem context y))
        in
        let (ys_missing,ts_missing) = List.unzip yts_missing in
        let can_context =
          map_add_list context ys_missing ts_missing
        in
        let can_map (is,ci) =
          let all_missing_zero =
            let f y =
              let ind_y = Map.find_exn ci y in
              is_zero_index ind_y
            in
            List.for_all ys_missing ~f
          in
          if all_missing_zero then
            let ci' =
              List.fold ~init:ci ~f:Map.remove ys_missing
            in
            can_find canno (is,ci')
          else
            other_map (is,ci)
        in
        {canno with can_context; can_map}

  
  let amode_patch_cannos =
    match Amode.mode with
    | Mupper -> fun cannos bound_vars -> cannos
    | Mlower
    | Mconstant ->
      fun cannos bound_vars ->
        let cans_binds = List.zip_exn cannos bound_vars in
        let resolve ~key:_ = 
	  function | `Left t
	           | `Right t -> Some t
	           | `Both (t1,t2) -> 
	             assert (t1 = t2);
	             Some t1
        in
        let context =
          let f acc_context (canno, bind) =
            let context =
              List.fold ~init:canno.can_context ~f:Map.remove bind
            in
	    Map.merge acc_context context resolve
          in
          List.fold cans_binds ~init:String.Map.empty ~f
        in
        let cannos_missing_vars =
          let f (canno,bound_vars) =
            let missing_context =
              let vars =
                Map.keys (List.fold ~init:canno.can_context ~f:Map.remove bound_vars)
              in
              List.fold ~init:context ~f:Map.remove vars
            in
            (canno, Map.to_alist missing_context)
          in
          List.map cans_binds ~f
        in
        List.map cannos_missing_vars ~f:(fun (canno,m_vars) -> amode_patch_canno m_vars canno)
        

  let nat_shift x_m canno x =
    assert ((x_m = x) || (not (Map.mem canno.can_context x)));
    let canno = amode_patch_canno [(x_m,Tnat)] canno in
    let deg = canno.can_deg in
    let tstack = canno.can_tstack in
    let new_context = 
      let cont = Map.remove canno.can_context x_m in
      Map.set cont x Tnat
    in
    let cmap =
      let shift max_deg (is,ci) cmap =
	match Map.find ci x_m with
	| Some (Inat n) -> 
	  let ci' = Map.remove ci x_m in
	  let q = can_find canno (is,ci) in
	  let ci1 = Map.set ci' x (Inat n) in
	  if max_deg then
	    let () = assert (not (Map.mem cmap (is,ci1))) in
	    Map.set cmap (is,ci1) q
	  else
	    let ci2 = Map.set ci' x (Inat (n+1)) in 
	    let p2 = Map.find_exn cmap (is,ci2) in
	    let p1 = Solver.fresh_var () in
	    let () = eq_sum_constr [p1;p2] [q] in
	    assert (not (Map.mem cmap (is,ci1)));
	    Map.set cmap (is,ci1) p1
	| None -> 
	  let q = can_find canno (is,ci) in
	  let ci1 = Map.set ci x (Inat 0) in
	  if max_deg then
	    let () = assert (not (Map.mem cmap (is,ci1))) in
	    Map.set cmap (is,ci1) q
          else
	    let ci2 = Map.set ci x (Inat 1) in 
	    let p2 = Solver.fresh_var () in
	    let p1 = Solver.fresh_var () in
	    let () = eq_sum_constr [p1;p2] [q] in
	    assert (not (Map.mem cmap (is,ci1)));
	    assert (not (Map.mem cmap (is,ci2)));
	    let cmap = Map.set cmap (is,ci1) p1 in
	    Map.set cmap (is,ci2) p2

	| _ -> raise (Anno_exn "Expecting nat index.")
      in
      let cmap =
	cindices_deg tstack canno.can_context 
	  deg ~init:Mc.empty ~add_ind:(shift true)
      in
      let f n cmap =
	cindices_deg tstack canno.can_context 
	  (deg - n) ~init:cmap ~add_ind:(shift false)
      in
      iterate (1,deg+1) cmap f 
    in
    { can_context = new_context
    ; can_tstack = tstack
    ; can_deg = deg
    ; can_map =
	fun i -> Map.find_exn cmap i
    }



  let append_inds is =
    let f js i =
      match i with
	| Iind is -> is@js
	| _ -> raise (Anno_exn "Wrong argument.")
    in
    List.fold is ~init:[] ~f


(* XXX There's bug a in the following code. 
   It's triggert by examples/power_radio.raml

  let ind_shift =  

    let process_constraints =

      let find_update cinds cmap =
	let f (qs,cmap) cind =
	  match Map.find cmap cind with
	    | None ->
	      let q = Solver.fresh_var () in
	      (q::qs, Map.set cmap cind q)
	    | Some q ->
	      (q::qs, cmap)
	in
	List.fold ~init:([],cmap) ~f cinds
      in

      let rec proc_consts cs cmap =
	match cs with
	  | [] -> cmap
	  | (cinds,q)::cs' ->
	    let cmap = proc_consts cs' cmap in
	    let (ps,cmap) = find_update cinds cmap in
	    let () = geq_sum_constr ps [q] in
	    cmap
      in

      proc_consts

    in

  fun yts canno (cid,t_ind) x ->

    let ys = List.map yts fst in
    assert ((List.mem ys x) || (not (Map.mem canno.can_context x)));
    let deg = canno.can_deg in
    let tstack = canno.can_tstack in
    let new_context = 
      let cont = 
	let init = canno.can_context in
	List.fold ys ~init ~f:Map.remove
      in
      Map.set cont x t_ind
    in
    let cmap =
      let shift =
	let is_valid = valid_cindex ~deg_opt:deg tstack new_context in

	let shift_cont is ci ci' j1 js (cs,cmap) =
  	  let q = can_find canno (is,ci) in
	  let ci1 = Map.set ci' x (Iind ((j1,cid)::js)) in
	  let cinds = 
	    assert ((Map.keys ci1) = (Map.keys new_context));
	    let cinds = 
	      if is_valid (is,ci1) then 
		[(is,ci1)]
	      else 
		[]
	    in
	    if is_zero_index j1 then
	      let ci2 = Map.set ci' x (Iind js) in 
	      if is_valid (is,ci2) then
		(is,ci2)::cinds
	      else
		cinds
	    else
	      cinds
      	  in
	  match cinds with
	    | [cind] ->
	      (cs, Map.set cmap cind q)
	    | _ ->
      	      ((cinds,q)::cs, cmap)
	in

	match yts with
	  | [] -> raise (Anno_exn "Missing constructor argument.")
	  | [(y,t)] ->
	    let deg_cid = constr_degree cid t_ind in
	    let (t1,ts) = 
	      if deg_cid = 0 then
		(t,[])
	      else  (* this wouldn't happen in the current translation *)
		match t with
		  | Ttuple (t1::ts) -> (t1,ts)  
		  | _ -> (t,[])
	    in
	    let zero_t1 = zero_index t1 in
	    fun (is,ci) acc ->
	      let (j1,js) =
		match Map.find ci y with
      		  | None -> (zero_t1,[])
		  | Some Ituple (j1::js) ->
		    begin
		      match ts with
			| [] -> (Ituple (j1::js), [])
			| _  -> (j1, append_inds js)
		    end
		  | Some j1 -> (j1,[])
	      in
	      let ci' = Map.remove ci y in
	      shift_cont is ci ci' j1 js acc

	  | (y1,t1)::yts ->
	    let ys = List.map yts fst in
	    let ys_bound = 
	      List.filter ys (Map.mem canno.can_context)
	    in
	    fun (is,ci) acc ->
	      let (ci',j1,js) = 
		let f (ci,inds) y =
		  match Map.find ci y with
		    | Some (Iind is) -> (Map.remove ci y, is@inds)
		    | Some _ -> raise 
		      (Anno_exn "Matching for multiple variables is only\
					supported for recursive types.")
		    | _ -> raise (Anno_exn "Dead code.")
		in
		let (ci',is) =
		  List.fold ys_bound ~init:(ci,[]) ~f 
		in
		let j1 = 
		  match Map.find ci y1 with
		    | Some j1 -> j1
		    | None -> zero_index t1
		in
		(Map.remove ci' y1, j1, is)
	      in
	      shift_cont is ci ci' j1 js acc
      in
      let (cs,cmap) = 
	cindices_max_deg tstack canno.can_context 
	  deg ~empty:([],Mc.empty) ~add_ind:shift
      in
      process_constraints cs cmap
    in
    let memo_map = make_memo_cmap () in
    { can_context = new_context
    ; can_tstack = tstack
    ; can_deg = deg
    ; can_map =
	fun i ->
	  match Map.find cmap i with
	    | Some q -> q
	    | None -> memo_map i
    }
    *)

  let ind_shift_t cid x tanno =
    let deg = tanno.tan_deg in
    let targ = unfold tanno.tan_type cid in
    let context = String.Map.singleton x targ in
    let is_tracked = tracked_index ~deg:deg tanno.tan_type in
    let cmap = 
      let deg_cid = constr_degree cid tanno.tan_type in
      let add_ind (is,ci) can =
	assert (is = []);
	let (j1,js) = 
	  match Map.find_exn ci x with
	    | Ituple (j1::js) ->
	      if deg_cid = 0 then
		(Ituple (j1::js),[])
	      else
		(j1,append_inds js)
	    | j -> (j,[])
	in
	let ps = 
	  let ps = 
	    let ind =
	      Iind ((j1,cid)::js)
	    in
	    if is_tracked ind then 
	      [tan_find tanno ind]
	    else 
	      []
	  in
	  if is_zero_index j1 && (is_tracked (Iind js)) then
	    (tan_find tanno (Iind js))::ps
	  else
	    ps
	in
	match ps with
	  | [p] ->
	    Mc.set can (is,ci) p
	  | _ ->
	    let q = Solver.fresh_var () in
	    let () = amode_sum_constr [q] ps in
	    Mc.set can (is,ci) q
      in
      cindices_max_deg [] context deg ~empty:Mc.empty ~add_ind
    in
    { can_tstack = []
    ; can_context = context
    ; can_deg = deg
    ; can_map = Mc.find_exn cmap
    }


(***************************
 * Old shift ops for bug fixing
*)

(* Use old ind_shift for now *)

  let ind_shift yts canno (cid,t_ind) x =
    let canno = amode_patch_canno yts canno in
    let ys = List.map yts fst in
    assert ((List.mem ~equal:(=) ys x) || (not (Map.mem canno.can_context x)));
    let deg = canno.can_deg in
    let tstack = canno.can_tstack in
    let new_context = 
      let cont = 
	let init = canno.can_context in
        List.fold ys ~init ~f:Map.remove
      in
      Map.set cont x t_ind
    in
    let new_canno = fresh_canno deg tstack new_context in
    let _ =
      let shift =

	let shift_cont is ci ci' j1 js =
  	  let q = can_find canno (is,ci) in
	  let ci1 = Map.set ci' x (Iind ((j1,cid)::js)) in
	  let ps = 
	    assert ((Map.keys ci1) = (Map.keys new_context));
	    let ps = 
	      if tracked_cindex ~deg:deg tstack new_context (is,ci1) then 
		[can_find new_canno (is,ci1)]
	      else 
		[]
	    in
	    if is_zero_index j1 then
	      let ci2 = Map.set ci' x (Iind js) in 
	      if tracked_cindex ~deg:deg tstack new_context (is,ci2) then
		(can_find new_canno (is,ci2))::ps
	      else
		ps
	    else
	      ps
      	  in
      	  eq_sum_constr ps [q]
	in

	match yts with
	| [] -> raise (Anno_exn "Missing constructor argument.")
	| [(y,t)] ->
	  let deg_cid = constr_degree cid t_ind in
	  let (t1,ts) = 
	    if deg_cid = 0 then
	      (t,[])
	    else  (* this wouldn't happen in the current translation *)
	      match t with
	      | Ttuple (t1::ts) -> (t1,ts)  
	      | _ -> (t,[])
	  in
	  let zero_t1 = zero_index t1 in
	  fun (is,ci) () ->
	    let (j1,js) =
	      match Map.find ci y with
      	      | None -> (zero_t1,[])
	      | Some Ituple (j1::js) ->
		begin
		  match ts with
		  | [] -> (Ituple (j1::js), [])
		  | _  -> (j1, append_inds js)
		end
	      | Some j1 -> (j1,[])
	    in
	    let ci' = Map.remove ci y in
	    shift_cont is ci ci' j1 js

	| (y1,t1)::yts ->
	  let ys = List.map yts fst in
	  let ys_bound = 
	    List.filter ys (Map.mem canno.can_context)
	  in
	  fun (is,ci) () ->
	    let (ci',j1,js) = 
	      let f (ci,inds) y =
		match Map.find ci y with
		| Some (Iind is) -> (Map.remove ci y, is@inds)
		| Some _ -> raise 
		              (Anno_exn "Matching for multiple variables is only\
					 supported for recursive types.")
		| _ -> raise (Anno_exn "Dead code.")
	      in
	      let (ci',is) =
		List.fold ys_bound ~init:(ci,[]) ~f 
	      in
	      let j1 = 
		match Map.find ci y1 with
		| Some j1 -> j1
		| None -> zero_index t1
	      in
	      (Map.remove ci' y1, j1, is)
	    in
	    shift_cont is ci ci' j1 js
      in
      cindices_max_deg tstack canno.can_context 
	deg ~empty:() ~add_ind:shift
    in
    new_canno


(*

  let ind_shift_t cid x tanno =
    let deg = tanno.tan_deg in
    let targ = unfold tanno.tan_type cid in
    let context = String.Map.singleton x targ in
    let cmap = 
      let deg_cid = constr_degree cid tanno.tan_type in
      let add_ind (is,ci) can =
	assert (is = []);
	let q = Solver.fresh_var () in
	let (j1,js) = 
	  match Map.find_exn ci x with
	    | Ituple (j1::js) ->
	      if deg_cid = 0 then
		(Ituple (j1::js),[])
	      else
		(j1,append_inds js)
	    | j -> (j,[])
	in
	let ps = 
	  let ps = 
	    let ind =
	      Iind ((j1,cid)::js)
	    in
	    if valid_index ~deg_opt:deg tanno.tan_type ind then 
	      [tan_find tanno ind]
	    else 
	      []
	  in
	  if is_zero_index j1 && (degree (Iind js) <= deg) then
	    (tan_find tanno (Iind js))::ps
	  else
	    ps
	in
	eq_sum_constr [q] ps;
	Mc.set can (is,ci) q
      in
      cindices_max_deg [] context deg ~empty:Mc.empty ~add_ind
    in
    { can_tstack = []
    ; can_context = context
    ; can_deg = deg
    ; can_map = Mc.find_exn cmap
    }


 end of old shift ops
*************************** *)


  let fold_tuple y canno xts = 
    let (xs,ts) = List.unzip xts in
    assert ((List.mem ~equal:(=) xs y) || (not (Map.mem canno.can_context y)));
    let (new_context, missing_xs) =
      let f (cont, missing ) (x,t_x) = 
	match Map.find cont x with
	  | Some at_x ->
	    (Map.remove cont x, missing)
	  | None ->
	    (cont, Set.add missing x)
      in
      let init = (canno.can_context, String.Set.empty) in
      let (cont, missing ) = List.fold xts ~init ~f in
      (Map.set cont y (Ttuple ts), missing)
    in
    let new_cmap ind =
      let memo_cmap = atype_make_memo_cmap () in
      let rec lookup (is,ci) xs js =
	match xs, js with
	  | [], [] -> can_find canno (is,ci)
	  | x::xs, j::js ->
	    if Set.mem missing_xs x then
	      if degree j > 0 then
		memo_cmap ind
	      else
		lookup (is,ci) xs js 
	    else
	      let ci' = Map.set ci x j in
	      lookup (is,ci') xs js
	  | _ -> raise (Anno_exn "Dead code.")
      in
      let (is,ci) = ind in
      match Map.find ci y with
	| Some (Ituple js) ->
	  let ci' = Map.remove ci y in
	  lookup (is,ci') xs js
	| _ -> raise (Anno_exn "Dead code.")
    in
    { canno with
      can_context = new_context;
      can_map = new_cmap
    }


  let unfold_tuple xs tanno =
    let deg = tanno.tan_deg in
    let context = 
      match tanno.tan_type with
	| Ttuple ts ->
	  String.Map.of_alist_exn (List.zip_exn xs ts)
	| _ -> raise (Anno_exn "Expecting tuple type.")
    in
    let can_map (is,ci) =
      assert (is = []);
      let js = List.map xs (Map.find_exn ci) in
      tan_find tanno (Ituple js)
    in
    { can_context = context
    ; can_tstack = []
    ; can_deg = deg
    ; can_map
    }


  let share y (x1,x2) canno =
    let context = canno.can_context in
    let t_x1 = Map.find_exn context x1 in
    assert (t_x1 = Map.find_exn context x2);
    let new_context =
      let context = Map.remove context x1 in
      let context = Map.remove context x2 in
      Map.set context y t_x1
    in
    let new_cmap =
      match t_x1 with
	| t when is_const_type t ->
	  begin
	    fun (is,ci) ->
	      let ci' = 
		let zero_ind = zero_index t in
		assert (Map.find_exn ci y = zero_ind);
		let ci' = Map.remove ci y in
		let ci' = Map.set ci' x1 zero_ind in
		Map.set ci' x2 zero_ind
	      in
	      can_find canno (is,ci')
	  end
	| Tnat
	| Tarray _
	| Ttuple _
	| Tind _ ->
	  let constr_map =
	    let add_ind (is,ci) con_map =
	      let i_x1 = Map.find_exn ci x1 in
	      let i_x2 = Map.find_exn ci x2 in
	      let sharing_coeffs = sharing_coefficient i_x1 i_x2 in
	      assert (Map.length sharing_coeffs > 0);
	      let q = can_find canno (is,ci) in
	      let ci' =
		let ci = Map.remove ci x1 in
		Map.remove ci x2
	      in
	      let f ~key:ind ~data:coeff cmap =
		let ci' = Map.set ci' y ind in
		Map.add_multi cmap (is,ci') (q, Int.to_float coeff)
	      in
	      Map.fold sharing_coeffs ~init:con_map ~f
	    in
	    cindices_max_deg ~empty:Mc.empty ~add_ind canno.can_tstack context canno.can_deg
    	  in
	  let is_tracked = 
	    let deg = canno.can_deg in
	    let tstack = canno.can_tstack in
	    tracked_cindex ~deg:deg tstack new_context 
	  in
	  let new_cmap =
	    let new_cmap = make_memo_cmap () in
	    fun (is,ci) -> begin
	      assert( is_tracked (is,ci) );
	      new_cmap (is,ci)
	    end
	  in
	  let _ =
	    let q_zero = Solver.fresh_var () in
	    let () = zero_constr q_zero in
	    let f ~key:(is,ci) ~data:coeff_list =
	      let p = 
		if is_tracked (is,ci) then
		  new_cmap (is,ci)
 		else
		  q_zero
	      in
	      eq_sum_coeff_constr p coeff_list
	    in
	    Map.iteri constr_map f
	  in
	  new_cmap
	| _ -> raise (Anno_exn "Dead code.")
    in
    { canno with
      can_context = new_context;
      can_map = new_cmap
    }

  let fresh_zero_tanno ?qzero deg t =
    let q = Solver.fresh_var () in
    let () = zero_constr q in
    let tan_map =
      match qzero with
      | None ->
        fun i -> q
      | Some qzero ->
        fun i ->
          if is_zero_index i then
            qzero
          else
            q
    in
    { tan_type = t
    ; tan_deg = deg
    ; tan_map
    }


  let zero_out_tanno ?(spare_zero=false) tanno = 
    let deg = tanno.tan_deg in
    let t = tanno.tan_type in
    let add_ind =
      if spare_zero then
	fun ind () ->
	  if is_zero_index ind then
	    ()
	  else
	    zero_constr (tan_find tanno ind)
      else
	fun ind () ->
	  zero_constr (tan_find tanno ind)
    in
    indices_max_deg ~empty:() ~add_ind t deg


  let equalize_tannos tanno1 tanno2 =
    let deg = tanno1.tan_deg in
    let typ = tanno1.tan_type in
    assert (deg = tanno2.tan_deg);
    assert (typ = tanno2.tan_type);
    let add_ind ind () =
      let q1 = tan_find tanno1 ind in
      let q2 = tan_find tanno2 ind in
      eq_constr q1 q2
    in
    indices_max_deg ~empty:() ~add_ind typ deg


  let update_context canno xts context =
    let (xs,_) = List.unzip xts in
    assert (List.for_all xs (fun x -> not (Map.mem context x)));
    assert (List.for_all xts (fun (x,t) -> match t with | Tarrow _ -> true | _ -> false));
    let zero_inds_miss =
      let f ~key:y ~data:t_y zero_inds =
	if Map.mem canno.can_context y then
	  zero_inds
	else
	  Map.set zero_inds y (zero_index t_y)
      in
      Map.fold context ~init:String.Map.empty ~f
    in
    let resolve ~key:_ = 
      function `Left at
	     | `Right at -> Some at
	     | `Both (at1,at2) -> raise (Anno_exn "Overlapping contexts.")
    in
    let can_map = 
      let add_ind (is,ci) acc =
	let q = can_find canno (is,ci) in
	let ci' = List.fold xs ~init:ci ~f:Map.remove in
	let ci'' = Map.merge ci' zero_inds_miss resolve in
	Map.set acc (is,ci'') q
      in
      let tstack = canno.can_tstack in
      let deg = canno.can_deg in
      let canno_map' = 
	cindices_max_deg ~empty:Mc.empty ~add_ind tstack 
	  canno.can_context deg
      in
      let atype_memo_map = atype_make_memo_cmap () in
      fun ind ->
	match Map.find canno_map' ind with
        | Some q -> q
	| None -> atype_memo_map ind
    in
    { canno with
      can_context = context;
      can_map
    }

  let arr_make rtanno t_elem linear_cost =
    assert( rtanno.tan_type = Tarray t_elem);
    let atype_memo_map = atype_make_memo_tmap () in
    let q_lin =
      let q' = rtanno.tan_map (Inat 1) in
      let q = Solver.fresh_var () in
      let _ = cost_constr q q' linear_cost in
      q
    in
    let tan_map ind =
      match ind with
      | Ituple [Ituple [i;j]] ->
	 assert begin
	     match i with
	     | Inat _ -> true
	     | _ -> false
	   end;
	 if is_zero_index j then
	   match i with
	   | Inat 1 -> q_lin
	   | _ -> rtanno.tan_map i
	 else
	   atype_memo_map ind
      | _ -> raise (Anno_exn "Wrong index shape.")
    in
    { tan_type = Ttuple [Ttuple [Tnat; t_elem]]
    ; tan_deg = rtanno.tan_deg
    ; tan_map
    }
      
  let arr_length rtanno t_elem =
    assert(rtanno.tan_type = Tnat);
    let tan_map ind =
      match ind with
      | Ituple [i] ->
	 assert begin
	     match i with
	     | Inat _ -> true
	     | _ -> false
	   end;
	 rtanno.tan_map i
      | _ -> raise (Anno_exn "Wrong index shape.")
    in
    { tan_type = Ttuple [Tarray t_elem]
    ; tan_deg = rtanno.tan_deg
    ; tan_map
    }

  let nat_of_int n rtanno =
    assert (rtanno.tan_type = Tnat);
    let deg = rtanno.tan_deg in
    let q = Solver.fresh_var () in
    let ps =
      let coeff i l =
        let p = tan_find rtanno (Inat i) in
        let k = binomial n i in
        (p, Int.to_float k)::l
      in
      iterate (0,deg+1) [] coeff
    in
    let () =
      Solver.add_constr_list ~lower:0.0 ~upper:0.0 ((q,-.1.0)::ps)
    in
    { can_context = String.Map.empty
    ; can_tstack = []
    ; can_deg = deg
    ; can_map = (fun i -> q)
    }

  let nat_add rtanno =
    assert (rtanno.tan_type = Tnat);
    let deg = rtanno.tan_deg in
    let t_arg = Ttuple [Ttuple [Tnat; Tnat]] in
    assert
      begin
        let f = function
          | [n1;n2;n] ->
            (n1 + n2 = n) &&
            (tracked_index ~deg Tnat (Inat n)) &&
            (not (tracked_index ~deg t_arg (Ituple [Ituple [Inat n1; Inat n2]])))
          | _ -> raise (Anno_exn "Dead code.")
        in
        let tups = Toolbox.tuples [deg;deg;deg] in
        (List.filter ~f tups) = []
      end;
    let tan_map i =
      match i with
      | (Ituple [Ituple [Inat n1; Inat n2]]) ->
        let ind = Inat (n1+n2) in
        assert (tracked_index ~deg Tnat ind);
        tan_find rtanno ind
      | _ -> raise (Anno_exn "Wrong index shape.")
    in
    { tan_type = t_arg
    ; tan_deg = deg
    ; tan_map
    }


  let nat_minus rtanno =
    let t_res = Ttuple [Tnat; Tnat] in
    assert (rtanno.tan_type = t_res);
    let deg = rtanno.tan_deg in
    let t_arg = Ttuple [Ttuple [Tnat; Tnat]] in
    let atanno = fresh_tanno deg t_arg in
    let () =
      let add_ind ind_r () =
        match ind_r with
        | Ituple [Inat n1; Inat n2] ->
          let ind_a = Ituple [Ituple [Inat (n1+n2); Inat 0]] in
          assert (tracked_index ~deg t_arg ind_a);
          let q = tan_find atanno ind_a in
          let p = tan_find rtanno ind_r in
          if n1 = 0 && n2 <> 0 then
            let ind_a' = Ituple [Ituple [Inat 0; Inat n2]] in
            let q' = tan_find atanno ind_a' in
            amode_sum_constr [q;q'] [p]
          else
            amode_constr q p
        | _ -> raise (Anno_exn "Wrong index shape.")
      in
      indices_max_deg ~empty:() ~add_ind t_res deg
    in
    let () =
      match Amode.mode with
      | Mupper -> ()
      | Mlower
      | Mconstant ->
        let add_ind ind () =
          match ind with
          | Ituple [Ituple [Inat n1; Inat n2]] ->
            if n1 > 0 && n2 > 0 then
              zero_constr (tan_find atanno ind)
            else
              ()
          | _ -> raise (Anno_exn "Wrong index shape.")
        in
        indices_max_deg ~empty:() ~add_ind t_arg deg
    in
    atanno

  
  let nat_mult rtanno =
    assert (rtanno.tan_type = Tnat);
    let deg = rtanno.tan_deg in
    let t_arg = Ttuple [Ttuple [Tnat; Tnat]] in
    let atanno = fresh_tanno deg t_arg in
    assert
      begin
        let f = function
          | [n1;n2;n] ->
            mult_coef n1 n2 n <> 0 &&
            (tracked_index ~deg Tnat (Inat n)) &&
            (not (tracked_index ~deg t_arg (Ituple [Ituple [Inat n1; Inat n2]])))
          | _ -> raise (Anno_exn "Dead code.")            
        in
        let tups = Toolbox.tuples [deg;deg;deg/2] in
        (List.filter ~f tups) = []
      end;
    let () =
      let add_ind ind () =
        if (degree ind)*2 > deg then
          zero_constr (tan_find rtanno ind)
        else
          ()
      in
      indices_max_deg ~empty:() ~add_ind Tnat deg in
    let () =
      let add_ind ind_a () =
        match ind_a with
        | (Ituple [Ituple [Inat n1; Inat n2]]) ->
          let add_ind' ind_r sum =
            match ind_r with
            | Inat n ->
              let a = mult_coef n1 n2 n in
              let coeff = (tan_find rtanno ind_r, Int.to_float a) in
              coeff :: sum
            | _ -> raise (Anno_exn "Wrong index shape.")
          in
          let p_sum = indices_max_deg ~empty:[] ~add_ind:add_ind' Tnat deg in
          let q = tan_find atanno ind_a in
          eq_sum_coeff_constr q p_sum
        | _ -> raise (Anno_exn "Wrong index shape.")
      in
      indices_max_deg ~empty:() ~add_ind t_arg deg
    in
    atanno

  let nat_div_mod rtanno =
    let t_res = Ttuple [Tnat; Tnat; Tnat] in
    assert (rtanno.tan_type = t_res);
    let deg = rtanno.tan_deg in
    let t_arg = Ttuple [Ttuple [Tnat; Tnat]] in
    let t_temp = Ttuple [Tnat; Tnat] in
    let temp_tanno = fresh_tanno deg t_temp in
    let atanno = fresh_tanno deg t_arg in    
    let () =
      let add_ind ind_r () =
        match ind_r with
        | Ituple [Inat n1; Inat k; Inat n2] ->
          let add_ind' ind_nat sum =
            match ind_nat with
            | Inat n ->
              let a = mult_coef n1 n2 n in
              let ind_t = Ituple [Inat n; Inat k] in
              if a <> 0 then
                let coeff = (tan_find temp_tanno ind_t, Int.to_float a) in
                coeff :: sum
              else
                sum
            | _ -> raise (Anno_exn "Wrong index shape.")
          in
          let p_sum = indices_max_deg ~empty:[] ~add_ind:add_ind' Tnat (deg-k) in
          let p_sum =
            if n1 + k = 0 && n2 > 0 then
              let () = assert (p_sum = []) in
              [(tan_find atanno (Ituple [Ituple [Inat 0; Inat n2]]), 1.0)]
            else
              p_sum
          in
          let q = tan_find rtanno ind_r in
          eq_sum_coeff_constr q p_sum
        | _ -> raise (Anno_exn "Wrong index shape.")
      in
      indices_max_deg ~empty:() ~add_ind t_res deg
    in
    let () =
      let add_ind ind_t () =
        match ind_t with
        | Ituple [Inat n1; Inat n2] ->
          let ind_a = Ituple [Ituple [Inat (n1+n2); Inat 0]] in
          assert (tracked_index ~deg t_arg ind_a);
          let q = tan_find atanno ind_a in
          let p = tan_find temp_tanno ind_t in
          amode_constr q p
        | _ -> raise (Anno_exn "Wrong index shape.")
      in
      indices_max_deg ~empty:() ~add_ind t_temp deg
    in
    let () =
      match Amode.mode with
      | Mupper -> ()
      | Mlower
      | Mconstant ->
        let add_ind ind () =
          match ind with
          | Ituple [Ituple [Inat n1; Inat n2]] ->
            if n1 > 0 && n2 > 0 then
              zero_constr (tan_find atanno ind)
            else
              ()
          | _ -> raise (Anno_exn "Wrong index shape.")
        in
        indices_max_deg ~empty:() ~add_ind t_arg deg
    in
    atanno

  let amode_consume_tanno =
    match Amode.mode with
    | Mconstant      
    | Mupper ->
      fun rtanno targ ->
        let deg = rtanno.tan_deg in
        let tres = rtanno.tan_type in
        let res_qzero = tan_find rtanno (zero_index tres) in
        let qzero = Solver.fresh_var () in
        let () = geq_constr qzero res_qzero in
        fresh_tanno ~qzero deg targ
    | Mlower ->
      fun rtanno t ->
        let deg = rtanno.tan_deg in
        let tanno = fresh_tanno deg t in
        let () = zero_out_tanno tanno in
        tanno
   

  
end
