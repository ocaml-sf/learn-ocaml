(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   shareletnormal.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   RAML expressions in share-let-normal form.
 *)

open Core
open Rtypes
open Expressions


let normalize_pattern_matches 
    : typed_expression -> typed_expression =

  let mk_evar x typ kind info =
    { exp_desc = Evar x
    ; exp_type = typ
    ; exp_kind = kind
    ; exp_info = info
    }
  in

  let mk_nat_exp x kind info =
    let e_succ = 
      { exp_desc = Ebase_fun Nat_succ
      ; exp_type = Tarrow ([Tnat],Tnat,())
      ; exp_kind = Efree
      ; exp_info = info
      }
    in
    let e_var = mk_evar x Tnat kind info in
    { exp_desc = Eapp (None, e_succ, [e_var])
    ; exp_type = Tnat
    ; exp_kind = Efree
    ; exp_info = info
    }
  in


  let mk_zero_exp x kind info =
    let e_zero = 
      { exp_desc = Ebase_const Czero
      ; exp_type = Tnat
      ; exp_kind = Efree
      ; exp_info = info
      }
    in
    let e_var = mk_evar x Tnat kind info in
    { exp_desc = Elet (Some (x,Tnat), e_zero, e_var)
    ; exp_type = Tnat
    ; exp_kind = Efree
    ; exp_info = info
    }
  in
  
  let mk_constr_exp c_id bs c_typ kind info =
    match bs with
      | [] -> raise (Emalformed "Empty variable list in pattern")
      | [(x,t)] ->
	{ exp_desc = Econst (c_id, mk_evar x t kind info)
	; exp_type = c_typ
	; exp_kind = Efree
	; exp_info = info
	}
      | (x,t)::bs' ->
	let (xs,ts) = List.unzip bs' in
	let evar = mk_evar x t kind info in
	let evars = List.map2_exn xs ts (fun x t -> mk_evar x t Efree info) in
        let etup = 
	  { exp_desc = Etuple (evar::evars)
	  ; exp_type = Ttuple (t::ts)
	  ; exp_kind = Efree
	  ; exp_info = info
	  }
	in
	{ exp_desc = Econst (c_id,etup)
	; exp_type = c_typ
	; exp_kind = Efree
	; exp_info = info
	}
  in

  let transformer exp =
    match exp.exp_desc with
      | Ematch (e,matches) ->  (* c_id,xs,e *)
	( match e.exp_desc with
	  | Evar y -> 
	    let update_match (c_id,xs,e) =
	      if List.mem ~equal:(=) (fst (List.unzip xs)) y then
		(c_id,xs,e)
	      else
		let e' = substitute e y (mk_constr_exp c_id xs) in
		(c_id,xs,e')
	    in
	    let matches' = List.map matches update_match in
	    { exp with exp_desc = 
		Ematch (e,matches')
	    }
	  | _ -> exp
	)

      | Enat_match (e,e1,x,e2) -> 
	( match e.exp_desc with
	  | Evar y when not ((fst x) = y) ->
	    { exp with exp_desc = 
                let e1' = substitute e1 y (fun t -> mk_zero_exp "##temp##") in
		let e2' = substitute e2 y (fun t -> mk_nat_exp (fst x)) in
		Enat_match (e,e1',x,e2')
	    }
	  | _ -> exp
	)

      | desc -> exp
  in

  e_transform_outside_in transformer



let normalize_nats
  : typed_expression -> typed_expression =

  let transformer exp =
    match exp.exp_desc with
    | Eapp (_
           , { exp_desc = Ebase_fun Nat_of_int }
           , [ {exp_desc = Ebase_const (Cint n) } ]
           )
      ->
      { exp with
        exp_desc =
          Ebase_fun (Nat_of_intc n)
      }
    | desc -> exp
  in

  e_transform transformer



let var_factory 
    : string -> (unit -> var_id) * (int -> var_id list) =
  fun prefix ->
    let counter = ref 0 in

    let fresh_var _ =
      counter := !counter + 1;
      prefix ^ (string_of_int !counter)
    in

    let rec fresh_vars = function
      | n when n <= 0 -> []
      | n -> fresh_var ()::fresh_vars (n-1)
    in
    
    (fresh_var,fresh_vars)







let share_let_normal
    : string -> typed_expression -> sln_expression =
  fun prefix ->
    let (fresh_var,fresh_vars) = var_factory prefix in

    let rec free_lets xs es exp =
      match xs,es with
	| [],[] -> exp
        | [],_  -> raise (Emalformed "")
        | _,[]  -> raise (Emalformed "")
	| x::xs,e::es ->
	  let  exp' = free_lets xs es exp in
	  { exp_desc = Elet (Some (x,e.exp_type),e,exp')
	  ; exp_type = exp'.exp_type
	  ; exp_kind = Efree
	  ; exp_info = Sln
	  }
    in

    let share_var (x,t_x) e1 es_binds cont =
      let nv1 = x^(fresh_var ()) in
      let nv2 = x^(fresh_var ()) in
      let e1' = subst_var e1 x nv1 in
      let subst_bind (e,vars) = 
	if Set.mem (Vb_set.of_list vars) (x,t_x) then
	  (e,vars)
        else
	  (subst_var e x nv2,vars)
      in
      let es_b' = List.map es_binds subst_bind in
      let e_x = 
	{ exp_desc = Evar x
	; exp_type = t_x
	; exp_kind = Efree
	; exp_info = Sln
	}
      in
      let e_cont = cont e1' es_b' in
      { exp_desc = Eshare (e_x,(nv1,t_x),(nv2,t_x),e_cont)
      ; exp_type = e_cont.exp_type
      ; exp_kind = Efree
      ; exp_info = Sln
      }
    in

    let share_vars e1 es_binds cont =
      let es_var_list = List.map es_binds (fun (e,vs) -> Set.diff (free_vars e) (Vb_set.of_list vs)  ) in
      let es_vars = List.fold es_var_list ~init:Vb_set.empty ~f:Set.union in
      let xts = Set.to_list(Set.inter (free_vars e1) es_vars) in
      let rec svars xts e1 es_binds =
	match xts with
	  | [] -> 
	    let (es,_) = List.unzip es_binds in
	    cont e1 es
	  | xt::xts -> 
	    share_var xt e1 es_binds (fun e1' es_b' -> svars xts e1' es_b')
      in
      svars xts e1 es_binds
    in


    let share_vars1 e1 e_b2 cont =
      share_vars e1 [e_b2] (
	fun e1' es' ->
	  match es' with
	    | [e2'] -> cont e1' e2'
	    | _ -> raise (Emalformed "Dead code!")
      )
    in	


    let share_vars2 e e_b1 e_b2 cont =
      share_vars e [e_b1;e_b2] (
	fun e' es' ->
	  match es' with
	    | [e1';e2'] -> cont e' e1' e2'
	    | _ -> raise (Emalformed "Dead code!")
      )
    in	



    let rec share_vars_tuple es nvars_set cont = 
      match es with
	| [] -> cont []
	| e::es -> 
	  let es_bind = List.map es (fun e -> (e,nvars_set) ) in
	  share_vars e es_bind 
	    (fun e' es' -> share_vars_tuple es' nvars_set 	
	      (fun es'' -> cont (e'::es'')))
    in

    let mk_var nv t =
      { exp_desc = Evar nv
      ; exp_type = t
      ; exp_kind = Efree
      ; exp_info = Sln
      }	
    in

    let rec norm exp =
      match exp.exp_desc with
	| Ebase_const c -> 
	  { exp with exp_desc = Ebase_const c
	    ; exp_info = Sln 
	  }
	| Ebase_fun base_fun -> 
	  { exp with exp_desc = Ebase_fun base_fun
	    ; exp_info = Sln
	  }
	| Ebase_op op  -> 
	  { exp with exp_desc = Ebase_op op
	    ; exp_info = Sln 
	  }

	| Evar v -> 
	  { exp with exp_desc = Evar v
            ; exp_info = Sln 
	  }

	| Eapp (cname,e,es) ->
	  let (en,ens) = (norm e, List.map es norm) in
	  let nvs = fresh_vars (List.length ens) in
	  let nvars = List.map2_exn nvs (List.map (ens) get_type) mk_var in
	  share_vars_tuple (en::ens) []
	    (fun exns ->
	      match exns with
		| [] -> raise (Emalformed "Dead code!")
		| en'::ens' ->
		free_lets (List.rev nvs) (List.rev ens')
		  { exp with exp_desc = Eapp (cname, en', nvars)
		    ; exp_info = Sln
		  }
	    )

	| Elambda (x,e) -> 
	  let en = norm e in
	  { exp with exp_desc = Elambda (x, en)
	    ; exp_info = Sln
	  }

	| Elet (x_opt,e1,e2) -> 
	  let (en1,en2) = (norm e1, norm e2) in
	  let binds = 
	    match x_opt with
	      | None -> []
	      | Some x -> [x]
	  in
	  share_vars1 en1 (en2,binds) (
	    fun e1' e2' ->
	      { exp with exp_desc = Elet (x_opt, e1', e2')
		; exp_info = Sln
	      }
	  )
	    
	| Eletrec (xs, es, e) -> 
	  let ens = List.map (e::es) norm in
	  share_vars_tuple ens xs (
	    fun ens' ->
	      match ens' with
		| [] -> raise (Emalformed "Dead code!")
		| en'::ens' ->
		  { exp with exp_desc = Eletrec (xs, ens', en')
		    ; exp_info = Sln
		  }
	  )


	| Econd (e,e1,e2) -> 
	  let (en, en1, en2) = (norm e, norm e1, norm e2) in
	  let nv = fresh_var () in
	  share_vars2 en (en1,[]) (en2,[]) (
	    fun en' en1' en2' -> 
	      let exp' = 
		{ exp with exp_desc = Econd (mk_var nv (Tbase Tbool), en1', en2')
		  ; exp_info = Sln
		}
	      in
	      free_lets [nv] [en'] exp'
	  )

	| Eshare (e1,x1,x2,e2) -> 
	  let (en1, en2) = (norm e1, norm e2) in
	  let nv = fresh_var () in
	  share_vars1 en1 (en2,[x1;x2]) (
	    fun en1' en2' -> 
	      let exp' = 
		{ exp with exp_desc = Eshare (mk_var nv (get_type e1), x1, x2, en2')
		  ; exp_info = Sln
		}
	      in
	      free_lets [nv] [en1'] exp'
	  )

	| Econst (c,e) -> 
	  let en = norm e in
	  let nv = fresh_var () in
	  free_lets [nv] [en]
	  { exp with exp_desc = Econst (c, mk_var nv (get_type e))
	    ; exp_info = Sln
	  }


	| Ematch (e,matches) ->
	  let (cs,xss,es) = Toolbox.unzip3 matches in
	  let (en, ens) = (norm e, List.map es norm) in
	  let nv = fresh_var () in
	  share_vars en (List.zip_exn ens xss) (
	    fun en' ens' -> 
	      let exp' = 
		{ exp with exp_desc = Ematch (mk_var nv (get_type e), Toolbox.zip3_exn cs xss ens')
		  ; exp_info = Sln
		}
	      in
	      free_lets [nv] [en'] exp'
	  )

	| Enat_match (e,e1,x,e2) -> 
	  let (en, en1, en2) = (norm e, norm e1, norm e2) in
	  let nv = fresh_var () in
	  share_vars2 en (en1,[]) (en2,[x]) (
	    fun en' en1' en2' -> 
	      let exp' = 
		{ exp with exp_desc = Enat_match (mk_var nv (get_type e), en1', x, en2')
		  ; exp_info = Sln
		}
	      in
	      free_lets [nv] [en'] exp'
	  )

	| Eref e -> 
	  let en = norm e in
	  let nv = fresh_var () in
	  free_lets [nv] [en]
	  { exp with exp_desc = Eref (mk_var nv (get_type e))
	    ; exp_info = Sln
	  }

	| Eref_deref e -> 
	  let en = norm e in
	  let nv = fresh_var () in
	  free_lets [nv] [en]
	  { exp with exp_desc = Eref_deref (mk_var nv (get_type e))
	    ; exp_info = Sln
	  }

	| Eref_assign (e1,e2) -> 
	  let (en1, en2) = (norm e1, norm e2) in
	  let (nv1, nv2) = (fresh_var (), fresh_var ()) in
	  share_vars1 en1 (en2,[]) (
	    fun en1' en2' -> 
	      let exp' = 
		{ exp with exp_desc = 
		    Eref_assign (mk_var nv1 (get_type e1), mk_var nv2 (get_type e2))
		  ; exp_info = Sln
		}
	      in
	      free_lets [nv2;nv1] [en2';en1'] exp'
	  )

	| Etuple es -> 
	  let ens = List.map es norm in
	  let nvs = fresh_vars (List.length ens) in
	  let nvars = List.map2_exn nvs (List.map ens get_type) mk_var in
	  let exp' =
	    { exp with exp_desc = Etuple nvars
	      ; exp_info = Sln
	    }
	  in
	  share_vars_tuple ens [] 
	    (fun ens' -> free_lets nvs ens' exp')

	| Etuple_match (e1,xs,e2) -> 
	  let (en1, en2) = (norm e1, norm e2) in
	  let nv = fresh_var () in
	  share_vars1 en1 (en2,xs) (
	    fun en1' en2' -> 
	      let exp' = 
		{ exp with exp_desc = Etuple_match (mk_var nv (get_type e1), xs, en2')
		  ; exp_info = Sln
		}
	      in
	      free_lets [nv] [en1'] exp'
	  )


	| Eundefined -> 
	  { exp with exp_desc = Eundefined
	    ; exp_info = Sln
	  }

	| Etick n ->
	  { exp with exp_desc = Etick n
	    ; exp_info = Sln
	  }
    in

    fun e ->
      let normalize = Fn.compose normalize_pattern_matches normalize_nats in
      norm (normalize e)


