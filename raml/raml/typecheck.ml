(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   typecheck.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Type checking simply-typed expressions.
 *)


open Core

open Expressions
open Rtypes


exception Type_error of string


module T = struct
  type t = raml_type [@@deriving sexp, compare]
end

module Type_set = Set.Make(T)

let print_newline () = Out_channel.newline stdout

let unify_types t1 t2 exp = 
  if t1 = t2 then
    ()
  else 
    begin
      print_string "Type Error: Mismatch, when trying to match\n\n"
      ; Pprint.print_raml_type t1
      ; print_newline ()
      ; print_string "with\n\n"
      ; Pprint.print_raml_type t2
      ; print_newline ()
      ; print_string "in expression\n\n"
      ; Pprint.print_expression exp
      ; print_newline ()
      ; raise (Type_error "Type mismatch.")
    end

let rec get_fun_types ts = 
  match ts with
    | [] -> []
    | [t] -> []
    | [t1;t2] -> [Tarrow ([t1], t2, ())]
    | t1::ts -> 
      let fts = get_fun_types ts in
      let fts1 = List.map fts 
	(fun t -> Tarrow ([t1],t,()))
      in 
      let fts2 = List.map fts 
	(fun t -> match t with
	  | Tarrow (targ,tres,()) -> Tarrow (t1::targ,tres,())
	  | _ -> failwith "Dead code"
	)
      in 
      fts1@fts2

let check_linear es =
  let clin ((e1,xs),(e2,ys)) =
    let fvars1 = Set.diff (free_vars e1) (Vb_set.of_list xs) in
    let fvars2 = Set.diff (free_vars e2) (Vb_set.of_list ys) in
    match Set.to_list (Set.inter fvars1 fvars2) with
      | [] -> ()
      | cvars -> (
	print_string "Type Error: Linearity violation.  Variable(s) \n\n   "
 	; Pprint.print_list_sep cvars (fun (x,t) -> print_string x) ", "
	; print_string "\n\nappear in expression\n\n   "
	; Pprint.print_expression e1
	; print_string "\n\nand expression\n\n   "
	; Pprint.print_expression e2
	; print_newline ()
	; print_newline ()
	; raise (Type_error "Linearity violation.")
      )
  in
  List.iter (Toolbox.pairs es) clin


let apply exp t ts =
  match t with
    | Tarrow (targ,tres,()) ->
      if (List.length targ) <> (List.length ts) then
	raise (Type_error "Wrong number of arguments.")
      else
	List.iter (List.zip_exn targ ts) (fun (a,b) -> unify_types a b exp)
      ; tres
    | _ -> raise (Type_error "Type mismatch: expecting a function type.")


let typecheck ?linear:(linear=false) exp constr_map =

  let lookup_ind_type = Map.find_exn constr_map in

  let check_linear es = 
    if linear then
      check_linear es
    else
      ()
  in

  let apply = apply exp in

  let rec tcheck context exp =

    let (==) t1 t2 = unify_types t1 t2 exp in

    let t_exp = exp.exp_type in

    let check_fun te ts = 
      let fts = get_fun_types ts in
      if List.mem ~equal:(=) fts te then
	()
      else begin
	print_string "\n\nType Error: Mismatch, when trying to match\n\n   "
	; Pprint.print_raml_type te
	; print_string "\n\nwith a function type such as\n\n   "
	; Pprint.print_raml_type (List.hd_exn fts)
	; print_string "\n\nin expression\n\n   "
	; Pprint.print_expression exp
	; Format.print_newline ()
	; Format.print_newline ()
	; raise (Type_error "Type mismatch.")
      end
    in

    match exp.exp_desc with
      | Ebase_const c -> (
        match c with
          | Cint _ -> t_exp == Tbase Tint
          | Cfloat _ -> t_exp == Tbase Tfloat
          | Cbool _ -> t_exp == Tbase Tbool
          | Czero -> t_exp == Tnat
          | Cunit -> if t_exp <> Tbase Tghost_unit then t_exp == Tbase Tunit
      )

      | Ebase_fun f -> (
	match f with
	  | Nat_succ -> check_fun t_exp [Tnat;Tnat]
	  | Nat_to_int -> check_fun t_exp [Tnat; Tbase Tint]
	  | Nat_of_int -> check_fun t_exp [Tbase Tint; Tnat]
	  | Nat_of_intc _ -> t_exp == Tnat
	  | Nat_add
	  | Nat_mult -> check_fun t_exp [Ttuple [Tnat; Tnat]; Tnat]
	  | Nat_minusc _ -> check_fun t_exp [Tnat; Ttuple [Tnat; Tnat]]
          | Nat_minus -> check_fun t_exp [Ttuple [Tnat; Tnat]; Ttuple [Tnat; Tnat]]
	  | Nat_div_mod -> check_fun t_exp [Ttuple [Tnat; Tnat]; Ttuple [Tnat; Tnat; Tnat]]

	  | Arr_make -> (
	    match t_exp with
	      | Tarrow ([Ttuple [Tnat; t1]], Tarray t2, ()) when t1 = t2 -> ()
	      | _ -> raise (Type_error "Type mismatch (Array.make).")
	  )
	  | Arr_set -> (
	    match t_exp with
	      | Tarrow ([Ttuple [Tarray t1; Tnat; t2]], Tbase Tunit, ()) when t1 = t2 -> ()
	      | _ -> raise (Type_error "Type mismatch (Array.set).")
	  )
	  | Arr_get -> (
	    match t_exp with
	      | Tarrow ([Ttuple [Tarray t1; Tnat]], t2, ()) when t1 = t2 -> ()
	      | _ -> raise (Type_error "Type mismatch (Array.get).")
	  )

	  | Arr_length -> (
	    match t_exp with
	      | Tarrow ([Tarray t1], Tnat, ())  -> ()
	      | _ -> raise (Type_error "Type mismatch (Array.length).")
	  )

	  | Ref_swap -> (
	    match t_exp with
	      | Tarrow ([Ttuple [Tref t1; t2]], t3, ()) when t1 = t2 && t1 = t3 -> ()
	      | _ -> raise (Type_error "Type mismatch (Raml.ref_swap).")
	  )

 	  | Res_consume _ -> (
	    match t_exp with
	      | Tarrow ([t1], Tbase Tunit, ()) -> ()
	      | _ -> raise (Type_error "Type mismatch (Raml.consume).")
	  )

      )

      | Ebase_op op -> (
	match op with
	  | Un_not -> check_fun t_exp [Tbase Tbool; Tbase Tbool]
	  | Un_iminus -> check_fun t_exp [Tbase Tint; Tbase Tint]
	  | Un_fminus -> check_fun t_exp [Tbase Tfloat; Tbase Tfloat]
	  | Bin_iadd
	  | Bin_isub
	  | Bin_imult
	  | Bin_imod
	  | Bin_idiv -> check_fun t_exp [Ttuple [Tbase Tint; Tbase Tint]; Tbase Tint]
	  | Bin_fadd
	  | Bin_fsub
	  | Bin_fmult
	  | Bin_fdiv -> check_fun t_exp [Ttuple [Tbase Tfloat; Tbase Tfloat]; Tbase Tfloat]
	  | Bin_and
	  | Bin_or -> check_fun t_exp [Ttuple [Tbase Tbool; Tbase Tbool]; Tbase Tbool]
	  | Bin_eq -> (
	    match t_exp with
	      | Tarrow ([Ttuple [t1; t2]], Tbase Tbool, ()) when t1 = t2 -> begin
		match t1 with
		  | Tbase _ | Tnat -> ()
		  | _ -> raise (Type_error "Equality is only supported for base types")
	        end
	      | _ -> raise (Type_error "Type mismatch (equality).")
	  )
	  | Bin_iless_eq
	  | Bin_igreater_eq
	  | Bin_iless
	  | Bin_igreater -> (
	    match t_exp with
	      | Tarrow ([Ttuple [t1; t2]], Tbase Tbool, ()) when t1 = t2 -> begin
		match t1 with
		  | Tbase Tint | Tnat -> ()
		  | _ -> raise (Type_error "Integer comparision is only supported for integers and nats.")
	        end
	      | _ -> raise (Type_error "Type mismatch (equality).")
	  )
	  | Bin_fless_eq
	  | Bin_fgreater_eq
	  | Bin_fless
	  | Bin_fgreater -> check_fun t_exp [Ttuple [Tbase Tfloat; Tbase Tfloat]; Tbase Tbool]
      )

      | Evar vid -> (
	match Map.find context vid with
	  | Some t -> t_exp == t
	  | None -> raise (Type_error "Unbound variable.")
      )

      | Eapp (cname,e,es) ->
	tcheck context e
	; List.iter es (tcheck context)
	; t_exp == (apply (get_type e) (List.map es (get_type)))
	; check_linear (List.map (e::es) (fun x -> (x,[])))

      | Elambda ((x,t_x),e) ->
	tcheck (Map.set context x t_x) e
	; let t_body = e.exp_type in
	  let mismatch = Type_error "Function type mismatch" in
	  ( match t_body with
	    | Tarrow (targ_body, tret_body, ()) -> (
	      match t_exp with
		| Tarrow (targ_exp, tret_exp, ()) ->
		  if tret_exp = t_body then
		    match targ_exp with
		      | [t] -> t == t_x
		      | _ -> raise mismatch
		  else if targ_exp = (t_x::targ_body) then
		    tret_exp == tret_body
		  else 
		    raise mismatch
		      | _ -> raise (Type_error "Non-function type for lambda expression.")
	    )
	    | _ -> t_exp == (Tarrow ([t_x], t_body, ()))
	  )

      | Elet (x_opt,e1,e2) -> 
	let _ = tcheck context e1 in
	let context2 = 
	  match x_opt with
	    | Some (x,t) -> 
	      t == e1.exp_type
	      ; check_linear [e1,[]; e2,[(x,t)]]
	      ; Map.set context x t
	    | None -> 
	      check_linear [e1,[]; e2,[]]
	      ; context
	in
	tcheck context2 e2 
	; t_exp == (e2.exp_type)


      | Eletrec (xts, es, e) ->
	if not (List.for_all es is_lambda) then
	  raise (Type_error "Expecting lambda bindings in letrec.");
	let (xs,ts) = List.unzip xts in
	let context' = Toolbox.map_add_list context xs ts in
	List.iter (List.zip_exn es ts) 
	  (fun (e,t) -> (e.exp_type == t); tcheck context' e)
	; tcheck context' e
	; t_exp == (e.exp_type)
	; check_linear (List.map (e::es) (fun x -> (x,xts)))

      | Econd (e,e1,e2) ->
	tcheck context e
	; e.exp_type == (Tbase Tbool)
	; tcheck context e1
	; tcheck context e2
	; e1.exp_type == e2.exp_type
	; e1.exp_type == t_exp
	; check_linear [e,[] ; e1,[]]
	; check_linear [e,[] ; e2,[]]

      | Eshare (e1,(x1,t_x1),(x2,t_x2),e2) -> 
	tcheck context e1
	; ( let context' = Toolbox.map_add_list context [x1;x2] [t_x1;t_x2] in
	    tcheck context' e2
	)
	; t_x1 == t_x2
	; t_x1 == e1.exp_type
	; t_exp == e2.exp_type
	; check_linear [e1,[] ; e2,[(x1,t_x1); (x2,t_x2)]]

      | Econst (cid,e) ->
	let t_ind = lookup_ind_type cid in
	tcheck context e
	; e.exp_type == (Rtypes.unfold t_ind cid)
	; t_exp == t_ind

      | Ematch (e1,matches) -> 
	let t_e1 = e1.exp_type in
	let check_match (cid, xts, e) =
	  t_e1 == (lookup_ind_type cid)
	  ; ( let t_cid = Rtypes.unfold t_e1 cid in
	      ( match xts with
		| [] -> raise (Emalformed "Empty pattern matching.")
		| [(x,t)] -> 
		  t == t_cid
		| xts -> (match t_cid with
		    | Ttuple ts -> List.iter (List.zip_exn ts (List.map xts snd)) (fun (a,b) -> a == b)
		    | _ -> raise (Type_error "Expectin tupel type when matching multiple variables")
		)
	      )
	  )
	  ; let (xs, ts) = List.unzip xts in
	    tcheck (Toolbox.map_add_list context xs ts) e
	    ; t_exp == e.exp_type
	in
	tcheck context e1
	; ( match t_e1 with
	  | Tind constrs -> List.iter matches check_match
	  | _ -> raise (Type_error "Pattern matching on a non-inductive type")
	)
	; List.iter matches ( fun (_, xts, e) ->
	  check_linear [(e1,[]); (e,xts)]
	)
	  
	  
      | Enat_match (e,e1,(x,t),e2) ->
	tcheck context e
	; e.exp_type == Tnat
	; tcheck context e1
	; e1.exp_type == t_exp
	; tcheck (Map.set context x t) e2
	; e2.exp_type == t_exp
	; check_linear [e,[] ; e1,[] ]
	; check_linear [e,[] ; e2,[x,t] ]

      | Eref e -> 
	tcheck context e
	; t_exp == (Tref e.exp_type)

      | Eref_deref e -> 
	tcheck context e
	; (Tref t_exp) == e.exp_type

      | Eref_assign (e1,e2) ->
	tcheck context e1
	; tcheck context e2
	; (Tref e2.exp_type) == e1.exp_type
	; t_exp == (Tbase Tunit)
	; check_linear [e1,[] ; e2,[]]

      | Etuple es -> 
	if List.length es <= 1 then
	  raise (Type_error "Tuples must have at least two components.");
	List.iter es (tcheck context)
	; t_exp == (Ttuple (List.map es get_type))
	; check_linear (List.map es (fun x -> (x,[])))

      | Etuple_match (e1,xts,e2) ->
	if List.length xts <= 1 then
	  raise (Type_error "Tuples must have at least two components.");
	let (xs,ts) = List.unzip xts in
	tcheck context e1
	; e1.exp_type == (Ttuple (List.map xts snd))
	; tcheck (Toolbox.map_add_list context xs ts) e2
	; t_exp == e2.exp_type
	; check_linear [e1,[] ; e2,xts]

      | Eundefined -> ()

      | Etick n -> t_exp == (Tbase Tunit)

  in
  tcheck (String.Map.empty) exp


type tstack_mode =
  | Tstack_bind
  | Tstack_open

let typecheck_stack ?linear:(linear=false) exp constr_map =

  let lookup_ind_type = Map.find_exn constr_map in

  let check_linear es = 
    if linear then
      check_linear es
    else
      ()
  in

  let apply = apply exp in

  let bounded_type e = 
    match e.exp_info with
      | [] -> e.exp_type
      | ts -> Tarrow (ts, e.exp_type, ())
  in

  let rec tcheck tstack_mode context exp = 

    let (==) t1 t2 = unify_types t1 t2 exp in

    let t_exp = exp.exp_type in

    let check_fun te ts = 
      let fts = get_fun_types ts in
      if List.mem ~equal:(=) fts te then
	()
      else begin
	print_string "\n\nType Error: Mismatch, when trying to match\n\n   "
	; Pprint.print_raml_type te
	; print_string "\n\nwith a function type such as\n\n   "
	; Pprint.print_raml_type (List.hd_exn fts)
	; print_string "\n\nin expression\n\n   "
	; Pprint.print_expression exp
	; Format.print_newline ()
	; Format.print_newline ()
	; raise (Type_error "Type mismatch.")
      end
    in


    let split_type t =
      match t, tstack_mode with
	| Tarrow (targs,tres,a), Tstack_open ->
	  (targs,tres)
	| _ -> ([],t)
    in

    match exp.exp_desc with
      | Ebase_const c -> 
        let _ =
          match c with
            | Cint _ -> t_exp == Tbase Tint
            | Cfloat _ -> t_exp == Tbase Tfloat
            | Cbool _ -> t_exp == Tbase Tbool
            | Czero -> t_exp == Tnat
            | Cunit -> if t_exp <> Tbase Tghost_unit then t_exp == Tbase Tunit
        in
        { exp with
          exp_desc = Ebase_const c;
          exp_info = []
        }

      | Ebase_fun f ->
	let _ =
	  match f with
	    | Nat_succ -> check_fun t_exp [Tnat;Tnat]
	    | Nat_to_int -> check_fun t_exp [Tnat; Tbase Tint]
	    | Nat_of_int -> check_fun t_exp [Tbase Tint; Tnat]
	    | Nat_of_intc _ -> t_exp == Tnat
	    | Nat_add
	    | Nat_mult -> check_fun t_exp [Ttuple [Tnat; Tnat]; Tnat]
	    | Nat_minusc _ -> check_fun t_exp [Tnat; Ttuple [Tnat; Tnat]]
	    | Nat_minus -> check_fun t_exp [Ttuple [Tnat; Tnat]; Ttuple [Tnat; Tnat]]
	    | Nat_div_mod -> check_fun t_exp [Ttuple [Tnat; Tnat]; Ttuple [Tnat; Tnat; Tnat]]

	    | Arr_make -> (
	      match t_exp with
		| Tarrow ([Ttuple [Tnat; t1]], Tarray t2, ()) when t1 = t2 -> ()
		| _ -> raise (Type_error "Type mismatch (Array.make).")
	    )
	    | Arr_set -> (
	      match t_exp with
		| Tarrow ([Ttuple [Tarray t1; Tnat; t2]], Tbase Tunit, ()) when t1 = t2 -> ()
		| _ -> raise (Type_error "Type mismatch (Array.set).")
	    )
	    | Arr_get -> (
	      match t_exp with
		| Tarrow ([Ttuple [Tarray t1; Tnat]], t2, ()) when t1 = t2 -> ()
		| _ -> raise (Type_error "Type mismatch (Array.get).")
	    )

	    | Arr_length -> (
	      match t_exp with
		| Tarrow ([Tarray t1], Tnat, ())  -> ()
		| _ -> raise (Type_error "Type mismatch (Array.length).")
	    )

	    | Ref_swap -> (
	      match t_exp with
		| Tarrow ([Ttuple [Tref t1; t2]], t3, ()) when t1 = t2 && t1 = t3 -> ()
                | _ -> raise (Type_error "Type mismatch (Raml.ref_swap).")
            )

            | Res_consume _ -> (
	      match t_exp with
	        | Tarrow ([t1], Tbase Tunit, ()) -> ()
	        | _ -> raise (Type_error "Type mismatch (Raml.consume).")
             )
	in
	let (tstack, exp_type) = split_type t_exp in
	{exp with 
	  exp_desc = Ebase_fun f;
	  exp_info = tstack;
	  exp_type
	}

      | Ebase_op op ->
	let _ =
	  match op with
	    | Un_not -> check_fun t_exp [Tbase Tbool; Tbase Tbool]
	    | Un_iminus -> check_fun t_exp [Tbase Tint; Tbase Tint]
	    | Un_fminus -> check_fun t_exp [Tbase Tfloat; Tbase Tfloat]
	    | Bin_iadd
	    | Bin_isub
	    | Bin_imult
	    | Bin_imod
	    | Bin_idiv -> check_fun t_exp [Ttuple [Tbase Tint; Tbase Tint]; Tbase Tint]
	    | Bin_fadd
	    | Bin_fsub
	    | Bin_fmult
	    | Bin_fdiv -> check_fun t_exp [Ttuple [Tbase Tfloat; Tbase Tfloat]; Tbase Tfloat]
	    | Bin_and
	    | Bin_or -> check_fun t_exp [Ttuple [Tbase Tbool; Tbase Tbool]; Tbase Tbool]
	    | Bin_eq -> (
	      match t_exp with
		| Tarrow ([Ttuple [t1; t2]], Tbase Tbool, ()) when t1 = t2 -> begin
		  match t1 with
		    | Tbase _ | Tnat  -> ()
		    | _ -> raise (Type_error "Equality is only supported for base types")
		  end
		| _ -> raise (Type_error "Type mismatch (equality).")
	    )
	    | Bin_iless_eq
	    | Bin_igreater_eq
	    | Bin_iless
	    | Bin_igreater ->  (
	      match t_exp with
		| Tarrow ([Ttuple [t1; t2]], Tbase Tbool, ()) when t1 = t2 -> begin
		  match t1 with
		    | Tbase Tint | Tnat -> ()
		    | _ -> raise (Type_error "Integer comparision is only supported for integers and nats.")
	        end
		| _ -> raise (Type_error "Type mismatch (equality).")
	    )
	    | Bin_fless_eq
	    | Bin_fgreater_eq
	    | Bin_fless
	    | Bin_fgreater -> check_fun t_exp [Ttuple [Tbase Tfloat; Tbase Tfloat]; Tbase Tbool]
	in
	let (tstack, exp_type) = split_type t_exp in
	{exp with 
	  exp_desc = Ebase_op op;
	  exp_info = tstack;
	  exp_type
	}

      | Evar vid ->
	let _ =
	  match Map.find context vid with
	    | Some t -> t_exp == t
	    | None -> raise (Type_error "Unbound variable.")
	in
	let (tstack, exp_type) = split_type t_exp in
	{exp with 
	  exp_desc = Evar vid;
	  exp_info = tstack;
	  exp_type
	}

      | Eapp (cname,e,es) ->
	let _ = check_linear (List.map (e::es) (fun x -> (x,[]))) in
	let es' = List.map es (tcheck_bind context) in
	let e' = tcheck Tstack_open context e in
	let ts = List.map es get_type in
	let () = t_exp == (apply (get_type e) ts) in
	let () = t_exp == e'.exp_type in
	let () = List.iter2_exn ts e'.exp_info (==) in
	let (tstack, exp_type) = split_type t_exp in
	{exp with 
	  exp_desc = Eapp (cname,e',es');
	  exp_info = tstack;
	  exp_type
	}

      | Elambda ((x,t_x),e) ->
	begin
	  match t_exp with
	    | Tarrow (targ_exp, tret_exp, _) -> 
	      begin
		match targ_exp with
		  | [] -> raise (Type_error "Malformed function type.")
		  | targ1::targs ->
		    targ1 == t_x;
		    let tstack_mode_e1 =
		      if List.length targs = 0 then
			Tstack_bind
		      else
			Tstack_open
		    in
		    let e' = tcheck tstack_mode_e1 (Map.set context x t_x) e in
		    let _ = tret_exp == e'.exp_type in
		    let _ = 
		      match List.zip targs e'.exp_info with
			| None -> raise (Type_error "Type mismatch: Wrong number of arguments.")
			| Some ts ->
			  List.iter ts (fun (t1,t2) -> t1 == t2)
		    in
		    let exp_desc = Elambda ((x,t_x),e') in
		    let (exp_info, exp_type) = 
		      match tstack_mode with
			| Tstack_bind -> ([], t_exp)
			| Tstack_open -> (targ_exp, tret_exp)
		    in
		    {exp with exp_desc; exp_info; exp_type}
	      end
	    | _ -> raise (Type_error "Non-function type for lambda expression.")
	end

      | Elet (x_opt,e1,e2) -> 
	let e1' = tcheck_bind context e1 in
	let context2 = 
	  match x_opt with
	    | Some (x,t) -> 
	      t == e1'.exp_type
	      ; check_linear [e1,[]; e2,[(x,t)]]
	      ; Map.set context x t
	    | None -> 
	      check_linear [e1,[]; e2,[]]
	      ; context
	in
	let e2' = tcheck tstack_mode context2 e2 in
	t_exp == (bounded_type e2')
	;{ exp with
	  exp_desc = Elet (x_opt,e1',e2');
	  exp_info = e2'.exp_info;
          exp_type = e2'.exp_type
  	}

      | Eletrec (xts, es, e) -> 
	if not (List.for_all es is_lambda) then
	  raise (Type_error "Expecting lambda bindings in letrec.");
	let (xs,ts) = List.unzip xts in
	let context' = Toolbox.map_add_list context xs ts in
	let es' = List.map (List.zip_exn es ts) 
	  begin 
	    fun (e,t) ->
	      let e' = tcheck_bind context' e in
	      e'.exp_type == t; 
	      e'
          end
	in
	let e' = tcheck tstack_mode context' e in
	t_exp == (bounded_type e')
	; check_linear (List.map (e::es) (fun x -> (x,xts)))
	;{ exp with
	  exp_desc = Eletrec (xts, es', e');
	  exp_info = e'.exp_info;
          exp_type = e'.exp_type
  	}

      | Econd (e,e1,e2) ->
	let e' = tcheck_bind context e in
	let _ = e'.exp_type == (Tbase Tbool) in
	let e1' = tcheck tstack_mode context e1 in
	let e2' = tcheck tstack_mode context e2 in
	t_exp == (bounded_type e1')
	; t_exp == (bounded_type e2')
	; List.iter2_exn e1'.exp_info e2'.exp_info (==)
	; check_linear [e,[] ; e1,[]]
	; check_linear [e,[] ; e2,[]]
	;{ exp with
	  exp_desc = Econd (e',e1',e2');
	  exp_info = e1'.exp_info;
          exp_type = e1'.exp_type
  	}

      | Eshare (e1,(x1,t_x1),(x2,t_x2),e2) -> 
	let e1' = tcheck_bind context e1 in
	let context' = Toolbox.map_add_list context [x1;x2] [t_x1;t_x2] in
	let e2' = tcheck tstack_mode context' e2 in
	t_x1 == t_x2
	; t_x1 == e1'.exp_type
	; t_exp == (bounded_type e2')
	; check_linear [e1,[] ; e2,[(x1,t_x1); (x2,t_x2)]]
	;{ exp with
	  exp_desc = Eshare (e1',(x1,t_x1),(x2,t_x2),e2');
	  exp_info = e2'.exp_info;
          exp_type = e2'.exp_type
  	}

      | Econst (cid,e) ->
	let t_ind = lookup_ind_type cid in
	let e' = tcheck_bind context e in
	e.exp_type == (Rtypes.unfold t_ind cid)
	; t_exp == t_ind
	;{ exp with
	  exp_desc = Econst (cid,e');
	  exp_info = [];
  	}

      | Ematch (e1,matches) -> 
        let _ = 
	  match matches with
	    | [] -> raise (Type_error "Pattern match with with empty list of matches.")
	    | _ -> ()
	in
	let e1' = tcheck_bind context e1 in
	let t_e1 = e1'.exp_type in
	let check_match (cid, xts, e) =
	  let _ = t_e1 == (lookup_ind_type cid) in
	  let t_cid = Rtypes.unfold t_e1 cid in
	  let _ = 
	    match xts with
	      | [] -> raise (Emalformed "Empty pattern matching.")
	      | [(x,t)] -> 
		t == t_cid
	      | xts -> 
		begin 
		  match t_cid with
		    | Ttuple ts -> 
		      List.iter (List.zip_exn ts (List.map xts snd)) 
			(fun (a,b) -> a == b)
		    | _ -> raise (Type_error "Expectin tupel type when matching multiple variables")
		end
	  in
	  let (xs, ts) = List.unzip xts in
	  let e' = tcheck tstack_mode (Toolbox.map_add_list context xs ts) e in
	  let _ = t_exp == bounded_type e' in
	  (cid, xts, e')
	in
	let matches' = List.map matches check_match in
	let _ = List.iter matches ( fun (_, xts, e) ->
	  check_linear [(e1,[]); (e,xts)] )
	in
	let (exp_info, exp_type) =
	  match t_exp, tstack_mode with
	    | Tarrow (targs, tret, ()), Tstack_open -> 
	      (targs, tret)
	    | _ -> ([], t_exp)
	in
	let _ = List.iter matches' 
	  (fun (_, _, e) -> List.iter2_exn e.exp_info exp_info (==) )
	in
	{exp with exp_info; exp_type;
	  exp_desc = Ematch (e1',matches')
	}

      | Enat_match (e,e1,(x,t),e2) ->
	let e' = tcheck_bind context e in
	let _ = e'.exp_type == Tnat in
	let e1' = tcheck tstack_mode context e1 in
	let _ = t_exp == (bounded_type e1') in
	let e2' = tcheck tstack_mode (Map.set context x t) e2 in
	t_exp == (bounded_type e2')
	; List.iter2_exn e1'.exp_info e2'.exp_info (==)
	; check_linear [e,[] ; e1,[] ]
	; check_linear [e,[] ; e2,[x,t] ]
	;{ exp with
	  exp_desc = Enat_match (e',e1',(x,t),e2');
	  exp_info = e2'.exp_info;
          exp_type = e2'.exp_type
  	}

      | Eref e -> 
	let e' = tcheck_bind context e in
	t_exp == (Tref e'.exp_type)
	;{exp with
	  exp_desc = Eref e';
	  exp_info = []
	}

      | Eref_deref e -> 
	let e' = tcheck_bind context e in
	let _ = (Tref t_exp) == e'.exp_type in
	let (tstack, exp_type) = split_type t_exp in
	{ exp with
	  exp_desc = Eref_deref e';
	  exp_info = tstack;
	  exp_type
	}

      | Eref_assign (e1,e2) ->
	let e1' = tcheck_bind context e1 in
	let e2' = tcheck_bind context e2 in
	(Tref e2'.exp_type) == e1'.exp_type
	; t_exp == (Tbase Tunit)
	; check_linear [e1,[] ; e2,[]]
	;{exp with
	  exp_desc = Eref_assign (e1',e2');
	  exp_info = []
	}

      | Etuple es ->
	if List.length es <= 1 then
	  raise (Type_error "Tuples must have at least two components.");
	let es' = List.map es (tcheck_bind context) in
	t_exp == (Ttuple (List.map es' get_type))
	; check_linear (List.map es (fun x -> (x,[])))
	;{exp with
	  exp_desc = Etuple es';
	  exp_info = []
	}

      | Etuple_match (e1,xts,e2) ->
	if List.length xts <= 1 then
	  raise (Type_error "Tuples must have at least two components.");
	let (xs,ts) = List.unzip xts in
	let e1' = tcheck_bind context e1 in
	let _ = e1'.exp_type == (Ttuple (List.map xts snd)) in
	let e2' = tcheck tstack_mode (Toolbox.map_add_list context xs ts) e2 in
	t_exp == (bounded_type e2')
	; check_linear [e1,[] ; e2,xts]
	;{ exp with
 	  exp_desc = Etuple_match (e1',xts,e2');
	  exp_info = e2'.exp_info;
          exp_type = e2'.exp_type
  	}


      | Eundefined ->
	let (tstack, exp_type) = split_type t_exp in
	{exp with 
	  exp_desc = Eundefined;
	  exp_info = tstack;
	  exp_type
	}


      | Etick n -> 
	t_exp == (Tbase Tunit)
	;{ exp with
 	  exp_desc = Etick n;
	  exp_info = []
  	}

  and tcheck_bind context exp = 
    let exp' = tcheck Tstack_bind context exp in
    assert (exp'.exp_info = []);
    exp'

  in

  tcheck Tstack_bind (String.Map.empty) exp
