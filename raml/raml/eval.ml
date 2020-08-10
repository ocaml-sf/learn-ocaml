(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   eval.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Evaluation.
 *)


open Core
open Toolbox
open Expressions
open Rtypes
open Metric

type location = int

type 'a value = 
  | Vbc of constant
  | Vbase_fun of builtin_fun * expression_kind * location list
  | Vbase_op of builtin_op * expression_kind * location list
  | Vnat of int
  | Vloc of location
  | Vtuple of location list
  | Vclosure of var_id * ('a, unit) expression * stack
  | Vconst of constr_id * location
  | Varray of (location Int.Map.t) * int


and stack = (location String.Map.t)

type 'a heap = ('a value Int.Map.t)


type 'a eval_env =
    { stack : stack
    ; args : location list
    }

let compare_values v1 v2 = 
  begin
    match v1,v2 with
      | None, None -> true
      | Some (l1,h1), Some (l2,h2) ->

	let find1 = Map.find_exn h1 in
	let find2 = Map.find_exn h2 in

	let rec cvals l1 l2 =
	  match l1, l2 with
	    | Vbc c1, Vbc c2 -> c1 = c2
	    | Vbase_fun (f1,k1,ls1), Vbase_fun (f2,k2,ls2) -> 
	      (f1 = f2) && (k1 = k2) && (clocs ls1 ls2)
	    | Vbase_op (op1,k1,ls1), Vbase_op (op2,k2,ls2) -> 
	      (op1 = op2) && (k1 = k2) && (clocs ls1 ls2)
	    | Vnat n1, Vnat n2 -> n1 = n2
	    | Vloc l1, Vloc l2 -> clocs [l1] [l2]
	    | Vtuple ls1, Vtuple ls2 -> clocs ls1 ls2
	    | Vclosure _, Vclosure _ -> true  (* we don't look inside closures *)
	    | Vconst (cid1,l1), Vconst (cid2,l2) -> (cid1 = cid2) && (clocs [l1] [l2])
	    | Varray (arr1, n1), Varray (arr2, n2) -> (n1 = n2) && (clocs (Map.data arr1) (Map.data arr2))
	    | _, _ -> false

	and clocs ls1 ls2 = 
	  match List.zip ls1 ls2 with
	    | None -> false
	    | Some ls ->
	      List.fold ls ~init:true 
		~f:(fun acc (l1, l2) -> (cvals (find1 l1) (find2 l2)) && acc)

	in
	
	cvals (find1 l1) (find2 l2)

      | _,_ -> false
  end


let loc_factory n =
  let next_loc = ref n in

  let fresh_loc _ =
    next_loc := !next_loc + 1;
    !next_loc
  in

  let rec fresh_locs = function
    | n when n <= 0 -> []
    | n -> fresh_loc ()::fresh_locs (n-1)
  in
  
  (fresh_loc,fresh_locs)

exception Eval_error of string
exception Eval_undefined

let evaluate : 
    ('a, unit) Expressions.expression ->
  (Metric.res_const -> float) list ->
  ((location * 'a heap) option) * (float * float) list
  = fun exp metrics ->

    let heap = ref Int.Map.empty in
    let costs = ref (List.map metrics (fun _ -> (0.0,0.0))) in

    let (fresh_loc,fresh_locs) = loc_factory 0 in

    let lookup loc =
      match Map.find !heap loc with
	| None -> raise (Eval_error ("Empty location: " ^ (string_of_int loc )))
	| Some value -> value
    in

    let store value =
      let l = fresh_loc () in
      heap := Map.set !heap ~key:l ~data:value;
      l
    in

    let too_many_args = Eval_error "Built-in function is applied to too many arguments." in
    let wrong_arg_type = Eval_error "Built-in function is applied to an argument of wrong type." in

    let eval_op op args kind =

      let count k =
	if kind = Efree then
	  ()
	else
	  costs := List.map2_exn !costs metrics 
	    (fun cost m -> cost ^- (m k))
      in

      let store_op n = 
	if n > 0 then count (Mclosure n); 
	store (Vbase_op (op,kind,args))
      in

      let count_store value =
	count (Mbop_eval op)
	; store value
      in

      match args with
      | [] -> store_op 0
      | _ :: _ :: _ -> raise too_many_args
      | [l] -> match op, lookup l with

        | Un_not, Vbc (Cbool b)     -> count_store (Vbc (Cbool  (not b)))
        | Un_iminus, Vbc (Cint i)   -> count_store (Vbc (Cint   (-i)))
        | Un_fminus, Vbc (Cfloat q) -> count_store (Vbc (Cfloat (-.q)))

        | _, Vtuple [l1; l2] -> begin
          match lookup l1, lookup l2 with
          | Vnat n1, Vnat n2 -> begin
	    match op with
	      | Bin_eq -> count_store (Vbc (Cbool (n1 = n2)))
              | Bin_iless_eq    -> count_store (Vbc (Cbool (n1 <= n2)))
              | Bin_igreater_eq -> count_store (Vbc (Cbool (n1 >= n2)))
              | Bin_iless       -> count_store (Vbc (Cbool (n1 <  n2)))
              | Bin_igreater    -> count_store (Vbc (Cbool (n1 >  n2)))
	      | _ -> raise wrong_arg_type
	  end

          | Vbc (Cint i1), Vbc (Cint i2) -> begin
            match op with
            | Bin_iadd        -> count_store (Vbc (Cint  (i1 +  i2)))
            | Bin_isub        -> count_store (Vbc (Cint  (i1 -  i2)))
            | Bin_imult       -> count_store (Vbc (Cint  (i1 *  i2)))
            | Bin_imod        -> count_store (Vbc (Cint  (i1 %  i2)))
            | Bin_idiv        -> count_store (Vbc (Cint  (i1 /  i2)))
            | Bin_eq          -> count_store (Vbc (Cbool (i1 =  i2)))
            | Bin_iless_eq    -> count_store (Vbc (Cbool (i1 <= i2)))
            | Bin_igreater_eq -> count_store (Vbc (Cbool (i1 >= i2)))
            | Bin_iless       -> count_store (Vbc (Cbool (i1 <  i2)))
            | Bin_igreater    -> count_store (Vbc (Cbool (i1 >  i2)))
            | _ -> raise wrong_arg_type
            end

          | Vbc (Cfloat q1), Vbc (Cfloat q2) -> begin
            match op with
            | Bin_fadd        -> count_store (Vbc (Cfloat (q1 +. q2)))
            | Bin_fsub        -> count_store (Vbc (Cfloat (q1 -. q2)))
            | Bin_fmult       -> count_store (Vbc (Cfloat (q1 *. q2)))
            | Bin_fdiv        -> count_store (Vbc (Cfloat (q1 /. q2)))
            | Bin_eq          -> count_store (Vbc (Cbool  (q1 =  q2)))
            | Bin_fless_eq    -> count_store (Vbc (Cbool  (q1 <= q2)))
            | Bin_fgreater_eq -> count_store (Vbc (Cbool  (q1 >= q2)))
            | Bin_fless       -> count_store (Vbc (Cbool  (q1 <  q2)))
            | Bin_fgreater    -> count_store (Vbc (Cbool  (q1 >  q2)))
            | _ -> raise wrong_arg_type
            end

          | Vbc (Cbool b1), Vbc (Cbool b2) -> begin
            match op with
            | Bin_and         -> count_store (Vbc (Cbool (b1 && b2)))
            | Bin_or          -> count_store (Vbc (Cbool (b1 || b2)))
            | Bin_eq          -> count_store (Vbc (Cbool (b1 =  b2)))
            | _ -> raise wrong_arg_type
            end

          | _ -> raise wrong_arg_type
          end

        | _ -> raise wrong_arg_type
    in

    let eval_bfun f args kind =

      let count k =
	if kind = Efree then
	  ()
	else
	  costs := List.map2_exn !costs metrics 
	    (fun cost m -> cost ^- (m k))
      in

      let store_fun n = 
	if n > 0 then count (Mclosure n); 
	store (Vbase_fun (f,kind,args))
      in

      let out_of_bounds = Eval_error "Array access is out of bounds." in

      match f,args with

	| Nat_of_intc n, [] ->
	  count (Mbfun_eval f);
	  store (Vnat n)
	  
	| _, [] -> store_fun 0

	| Nat_succ, [l] -> (
	  match lookup l with
	    | Vnat n ->
	      count (Mbfun_eval f);
	      store (Vnat (n+1))
	    | _ -> raise wrong_arg_type
	)

	| Nat_to_int, [l] -> (
	  match lookup l with
	    | Vnat n ->
	      count (Mbfun_eval f);
	      store (Vbc (Cint n))
	    | _ -> raise wrong_arg_type
	)

	| Nat_of_int, [l] -> (
	  match lookup l with
	    | Vbc (Cint n) ->
	      count (Mbfun_eval f);
	      store (Vnat n)
	    | _ -> raise wrong_arg_type
	)

	  
	| Nat_minusc c, [l] -> (
	  match lookup l with
	    | Vnat n ->
	      let diff = n - c in
	      if diff < 0 then
		raise (Eval_error ("Natural number overflow: " ^ string_of_int (diff)))
	      else
		count (Mbfun_eval f);
	      store (Vtuple (List.map [Vnat diff; Vnat c] store))

	    | _ -> raise wrong_arg_type
	)

	| Arr_length, [l] -> (
	  match lookup l with
	    | Varray (arr,n) ->
	      count (Mbfun_eval f);
	      store (Vnat n)
	    | _ -> raise wrong_arg_type
	)

        | Nat_add, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1, lookup l2 with
                | Vnat n1, Vnat n2 ->
                  count (Mbfun_eval f);
                  store (Vnat (n1 + n2))
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
        end

        | Nat_mult, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1, lookup l2 with
                | Vnat n1, Vnat n2 ->
                  count (Mbfun_eval f);
                  store (Vnat (n1 * n2))
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
        end

        | Nat_minus, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1, lookup l2 with
                | Vnat n1, Vnat n2 ->
                  let diff = n1 - n2 in
                  if diff < 0 then
                    raise (Eval_error ("Natural number overflow: " ^ string_of_int (diff)));
                  count (Mbfun_eval f);
                  store (Vtuple (List.map [Vnat diff; Vnat n2] store))
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
        end

        | Nat_div_mod, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1, lookup l2 with
                | Vnat n1, Vnat n2 ->
                  let (ndiv,nmod) = (n1/n2, n1%n2) in
                  count (Mbfun_eval f);
                  store (Vtuple (List.map [Vnat ndiv; Vnat nmod; Vnat n2] store))
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
        end

        | Arr_make, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1, lookup l2 with
                | Vnat n, v_init ->
                  count (Mbfun_eval f);
                  let f_add i amap =
                    count (Marr_make_elem);
                    let l = store v_init in
                    Map.set amap i l
                  in
                  store (Varray (iterate (0, n) Int.Map.empty f_add, n))
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
        end

        | Arr_get, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1, lookup l2 with
                | Varray (arr,n), Vnat i ->
                  count (Mbfun_eval f);
                  if i < 0 || i > n-1 then
                    raise out_of_bounds
                  else
                    Map.find_exn arr i
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
        end

        | Arr_set, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2; l3] -> begin
              match lookup l1, lookup l2 with
                | Varray (arr,n), Vnat i ->
                  count (Mbfun_eval f);
                  if i < 0 || i > n-1 then
                    raise out_of_bounds
                  else
                    heap := Map.set !heap l1 (Varray (Map.set arr i l3, n));
                  store (Vbc Cunit)
                | _ -> raise wrong_arg_type
              end
            | _ -> raise wrong_arg_type
	end

	| Ref_swap, [l] -> begin
          match lookup l with
            | Vtuple [l1; l2] -> begin
              match lookup l1 with
		| Vloc l_ret ->
		  count (Mbfun_eval f);
		  heap := Map.set !heap l1 (Vloc l2);
		  l_ret
                | _ -> raise wrong_arg_type
	    end
            | _ -> raise wrong_arg_type
        end

        | Res_consume _, [l1;l2] -> begin
          match lookup l1 with
            | Vbc (Cint n) ->
		let () = count (Mbfun_eval f) in
                store (Vbc Cunit)
            | _ -> raise wrong_arg_type
        end

        | Res_consume _, [l] -> begin
	    let () = count (Mbfun_eval f) in
            store (Vbc Cunit)
        end

        | _ -> raise too_many_args
    in

    let rec eval_all es stack cont = 
      match es with
	| [] -> cont []
	| e::es ->
	  let cont' l =
	    eval_all es stack (fun ls -> cont (l::ls))
	  in
	  eval e [] stack cont'

    and eval exp args stack cont = 

      let count k =
	if exp.exp_kind = Efree then
	  ()
	else
	  costs := List.map2_exn !costs metrics 
	    (fun cost m -> cost ^- (m k))
      in

      let check_args n =
	let l = List.length args in
	if l > n then
	  raise (Eval_error ("Expecting " ^ (string_of_int n) ^ 
				" arguments but found " ^ (string_of_int l) ^ "." ) )
	else ()
      in

      let interpret_loc loc = match lookup loc, args with
        | _, [] -> cont loc
        | Vbase_op (op, kind, args_cl), _ -> cont (eval_op op (args_cl@args) kind)
        | Vbase_fun (f, kind, args_cl), _ -> cont (eval_bfun f (args_cl @ args) kind)
        | Vclosure ( x, e, stack_cl), l1::ls ->
          let stack' = Map.set stack_cl x l1 in
          eval e ls stack' cont
        | _, _ -> raise too_many_args
      in

      match exp.exp_desc with

	| Ebase_const Czero  ->
	  check_args 0;
	  count Mbase_const;
	  let l = store (Vnat 0) in
	  cont l

	| Ebase_const const  ->
	  check_args 0;
	  count Mbase_const;
	  let l = store (Vbc const) in
	  cont l

	| Ebase_fun f -> 
	  count Mbfun_load;
	  cont (eval_bfun f args exp.exp_kind)

	| Ebase_op op ->
	  count Mbop_load;
	  cont (eval_op op args exp.exp_kind)

	| Evar x -> 
	  count Mvar;
	  ( match Map.find stack x with
	    | None -> raise (Eval_error ("Unbound variable: " ^ x))
	    | Some loc -> interpret_loc loc
	  )

	| Eapp (_,e,es) ->
          let cont' results = 
	    let ls = List.rev results in
            match args with
            | [] ->
              eval e ls stack (fun l -> cont l)
            | b::bs ->
              let closure_cont l =
                match lookup l with
                | Vclosure ( x_cl, e_cl, stack_cl) ->
                  let new_stack = Map.set stack_cl x_cl b in
                  eval e_cl bs new_stack (fun l -> cont l)
                | _ -> raise (Eval_error "Expecting closure")
              in
              eval e ls stack closure_cont
	  in
          count (Mapp (List.length es));
          eval_all (List.rev es) stack cont'

	| Elambda ((x,t),e) -> (
	  match args with
	    | [] ->
	      let fvar_set = Set.remove (free_vars e) (x,t) in
	      let c_stack = Map.filteri stack 
		~f:(fun ~key:x ~data:_ -> 
		  Set.exists fvar_set ~f:(fun yt -> fst yt = x))
	      in 
	      count (Mclosure (Set.length fvar_set));
	      let l = store (Vclosure (x,e,c_stack) ) in
	      cont l
	    | l::ls ->
	      let stack' = Map.set stack x l in
	      eval e ls stack' cont
	)

	| Elet (x_opt,e1,e2) ->
	  count Mlet; 
	  let cont' l1 = 
	    let stack' =
	      match x_opt with
		| None -> stack
		| Some (x,t) -> Map.set stack ~key:x ~data:l1
	    in
	    eval e2 args stack' (fun l -> cont l)
	  in
	  eval e1 [] stack cont'

	| Eletrec (xs,es,e) ->
	  count (Mletrec (List.length xs)); 
	  let cont' ls =
	    let stack' = map_add_list stack (List.map xs fst) ls in
	    let patch_closure l = 
	      match lookup l with
		| Vclosure (x,e,cl_stack) ->
		  let resolve ~key:_ = 
		    function `Both (l1,l2) -> Some l1 
		      | `Left l -> Some l
		      | `Right l -> Some l 
		  in
		  let cl_new = Vclosure (x,e, Map.merge stack' stack resolve) in
		  heap := Map.set !heap l cl_new
		| _ -> raise (Eval_error "Dead code (hopefully)")
	    in
	    let _ = List.iter ls patch_closure in
	    eval e args stack' (fun l -> cont l)
	  in
	  eval_all es stack cont'

	| Econd (e,e1,e2) -> 
	  let cont' l =
	    match lookup l with
	      | Vbc (Cbool b) ->
		count Mcond; 
		let e' = if b then e1 else e2 in
		eval e' args stack (fun l -> cont l)
	      | _ -> raise (Eval_error "Non-boolean value in conditional.")
	  in
	  eval e [] stack cont'

	| Eshare (e1,(x1,t1),(x2,t2),e2) ->
	  let cont' l1 = 
	    count Mshare; 
	    let stack' = Map.set (Map.set stack x1 l1) x2 l1 in
	    eval e2 args stack' (fun l -> cont l)
	  in
	  eval e1 [] stack cont'

	| Econst (c,e) ->
	  check_args 0;
	  let cont' l =
	    count Mconst
	    ; cont (store (Vconst (c,l)))
	  in
	  eval e [] stack cont'


	| Ematch (e1,matches) -> 
	  let rec cont_match matches c_m l_m  =
	    match matches with
	      | [] -> raise (Eval_error "Non-exhaustive pattern match.")
	      | (c,xs,e)::matches ->
		if c_m = c then
		  let stack' =
		    match xs with
		      | [] -> raise (Emalformed "Empty variable list in match.")
		      | [(x,_)]  -> Map.set stack x l_m
		      | _ -> (
			match lookup l_m with
			  | Vtuple ls -> map_add_list stack (List.map xs fst) ls
			  | _ -> raise 
			    (Eval_error "Matching multiple variables on a value that isn't a tuple.")
		      )
		  in
		  count (Mmatch (List.length matches)); 
		  eval e args stack' 
		    (fun l -> cont l)
		else
		  cont_match matches c_m l_m
	  in
	  let cont' l = 
	    match lookup l with
	      | Vconst (c_m,l_m) -> cont_match matches c_m l_m
	      | _ -> raise (Eval_error "Matching on a value that isn't a constructor.")
	  in
	  eval e1 [] stack cont'

	| Enat_match (e,e1,x,e2) -> 
	  let cont' l = 
	    count Mnat_match; 
	    match lookup l with
	      | Vnat n ->
		if n = 0 then
		  eval e1 args stack cont
		else (* n > 0 *)
		  let l = store (Vnat (n-1)) in
		  let stack' = Map.set stack (fst x) l in
		  eval e2 args stack' cont
	      | _ -> raise (Eval_error "Matching on a value that isn't a nat.")
	  in
	  eval e [] stack cont'

	| Eref e ->
	  check_args 0;
	  let cont' l = 
   	    count Mref
	    ; cont (store (Vloc l)) 
	  in
	  eval e [] stack cont'

	| Eref_deref e ->
	  (* e may be a function, there might be arguments waiting *)
	  let cont' l =
 	    count Mref_deref
	    ; match lookup l with
	      | Vloc l -> interpret_loc l
	      | _ -> raise (Eval_error "Dereferencing a value that isn't a reference.")
	  in
	  eval e [] stack cont'

	| Eref_assign (e1,e2) ->
	  check_args 0;
	  let cont' l2 =
	    eval e1 [] stack 
	      (fun l1 -> 
		count Mref_assign
		; heap := Map.set !heap l1 (Vloc l2)
		; cont (store (Vbc Cunit))
	      )
	  in
	  eval e2 [] stack cont'

	| Etuple es ->
	  check_args 0;
	  eval_all es stack (fun ls -> 
 	    count (Mtuple (List.length es))
	    ; cont (store (Vtuple ls))
	  )

	| Etuple_match (e1,xs,e2) ->
	  let cont' l1 =
	    match lookup l1 with
	      | Vtuple ls -> 
		let stack' = map_add_list stack (List.map xs fst) ls in
		count (Mtuple_match (List.length xs));
		eval e2 args stack' cont
	      | _ -> raise (Eval_error "Matching on a value that ins't a tuple.")
	  in
	  eval e1 [] stack cont'

	| Eundefined ->
	  raise (Eval_undefined)

	| Etick q ->
	  check_args 0;
	  count (Mtick q);
	  cont (store (Vbc Cunit))

    in

    try
      let l = eval exp [] String.Map.empty (fun value -> value) in
      ( Some (l, !heap), !costs)
    with
      | Eval_undefined -> (None, !costs)

