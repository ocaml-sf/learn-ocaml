 (* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   analysis.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Deriving and solving type annotations.
 *)

open Core

open Rconfig
open Toolbox
open Rtypes
open Expressions
open Solver
open Indices
open Annotations
open Metric



(* Set to false to dactivate constant PASS THROUGH in function applications. *)
(* This is broken for lower bounds and constant time. The problem are
   exceptions: we don't want to pass through constant potential that
   can be consume after the the exception was raised. The right thing
   to do would be to detect if exceptions are raised on not to pass
   through in this case. In the interest of time I implemented a work
   around that uses the cost-free metric to pass through constant
   potential. The places in which I made the respective changes are
   marked with PASS THROUGH *)

   let app_const_pass_through = true


   module Make (Solver: SOLVER) (Amode: AMODE) =
   struct

   module Anno = Annotations.Make(Solver)(Amode)

   let amode_zero_out_tanno =
    match Amode.mode with
    | Mupper -> fun ?(spare_zero=false) _ -> ()
    | Mlower
    | Mconstant ->
      fun ?(spare_zero=false) tanno ->
        Anno.zero_out_tanno ~spare_zero tanno

   let amode_fresh_canno =
    match Amode.mode with
    | Mupper -> Anno.fresh_canno
    | Mlower
    | Mconstant -> Anno.zero_canno

   type var = Anno.var
   exception Anno_exn = Anno.Anno_exn

   exception Analysis_error of string


   (* An annotated type is a rtype in with arrow types are decorated with fun_anno's. *)
  (* The type variable 'a is for the the type of LP variables. *)

  type fun_anno =
    { fan_targs : raml_type list
    ; fan_tres : raml_type
    ; fan_map :
        fun_anno_type list
        -> (fun_anno_type * (var type_anno
                             -> metric
                             -> var type_anno) )
    }

  and fun_anno_type =
    | Fbase of base_type
    | Fnat
    | Fvar of type_var
    | Farray of fun_anno_type ref_chain ref
    | Fref of fun_anno_type ref_chain ref
    | Ftuple of fun_anno_type list
    | Farrow of fun_anno
    | Find of (fun_anno_type constructor) list


  let fanno_to_raml_type ft =
    let rec trans ft =
      match ft with
      | Fbase bt -> Tbase bt
      | Fnat -> Tnat
      | Fvar var -> Tvar var
      | Farray rc ->
        let ft = rc_get rc in
        Tarray (trans ft)
      | Fref rc ->
        let ft = rc_get rc in
        Tref (trans ft)
      | Ftuple ts ->
        Ttuple (List.map ts trans)
      | Farrow fanno ->
        Tarrow (fanno.fan_targs,fanno.fan_tres,())
      | Find clist ->
        let clist' =
          List.map clist
            (fun c -> {c with cstr_type = trans c.cstr_type})
        in
        Tind clist'
    in
    trans ft


  let unfold_ftype
    : fun_anno_type -> constr_id -> fun_anno_type =
    fun t cid ->
      match t with
      | Find clist -> (
          match List.find clist (fun d -> d.cstr_id = cid) with
          | None ->
            raise (Analysis_error "Constructor not part of type declaration.")
          | Some d ->
            let deg = d.cstr_deg in
            if deg > 0 then
              Ftuple (d.cstr_type::(Toolbox.repeat t deg))
            else if deg = 0 then
              d.cstr_type
            else
              raise (Analysis_error "Negative degree.")
        )
      | _ -> raise (Analysis_error "Unfolding a non-inductive type.")


  let compare_fun_annos fanno1 fanno2 =
    assert ((compare_raml_type fanno1.fan_tres fanno2.fan_tres) = 0);
    assert
      begin
        let ts = List.zip_exn fanno1.fan_targs fanno2.fan_targs in
        List.for_all ts (fun (t1,t2) -> (compare_raml_type t1 t2) = 0)
      end;
    phys_equal fanno1.fan_map fanno2.fan_map


  let rec compare_fanno_types
    : fun_anno_type -> fun_anno_type -> bool =
    fun ft1 ft2 ->
      match ft1,ft2 with
      | Fbase bt1, Fbase bt2 when bt1 = bt2 ->
        true
      | Fnat, Fnat ->
        true
      | Fvar vid1, Fvar vid2 when vid1 = vid2 ->
        true
      | Farray rc1, Farray rc2
      | Fref rc1, Fref rc2 ->
        compare_fanno_types (rc_get rc1) (rc_get rc2)
      | Ftuple t1s, Ftuple t2s ->
        List.for_all (List.zip_exn t1s t2s) (fun (t1,t2) -> compare_fanno_types t1 t2)
      | Farrow fanno1, Farrow fanno2 ->
        compare_fun_annos fanno1 fanno2
      | Find clist1, Find clist2 ->
        let g (c1,c2) =
          assert (c1.cstr_deg = c2.cstr_deg);
          assert (c1.cstr_id = c2.cstr_id);
          compare_fanno_types c1.cstr_type c2.cstr_type
        in
        List.for_all (List.zip_exn clist1 clist2) g
      | _ -> raise (Analysis_error "Type mismatch")



  type fun_anno_context = (fun_anno_type) String.Map.t

  type forward_result =
    | Fclosed of fun_anno_type
    | Fopen of (fun_anno_type list -> fun_anno_type)

  (* the raml_type list is the type stack used in the type deriviations at this point *)
  type analysis_expression = (raml_type list, unit) expression

  type analysis_arg =
    { anl_exp : analysis_expression (* expression to be analyzed *)
    ; anl_metric : metric  (* a resource metric *)
    ; anl_deg : int (*non-negative degree (redundant) *)
    ; anl_rtanno : var type_anno (* matching type annotation for the return type *)
    ; anl_level : int (* level of the expression, to collect function types for printing; 10000 -> disabled*)
    ; anl_sigma : fun_anno_context (* a context with annotated function types *)
    ; anl_tstack : fun_anno_type list (* a type stack with annotated function types *)
    }

  type let_rec_anno =
    | Rapplied of (fun_anno_type list) * var type_anno * var type_anno
    | Rused of (fun_anno_type list) * var type_anno * var type_anno
    | Rready of fun_anno
    | Rempty

  (**********)
  (* The folloing is just for reporting function types at the end of the analysis. *)

  let record_fun_type = ref (fun (n:int) (fid:string) (atanno:var type_anno) (rtanno:var type_anno) -> ())
  let record_consume_type = ref (fun (fid:string) (atanno:var type_anno) (rtanno:var type_anno) -> ())
  let recorded_fun_types = ref ([] : (string * var type_anno * var type_anno) list)

  let record_fun_max_level = 10000

  let init_type_collection types_mode =

    let () =
      recorded_fun_types := []
    in                            

    let () = 
      record_fun_type :=
        match types_mode with
        | Pall ->
          fun _ fid atanno rtanno ->
            recorded_fun_types := (fid,atanno,rtanno)::!recorded_fun_types
        | Pconsume
        | Pnone ->
          fun _ _ _ _ -> ()
        | Plevel level ->
          fun n fid atanno rtanno ->
            if n <= level && n < record_fun_max_level then
              recorded_fun_types := (fid,atanno,rtanno)::!recorded_fun_types
            else ()
        | Pregexp re -> failwith "Not implemented yet."
    in

    let () =
      record_consume_type :=
        match types_mode with
        | Pconsume ->
          fun fid atanno rtanno ->
            recorded_fun_types := (fid,atanno,rtanno)::!recorded_fun_types
        | _ -> fun _ _ _ -> ()
    in
    ()
    (**********)

    let unroll_fanno_type =
      let rec unroll ft =
        match ft with
        | Fbase bt -> Tbase bt
        | Fnat -> Tnat
        | Fvar vid -> Tvar vid
        | Farray t -> Tarray (unroll (rc_get t))
        | Fref t -> Tref (unroll (rc_get t))
        | Ftuple ts -> Ttuple (List.map ts unroll)
        | Farrow fanno -> Tarrow (fanno.fan_targs, fanno.fan_tres, ())
        | Find clist ->  Tind (
            List.map clist
              (fun c -> {c with cstr_type = unroll c.cstr_type})
          )
      in
      unroll

  (* Always use this function to apply function annotations. *)
  (* This is a great spot to catch bugs.*)
  let apply_fanno fanno fts =
    assert (List.map fts unroll_fanno_type = fanno.fan_targs);
    fanno.fan_map fts

  let get_var e =
    match e.exp_desc with
    | Evar x -> x
    | _ -> raise (Analysis_error "Expression is not in let normal form.")


  let open_fanno_type tstack ft =
    match tstack, ft with
    | _::_, Farrow fanno ->
      assert (tstack = fanno.fan_targs);
      Fopen (fun fts -> fst (apply_fanno fanno fts))
    | _,_ ->
      assert (tstack = []);
      Fclosed ft

  let rec combine_ref_chains rc1 rc2 =
    let ft1 = rc_get rc1 in
    let ft2 = rc_get rc2 in
    let ft = combine_fanno_types ft1 ft2 in
    let rc = ref (Rleaf ft) in
    let f _ = Rnode rc in
    let _ = rc_update f rc1 in
    let _ = rc_update f rc2 in
    ()

  and combine_fanno_types ft1 ft2 =
    match ft1,ft2 with
    | Fbase bt1, Fbase bt2 when bt1 = bt2 ->
      Fbase bt1
    | Fnat, Fnat ->
      Fnat
    | Fvar vid1, Fvar vid2 when vid1 = vid2 ->
      Fvar vid1
    | Farray rc1, Farray rc2 ->
      let _ = combine_ref_chains rc1 rc2 in
      Farray rc1
    | Fref rc1, Fref rc2 ->
      let _ = combine_ref_chains rc1 rc2 in
      Fref rc1
    | Ftuple ft1s, Ftuple ft2s ->
      let ts = List.map2_exn ft1s ft2s (combine_fanno_types) in
      Ftuple ts
    | Farrow fanno1, Farrow fanno2 ->
      let fanno = combine_fannos fanno1 fanno2 in
      Farrow fanno
    | Find clist1, Find clist2 ->
      let clist =
        let g c1 c2 =
          assert (c1.cstr_deg = c2.cstr_deg);
          assert (c1.cstr_id = c2.cstr_id);
          let cstr_type = combine_fanno_types c1.cstr_type c2.cstr_type in
          {c1 with cstr_type}
        in
        List.map2_exn clist1 clist2 g
      in
      Find clist
    | _ -> raise (Analysis_error "Type mismatch when combining annotated types")

  and combine_fannos fanno1 fanno2 =
    let targs = fanno1.fan_targs in
    let tres = fanno1.fan_tres in
    assert (fanno2.fan_targs = targs);
    assert (fanno2.fan_tres = tres);
    let fan_map  fts =
      let (ft1, fa1) = apply_fanno fanno1 fts in
      let (ft2, fa2) = apply_fanno fanno2 fts in
      let ft = combine_fanno_types ft1 ft2 in
      let fa rtanno metric =
        let atanno1 = fa1 rtanno metric in
        let atanno2 = fa2 rtanno metric in
        Anno.combine_tannos atanno1 atanno2
      in
      (ft, fa)
    in
    {fanno1 with fan_map}


  let combine_forward_results fr1 fr2 =
    match fr1, fr2 with
    | Fclosed ft1, Fclosed ft2 ->
      Fclosed (combine_fanno_types ft1 ft2)
    | Fopen open_ft1, Fopen open_ft2 ->
      let open_ft fts =
        let ft1 = open_ft1 fts in
        let ft2 = open_ft2 fts in
        combine_fanno_types ft1 ft2
      in
      Fopen open_ft
    | _,_ -> raise (Analysis_error "Forward result mismatch (open/closed) in branching.")


  let make_fanno_type analyze rtype =

    let rec make_fanno_type rtype =
      match rtype with
      | Tbase bt -> Fbase bt
      | Tnat -> Fnat
      | Tvar vid -> Fvar vid
      | Tarray t -> Farray (ref (Rleaf (make_fanno_type t)))
      | Tref t -> Fref (ref (Rleaf (make_fanno_type t)))
      | Ttuple ts -> Ftuple (List.map ts (make_fanno_type))
      | Tarrow (targs,tres,a) -> Farrow (make_fanno targs tres)
      | Tind clist ->  Find (
          List.map clist
            (fun c -> {c with cstr_type = make_fanno_type c.cstr_type})
        )

    and make_fanno targs tres =
      let fan_map fts =
        let ft =
          make_fanno_type tres
        in
        (ft, analyze targs tres)
      in
      { fan_map
      ; fan_targs = targs
      ; fan_tres = tres
      }
    in
    make_fanno_type rtype


  let unit_fanno_type rtype =
    let analyze targs tres rtanno metric =
      assert (rtanno.tan_type = tres);
      Anno.fresh_tanno rtanno.tan_deg (Ttuple targs)
    in
    make_fanno_type analyze rtype


  let zero_fanno_type rtype =
    let analyze targs tres rtanno metric =
      assert (rtanno.tan_type = tres);
      let () = Anno.zero_out_tanno ~spare_zero:true rtanno in
      let qzero = tan_find rtanno (zero_index tres) in
      Anno.fresh_zero_tanno ~qzero rtanno.tan_deg (Ttuple targs)
    in
    make_fanno_type analyze rtype



  let fold_fanno_type ft cid t_ind =
    match t_ind with
    | Tind clist ->
      begin
        match List.find clist (fun d -> d.cstr_id = cid) with
        | None ->
          raise (Anno_exn "Constructor not part of type declaration.")
        | Some d ->
          let deg = d.cstr_deg in
          if deg = 0 then
            let f c =
              if c.cstr_id = cid then
                let _ = assert (c.cstr_type = (unroll_fanno_type ft)) in
                {c with cstr_type = ft}
              else
                {c with cstr_type = unit_fanno_type c.cstr_type}
            in
            Find (List.map clist f)
          else
            begin
              match ft with
              | Ftuple (ft1::ft2::fts) ->
                assert ((unroll_fanno_type ft2) = t_ind);
                let ft_ind =
                  List.fold fts ~init:ft2 ~f:combine_fanno_types
                in
                let f c =
                  if c.cstr_id = cid then
                    {c with cstr_type = combine_fanno_types ft1 c.cstr_type}
                  else
                    c
                in
                begin
                  match ft_ind with
                  | Find aclist -> Find (List.map aclist f)
                  | _ -> raise (Anno_exn "Dead code.")
                end
              | _ ->
                raise (Anno_exn "Expection tuple type when folding a constructor of deg > 0.")
            end
      end
    | _ -> raise (Anno_exn "Folding to a non-inductive type.")


  let op_fanno_type op t =
    match t with
    | Tarrow ([targ], tres ,()) ->
      let fanno =
        let fan_map fts =
          match fts with
          | [ft] ->
            let ftres =
              make_fanno_type (fun _ -> raise (Analysis_error "Expecting a base type.")) tres
            in
            let analyze =
              match targ with
              | Ttuple [Tnat;Tnat] ->
                fun rtanno metric ->
                  assert (
                    match rtanno.tan_type with
                    | Tbase _ -> true
                    | _ -> false
                  );
                  let qzero = tan_find rtanno Iunit in
                  let deg = rtanno.tan_deg in
                  let atanno = Anno.fresh_tanno ~qzero deg (Ttuple [targ]) in
                  let cost = metric (Mbop_eval op) in
                  Anno.add_cost_t atanno cost
              | _ ->
                fun rtanno metric ->
                  let cost = metric (Mbop_eval op) in
                  Anno.constant_tanno cost (Ttuple [targ]) rtanno
            in
            (ftres, analyze)
          | _ ->
            raise (Analysis_error "Expecting one function type.")
        in
        { fan_targs = [targ]
        ; fan_tres = tres
        ; fan_map
        }
      in
      Farrow fanno
    | _ -> raise (Analysis_error "Expecting function type with one argument.")


  let bf_fanno_type bf bf_type =
    match bf with
    | Nat_succ ->
      assert (bf_type = Tarrow ([Tnat], Tnat, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [ft] ->
            assert (ft = Fnat);
            let analyze rtanno metric =
              let atanno = Anno.nat_shift_t rtanno in
              let cost = metric (Mbfun_eval Nat_succ) in
              Anno.add_cost_t atanno cost
            in
            (Fnat, analyze)
          | _ ->
            raise (Analysis_error "Expecting exectly one function type for built-in functions.")
        in
        { fan_targs = [Tnat]
        ; fan_tres = Tnat
        ; fan_map
        }
      in
      Farrow fanno

    | Nat_to_int ->
      assert (bf_type = Tarrow ([Tnat], Tbase Tint, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [Fnat] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = Tbase Tint);
              let qzero = tan_find rtanno Iunit in
              let deg = rtanno.tan_deg in
              let atanno = Anno.fresh_tanno ~qzero deg (Ttuple [Tnat]) in
              let () = amode_zero_out_tanno ~spare_zero:true atanno in
              let cost = metric (Mbfun_eval Nat_to_int) in
              Anno.add_cost_t atanno cost
            in
            (Fbase Tint, analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = [Tnat]
        ; fan_tres = Tbase Tint
        ; fan_map
        }
      in
      Farrow fanno

    | Arr_make ->
      begin
        match bf_type with
        | Tarrow ([Ttuple [Tnat;t_elem]], Tarray t_elem', ()) when t_elem = t_elem' ->
          let fanno =
            let fan_map fts =
              match fts with
              | [Ftuple [Fnat;ft_elem]] ->
                assert (t_elem = fanno_to_raml_type ft_elem);
                let analyze rtanno metric =
                  assert (rtanno.tan_type = Tarray t_elem);
                  let linear_cost = metric Marr_make_elem in
                  let atanno = Anno.arr_make rtanno t_elem linear_cost in
                  let cost = metric (Mbfun_eval Arr_make) in
                  Anno.add_cost_t atanno cost
                in
                let rc = ref (Rleaf ft_elem) in
                (Farray rc, analyze)
              | _ ->
                raise (Analysis_error "Expecting function argument of type nat and an element type.")
            in
            { fan_targs = [Ttuple [Tnat;t_elem]]
            ; fan_tres = Tarray t_elem
            ; fan_map
            }
          in
          Farrow fanno
        | _ -> raise (Analysis_error "Wrong function type for Array.make.")
      end

    | Arr_length ->
      begin
        match bf_type with
        | Tarrow ([Tarray t_elem],Tnat, ()) ->
          let fanno =
            let fan_map fts =
              match fts with
              | [Farray rc] ->
                let ft_elem = rc_get rc in
                assert (t_elem = fanno_to_raml_type ft_elem);
                let analyze rtanno metric =
                  assert (rtanno.tan_type = Tnat);
                  let atanno = Anno.arr_length rtanno t_elem in
                  let cost = metric (Mbfun_eval Arr_length) in
                  Anno.add_cost_t atanno cost
                in
                (Fnat, analyze)
              | _ ->
                raise (Analysis_error "Expecting function argument of array type.")
            in
            { fan_targs = [Tarray t_elem]
            ; fan_tres = Tnat
            ; fan_map
            }
          in
          Farrow fanno
        | _ -> raise (Analysis_error "Wrong function type for Array.make.")
      end

    | Arr_set ->
      begin
        match bf_type with
        | Tarrow ([Ttuple [Tarray t_elem;Tnat;t_elem']],Tbase Tunit, ()) when t_elem = t_elem' ->
          let fanno =
            let fan_map fts =
              match fts with
              | [Ftuple [Farray rc; Fnat; ft_elem]] ->
                assert (t_elem = fanno_to_raml_type ft_elem);
                assert (t_elem = fanno_to_raml_type (rc_get rc));
                let _ = rc_update (fun ft -> Rleaf (combine_fanno_types ft ft_elem)) rc in
                let analyze rtanno metric =
                  assert (rtanno.tan_type = Tbase Tunit);
                  let qzero = tan_find rtanno Iunit in
                  let deg = rtanno.tan_deg in
                  let t_arg = Ttuple [Ttuple [Tarray t_elem;Tnat;t_elem]] in
                  let atanno = Anno.fresh_tanno ~qzero deg t_arg in
                  let () = amode_zero_out_tanno ~spare_zero:true atanno in
                  let cost = metric (Mbfun_eval Arr_set) in
                  Anno.add_cost_t atanno cost
                in
                (Fbase Tunit, analyze)
              | _ ->
                raise (Analysis_error "Expecting function argument of array type.")
            in
            { fan_targs = [Ttuple [Tarray t_elem;Tnat;t_elem]]
            ; fan_tres = Tbase Tunit
            ; fan_map
            }
          in
          Farrow fanno
        | _ -> raise (Analysis_error "Wrong function type for Array.make.")
      end

    | Arr_get ->
      begin
        match bf_type with
        | Tarrow ([Ttuple [Tarray t_elem;Tnat]],t_elem', ()) when t_elem = t_elem' ->
          let fanno =
            let fan_map fts =
              match fts with
              | [Ftuple [Farray rc; Fnat]] ->
                let ft_elem = rc_get rc in
                assert (t_elem = fanno_to_raml_type ft_elem);
                let analyze rtanno metric =
                  assert (rtanno.tan_type = t_elem);
                  let _ = Anno.zero_out_tanno ~spare_zero:true rtanno in
                  let qzero = tan_find rtanno (zero_index t_elem) in
                  let deg = rtanno.tan_deg in
                  let t_arg = Ttuple [Ttuple [Tarray t_elem; Tnat]] in
                  let atanno = Anno.fresh_tanno ~qzero deg t_arg in
                  let () = amode_zero_out_tanno ~spare_zero:true atanno in
                  let cost = metric (Mbfun_eval Arr_get) in
                  Anno.add_cost_t atanno cost
                in
                (ft_elem, analyze)
              | _ ->
                raise (Analysis_error "Expecting function argument of array type.")
            in
            { fan_targs = [Ttuple [Tarray t_elem;Tnat]]
            ; fan_tres = t_elem
            ; fan_map
            }
          in
          Farrow fanno
        | _ -> raise (Analysis_error "Wrong function type for Array.make.")
      end


    | Nat_of_int ->
      assert (bf_type = Tarrow ([Tbase Tint], Tnat, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [Fbase Tint] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = Tnat);
              let () = Anno.zero_out_tanno ~spare_zero:true rtanno in
              let cost = metric (Mbfun_eval Nat_of_int) in
              Anno.constant_tanno cost (Ttuple [Tbase Tint]) rtanno
            in
            (Fnat, analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = [Tbase Tint]
        ; fan_tres = Tnat
        ; fan_map
        }
      in
      Farrow fanno

    | Nat_of_intc n ->
      raise (Analysis_error "Dead code.")

    | Nat_add ->
      assert (bf_type = Tarrow ([Ttuple [Tnat; Tnat]], Tnat, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [Ftuple [Fnat; Fnat]] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = Tnat);
              let cost = metric (Mbfun_eval Nat_add) in
              let gamma = Anno.nat_add rtanno in
              Anno.add_cost_t gamma cost
            in
            (Fnat, analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = [Ttuple [Tnat; Tnat]]
        ; fan_tres = Tnat
        ; fan_map
        }
      in
      Farrow fanno

    | Nat_mult ->
      assert (bf_type = Tarrow ([Ttuple [Tnat; Tnat]], Tnat, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [Ftuple [Fnat; Fnat]] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = Tnat);
              let cost = metric (Mbfun_eval Nat_mult) in
              let gamma = Anno.nat_mult rtanno in
              Anno.add_cost_t gamma cost
            in
            (Fnat, analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = [Ttuple [Tnat; Tnat]]
        ; fan_tres = Tnat
        ; fan_map
        }
      in
      Farrow fanno

    | Nat_minus ->
      let arg_ts = [Ttuple [Tnat; Tnat]] in
      let tres =  Ttuple [Tnat;Tnat] in
      assert (bf_type = Tarrow (arg_ts, tres, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [Ftuple [Fnat; Fnat]] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = tres);
              let cost = metric (Mbfun_eval Nat_minus) in
              let gamma = Anno.nat_minus rtanno in
              Anno.add_cost_t gamma cost
            in
            (Ftuple [Fnat;Fnat], analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = arg_ts
        ; fan_tres = tres
        ; fan_map
        }
      in
      Farrow fanno

    | Nat_div_mod ->
      let arg_ts = [Ttuple [Tnat; Tnat]] in
      let tres =  Ttuple [Tnat;Tnat;Tnat] in
      assert (bf_type = Tarrow (arg_ts, tres, () ) );
      let fanno =
        let fan_map fts =
          match fts with
          | [Ftuple [Fnat; Fnat]] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = tres);
              let cost = metric (Mbfun_eval Nat_div_mod) in
              let gamma = Anno.nat_div_mod rtanno in
              Anno.add_cost_t gamma cost
            in
            (Ftuple [Fnat;Fnat;Fnat], analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = arg_ts
        ; fan_tres = tres
        ; fan_map
        }
      in
      Farrow fanno

    | Res_consume line_num ->
      let tres = Tbase Tunit in
      let targ =
        match bf_type with
        | Tarrow ([targ], Tbase Tunit, () ) -> targ
        | _ -> raise (Analysis_error "Cannot match type for consume.")
      in        
      let fanno =
        let fan_map fts =
          match fts with
          | [ftarg] ->
            let analyze rtanno metric =
              assert (rtanno.tan_type = tres);
              let cost = metric (Mbfun_eval (Res_consume line_num)) in
              let gamma = Anno.amode_consume_tanno rtanno (Ttuple [targ]) in
              (**Recording the function types**)
              let () =
                let fid = "consume @ L" ^ (Int.to_string line_num) in
                if not (phys_equal metric m_costfree) then
                  !record_consume_type fid gamma rtanno
              in
              (****)
              Anno.add_cost_t gamma cost
            in
            (Fbase Tunit, analyze)
          | _ ->
            raise (Analysis_error "Expecting function argument of type nat.")
        in
        { fan_targs = [targ]
        ; fan_tres = tres
        ; fan_map
        }
      in
      Farrow fanno

    | Nat_minusc _
    | Ref_swap ->

      let f_str = string_of_builtin_fun bf in
      failwith ("Built-in function " ^ f_str ^ " not implemented yet.")

  let unstack_type exp =
    let t_exp = exp.exp_type in
    match exp.exp_info with
    | [] -> t_exp
    | ts -> Tarrow (ts, t_exp, ())


  let rec forward
    : fun_anno_context -> analysis_expression -> forward_result =
    fun sigma exp ->
      let tstack_exp = exp.exp_info in

      (* for debugging *)
      (* print_string "\nForward:\n";
         Pprint.print_expression exp; *)

      match exp.exp_desc with

      | Ebase_const _ ->
        assert (tstack_exp = []);
        Fclosed (make_fanno_type (fun _ -> raise (Analysis_error "Expecting a base type.")) exp.exp_type)

      | Ebase_fun (Nat_of_intc n) ->
        assert (tstack_exp = []);
        Fclosed (make_fanno_type (fun _ -> raise (Analysis_error "Expecting a base type.")) exp.exp_type)

      | Ebase_fun fbase ->
        let ft =
          let t = unstack_type exp in
          bf_fanno_type fbase t
        in
        open_fanno_type tstack_exp ft

      | Ebase_op op ->
        let t =
          match tstack_exp with
          | [] -> exp.exp_type
          | _::_ -> Tarrow (tstack_exp, exp.exp_type, ())
        in
        let ft = op_fanno_type op t in
        open_fanno_type tstack_exp ft

      | Evar x ->
        open_fanno_type tstack_exp (Map.find_exn sigma x)

      | Eapp (name,e,es) ->
        let xs = List.map es get_var in
        begin
          match forward sigma e with
          | Fopen op_ft ->
            let fts_xs = List.map xs (Map.find_exn sigma) in
            open_fanno_type tstack_exp (op_ft fts_xs)
          | Fclosed _ -> raise (Analysis_error "Expecting open forword result.")
        end

      | Elambda ((x,t_x),e) ->
        begin
          match tstack_exp with
          | [] ->
            let fan_map fts =
              match fts with
              | ft_x::fts ->
                let sigma' = Map.set sigma x ft_x in
                let ft_ret =
                  match forward sigma' e with
                  | Fopen open_ftype ->
                    open_ftype fts
                  | Fclosed ftype ->
                    assert (fts = []);
                    ftype
                in
                let analyze_body rtanno metric =
                  assert (e.exp_type = rtanno.tan_type);
                  let arg =
                    { anl_deg = rtanno.tan_deg
                    ; anl_metric = metric
                    ; anl_exp = e
                    ; anl_rtanno = rtanno
                    ; anl_level = record_fun_max_level
                    ; anl_tstack = fts
                    ; anl_sigma = sigma'
                    }
                  in
                  let gamma = backward arg in
                  let gamma' = Anno.stack_variable gamma x t_x in
                  Anno.bind_arguments gamma'
                in
                (ft_ret, analyze_body)

              | [] -> raise (Analysis_error "Function annotations are expecting arguments.")
            in
            begin
              match exp.exp_type with
              | Tarrow (targs, tres, _) ->
                let fanno =
                  { fan_map
                  ; fan_targs = targs
                  ; fan_tres = tres
                  }
                in
                Fclosed (Farrow fanno)
              | _ -> raise (Analysis_error "Expecting function type.")
            end

          | _ ->
            let open_fanno fts =
              match fts with
              | ft_x::fts ->
                let sigma' = Map.set sigma x ft_x in
                let ft_ret =
                  match forward sigma' e with
                  | Fopen open_ftype ->
                    open_ftype fts
                  | Fclosed ftype ->
                    assert (fts = []);
                    ftype
                in
                ft_ret

              | [] -> raise (Analysis_error "Open function annotations are expecting arguments.")
            in
            Fopen open_fanno
        end

      | Elet (x_opt,e1,e2) ->
        assert (e1.exp_info = []);
        begin
          match x_opt with
          | None ->
            let _ = forward_closed sigma e1 in (* side-effects of ref assign *)
            forward sigma e2
          | Some (x,_) ->
            let ft_x = forward_closed sigma e1 in
            let sigma' = Map.set sigma x ft_x in
            forward sigma' e2
        end

      | Eletrec (xts,es,e) ->
        assert (List.fold es ~init:true ~f:(fun acc e -> (e.exp_info = []) && acc));
        let fts = forward_rec_bind sigma xts es in
        let xs = List.map xts fst in
        let sigma' = Toolbox.map_add_list sigma xs fts in
        forward sigma' e

      | Econd (e,e1,e2) ->
        let fr1 = forward sigma e1 in
        let fr2 = forward sigma e2 in
        combine_forward_results fr1 fr2

      | Eshare (e1,(x1,_),(x2,_),e2) ->
        let x = get_var e1 in
        let ft_x = Map.find_exn sigma x in
        let sigma' =
          let sigma' = Map.set sigma x1 ft_x in
          Map.set sigma' x2 ft_x
        in
        forward sigma' e2

      | Econst (c,e) ->
        assert (tstack_exp = []);
        let x = get_var e in
        Fclosed (fold_fanno_type (Map.find_exn sigma x) c exp.exp_type)

      | Ematch (e,matches) ->
        let x = get_var e in
        let ft_x = Map.find_exn sigma x in
        let forward_match (c,yts,e) =
          let fts_ys =
            match yts, unfold_ftype ft_x c with
            | [yt], ft -> [ft]
            | yt::yts, Ftuple fts -> fts
            | _, _ -> raise (Analysis_error "Typemismatch when undfolding of inductive type.")
          in
          let ys = List.map yts fst in
          let sigma' = Toolbox.map_add_list sigma ys fts_ys in
          forward sigma' e
        in
        let frs = List.map matches forward_match in
        let init = List.hd_exn frs in
        let frs' = List.tl_exn frs in
        List.fold frs' ~init ~f:combine_forward_results

      | Enat_match (e,e1,(y,t_y),e2) ->
        let x = get_var e in
        assert (Map.find_exn sigma x = Fnat);
        let ft1 = forward sigma e1 in
        let sigma' = Map.set sigma y Fnat in
        let ft2 = forward sigma' e2 in
        combine_forward_results ft1 ft2

      | Eref e ->
        assert (tstack_exp = []);
        let x = get_var e in
        let rc = ref (Rleaf (Map.find_exn sigma x)) in
        Fclosed (Fref rc)

      | Eref_deref e ->
        let x = get_var e in
        let ft =
          match Map.find_exn sigma x with
          | Fref rc -> rc_get rc
          | _ -> raise (Analysis_error "Expecting ref type.")
        in
        open_fanno_type tstack_exp ft

      | Eref_assign (e1,e2) ->
        assert (tstack_exp = []);
        let x1 = get_var e1 in
        let x2 = get_var e2 in
        let rc =
          match Map.find_exn sigma x1 with
          | Fref rc -> rc
          | _ -> raise (Analysis_error "Expecting ref type.")
        in
        let ft2 = Map.find_exn sigma x2 in
        let _ = rc_update (fun ft1 -> Rleaf (combine_fanno_types ft1 ft2)) rc in
        Fclosed (Fbase Tunit)

      | Etuple es ->
        assert (tstack_exp = []);
        let xs = List.map es get_var in
        let fts = List.map xs (Map.find_exn sigma) in
        Fclosed (Ftuple fts)

      | Etuple_match (e1,yts,e2) ->
        let x = get_var e1 in
        let fts =
          match Map.find_exn sigma x with
          | Ftuple fts -> fts
          | _ -> raise (Analysis_error "Expecting tuple type.")
        in
        let ys = List.map yts fst in
        let sigma' = Toolbox.map_add_list sigma ys fts in
        forward sigma' e2

      | Eundefined ->
        let ft = unit_fanno_type exp.exp_type in
        open_fanno_type tstack_exp ft

      | Etick _ ->
        assert (tstack_exp = []);
        Fclosed (Fbase Tunit)

  and forward_closed sigma exp =
    match forward sigma exp with
    | Fclosed ft -> ft
    | _ -> raise (Analysis_error "Expecting closed forward result.")


  and forward_rec_bind
    : fun_anno_context
      -> var_bind list
      -> analysis_expression list
      -> fun_anno_type list =

    let equalize_fanno_types ft1 ft2 =
      if compare_fanno_types ft1 ft2 then
        ()
      else
        raise (Analysis_error ("Polymporphic recursion is currently not supported."
                               ^ "Check the functional arguments."))
    in

    let rec fix_annos rtype =
      match rtype with
      | Tbase bt -> Tbase bt
      | Tnat -> Tnat
      | Tvar vid -> Tvar vid
      | Tarray t -> Tarray (fix_annos t)
      | Tref t -> Tref (fix_annos t)
      | Ttuple ts -> Ttuple (List.map ts fix_annos)
      | Tarrow (targs,tres,a) ->
        Tarrow
          (List.map targs fix_annos
          , fix_annos tres
          , ref Rempty )
      | Tind clist ->  Tind (
          List.map clist
            (fun c -> {c with cstr_type = fix_annos c.cstr_type})
        )
    in

    let update_fixed_rtype fixed_metric t_fixed t_body =

      let rec update_fixed t_fixed t_body =
        match t_fixed, t_body with
        | Tbase bt1, Fbase bt2 when bt1 = bt2 ->
          ()
        | Tnat, Fnat ->
          ()
        | Tvar vid1, Fvar vid2 when vid1 = vid2 ->
          ()
        | Tarray t, Farray rc ->
          update_fixed t (rc_get rc)
        | Tref t, Fref rc ->
          update_fixed t (rc_get rc)
        | Ttuple t1s, Ftuple t2s ->
          List.iter2_exn t1s t2s update_fixed
        | Tarrow (targs,tret,rec_anno_ref), Farrow fanno_body ->
          assert ( ( List.map targs (t_map (fun _ -> ()))) = fanno_body.fan_targs );
          assert ( (t_map (fun _ -> ()) tret) = fanno_body.fan_tres );
          begin
            match !rec_anno_ref with
            | Rempty -> rec_anno_ref := Rready fanno_body
            | Rused (fixed_fts, fixed_atanno, fixed_rtanno) ->
              let (ft_body, fa_body) = apply_fanno fanno_body fixed_fts in
              let atanno = fa_body fixed_rtanno fixed_metric in
              Anno.equalize_tannos atanno fixed_atanno;
              update_fixed tret ft_body;
              rec_anno_ref := Rapplied (fixed_fts, fixed_atanno, fixed_rtanno)
            | _ -> raise (Analysis_error "Trying to update a let_rec_anno that is not empty.")
          end
        | Tind clist1, Find clist2 ->
          let g c1 c2 =
            assert (c1.cstr_deg = c2.cstr_deg);
            assert (c1.cstr_id = c2.cstr_id);
            update_fixed c1.cstr_type c2.cstr_type
          in
          List.iter2_exn clist1 clist2 g
        | _ -> raise (Analysis_error "Type mismatch")
      in
      update_fixed t_fixed t_body
    in

    let rec contains_rused t_fixed =
      match t_fixed with
      | Tbase _
      | Tnat
      | Tvar _ -> false
      | Tarray t
      | Tref t -> contains_rused t
      | Ttuple ts -> List.exists ts contains_rused
      | Tind clist ->
        List.exists clist
          (fun c -> contains_rused c.cstr_type)
      | Tarrow (targs_fixed,tret_fixed,rec_anno_ref) ->
        let b1 = List.exists targs_fixed contains_rused in
        let b2 = contains_rused tret_fixed in
        let b3 =
          match !rec_anno_ref with
          | Rused _ -> true
          | _ -> false
        in
        b1 && b2 && b3
    in

    let allow_rused = ref true in

    let make_fun_annos ?(poly_rec=false) fixed_metric max_deg t_fixed ft' =

      assert (max_deg >= 0);

      let rec rec_fannos t_fixed ft' =
        match t_fixed, ft' with
        | Tbase bt1, Fbase bt2 when bt1 = bt2 ->
          Fbase bt1
        | Tnat, Fnat ->
          Fnat
        | Tvar vid1, Fvar vid2 when vid1 = vid2 ->
          Fvar vid1
        | Tarray t1, Farray rc ->
          let t2 = rc_get rc in
          let rc =  ref (Rleaf (rec_fannos t1 t2)) in
          Farray rc
        | Tref t1, Fref rc ->
          let t2 = rc_get rc in
          let rc = ref (Rleaf (rec_fannos t1 t2)) in
          Fref rc
        | Ttuple t1s, Ftuple t2s ->
          Ftuple (List.map2_exn t1s t2s rec_fannos)
        | Tind clist1, Find clist2 ->
          let g c1 c2 =
            assert (c1.cstr_deg = c2.cstr_deg);
            assert (c1.cstr_id = c2.cstr_id);
            let cstr_type = rec_fannos c1.cstr_type c2.cstr_type in
            {c1 with cstr_type}
          in
          Find (List.map2_exn clist1 clist2 g)
        | Tarrow (targs_fixed,tret_fixed,rec_anno_ref), Farrow fanno' ->

          let targs = List.map targs_fixed (t_map (fun _ -> ())) in
          let tres = t_map (fun _ -> ()) tret_fixed in

          let fanno =
            let fan_map fts =
              let (ft',fa') =
                (* if max_deg > 0 then *)
                apply_fanno fanno' fts
                (* else *)
                (*   raise (Analysis_error "Access to dumy annotation for degree 0.") *)
              in
              let fa fixed_atanno fixed_rtanno rtanno met =
                if rtanno.tan_deg < max_deg then
                  fa' rtanno met
                else begin
                  assert (phys_equal met fixed_metric);
                  assert (rtanno.tan_deg = max_deg);
                  Anno.equalize_tannos rtanno fixed_rtanno;
                  fixed_atanno
                end
              in
              let (ft,fa) =
                match !rec_anno_ref with
                | Rempty -> (*raise (Analysis_error "Trying to read empty let_rec_anno.")*)
                  ( rec_fannos tret_fixed ft'
                  , if !allow_rused then
                      fun rtanno met ->
                        if rtanno.tan_deg < max_deg then
                          fa' rtanno met
                        else begin
                          assert (phys_equal met fixed_metric);
                          assert (rtanno.tan_deg = max_deg);
                          let fixed_atanno = Anno.fresh_tanno max_deg (Ttuple targs) in
                          rec_anno_ref := Rused (fts, fixed_atanno, rtanno);
                          fixed_atanno
                        end
                    else
                      fun _ -> raise (Analysis_error "Trying to read empty let_rec_anno.")
                  )
                | Rready fanno_body ->
                  let fixed_rtanno = Anno.fresh_tanno max_deg tres in
                  let fixed_atanno = Anno.fresh_tanno max_deg (Ttuple targs) in
                  rec_anno_ref := Rapplied (fts, fixed_atanno, fixed_rtanno);
                  let (ft_body, fa_body) = apply_fanno fanno_body fts in
                  update_fixed_rtype fixed_metric tret_fixed ft_body;
                  let atanno = fa_body fixed_rtanno fixed_metric in
                  Anno.equalize_tannos atanno fixed_atanno;
                  (rec_fannos tret_fixed ft', fa fixed_atanno fixed_rtanno)
                | Rused (fixed_fts, fixed_atanno, fixed_rtanno)
                | Rapplied (fixed_fts, fixed_atanno, fixed_rtanno) ->
                  List.iter2_exn fts fixed_fts equalize_fanno_types;
                  (rec_fannos tret_fixed ft', fa fixed_atanno fixed_rtanno)
              in
              let fa =
                if not poly_rec || is_const_type (Ttuple targs) then
                  fa
                else
                  fun rtanno met ->
                    let cf_deg = max_deg - 1 in
                    (* attempt to deal with exceptions for lower/constant :
                      match Amode.mode with
                      | Mupper -> max_deg - 1
                      | Mlower
                      | Mconstant ->
                        (* PASS THROUGH for lower and constant *)
                        (* Needed here because of exceptions *)
                        if max_deg = 1 then
                          1
                        else
                          max_deg - 1
                    in *)
                    if cf_deg = 0 || rtanno.tan_deg < max_deg || (phys_equal met m_costfree) then
                      fa rtanno met
                    else begin
                      assert (phys_equal met fixed_metric);
                      assert (rtanno.tan_deg = max_deg);
                      let cf_rtanno = Anno.fresh_tanno cf_deg tres in
                      let cf_atanno = fa' cf_rtanno m_costfree in
                      let rec_rtanno = Anno.fresh_tanno max_deg tres in
                      let rec_atanno = fa rec_rtanno met in
                      let _ = Anno.add_tannos ~tan:rtanno cf_rtanno rec_rtanno in
                      Anno.add_tannos cf_atanno rec_atanno
                    end
              in
              (ft,fa)
            in
            { fan_targs = targs
            ; fan_tres = tres
            ; fan_map}
          in
          Farrow fanno
        | _,_ -> raise (Analysis_error "Type mismatch.")
      in
      rec_fannos t_fixed ft'
    in

    fun sigma xts es  ->

      assert(List.for_all es is_lambda);

      let (xs,ts) = List.unzip xts in

      let make_poly_types get_ftypes =

        let poly_type (y,t) =

          let rec final_tanno rtype get_ftype =
            match rtype with
            | Tbase bt -> Fbase bt
            | Tnat -> Fnat
            | Tvar vid -> Fvar vid
            | Tarray t ->
              let get_fanno' max_deg metric =
                match get_ftype max_deg metric with
                | Farray rc -> rc_get rc
                | _ -> raise (Analysis_error "Type mismatch.")
              in
              let rc = ref (Rleaf (final_tanno t get_fanno')) in
              Farray rc
            | Tref t ->
              let get_fanno' max_deg metric =
                match get_ftype max_deg metric with
                | Fref rc -> rc_get rc
                | _ -> raise (Analysis_error "Type mismatch.")
              in
              let rc = ref (Rleaf (final_tanno t get_fanno')) in
              Fref rc
            | Ttuple ts ->
              let get_fanno' n max_deg metric =
                match get_ftype max_deg metric with
                | Ftuple fts -> List.nth_exn fts n
                | _ -> raise (Analysis_error "Type mismatch.")
              in
              Ftuple (List.mapi ts (fun n t -> final_tanno t (get_fanno' n)))
            | Tind clist ->
              let get_fanno' cid max_deg metric =
                match get_ftype max_deg metric with
                | Find anno_clist ->
                  let constr =
                    let f c =  cid = c.cstr_id in
                    List.find_exn ~f anno_clist
                  in
                  constr.cstr_type
                | _ -> raise (Analysis_error "Type mismatch.")
              in
              Find (
                List.map clist
                  (fun c -> {c with cstr_type = final_tanno c.cstr_type (get_fanno' c.cstr_id)})
              )
            | Tarrow (targs,tret,()) ->
              let fan_map fts =
                let ft =
                  let get_fanno' max_deg metric =
                    match get_ftype max_deg metric with
                    | Farrow fanno' ->
                      fst (apply_fanno fanno' fts)
                    | _ -> raise (Analysis_error "Type mismatch.")
                  in
                  final_tanno tret get_fanno'
                in
                let fa rtanno metric =
                  match get_ftype rtanno.tan_deg metric with
                  | Farrow fanno ->
                    let (_,fa') = apply_fanno fanno fts in
                    fa' rtanno metric
                  | _ -> raise (Analysis_error "Type mismatch.")
                in
                (ft,fa)
              in
              let fanno =
                { fan_targs = targs
                ; fan_tres = tret
                ; fan_map
                }
              in
              Farrow fanno
          in

          let get_ftype degree metric =
            let fts = get_ftypes degree metric in
            snd (List.find_exn (List.zip_exn xs fts) ~f:(fun (x,ft) -> x = y))
          in

          final_tanno t get_ftype

        in

        List.map xts poly_type

      in

      let rec mono_type_annos degree metric =

        assert (degree >= 0);


        let fts' =
          make_poly_types mono_type_annos
        in


        let fixed_ts = List.map ts fix_annos in

        assert (not (List.exists fixed_ts contains_rused ));

        let make_fun_annos_save ?(poly_rec=false) fixed_t ft' =
          let ft = make_fun_annos ~poly_rec metric degree fixed_t ft' in
          assert (not (List.exists fixed_ts contains_rused ));
          ft
        in

        let sigma' =
          let fts = List.map2_exn fixed_ts fts' (make_fun_annos_save ~poly_rec:true) in
          Toolbox.map_add_list sigma xs fts
        in

        let forward_rec e =
          match forward sigma' e with
          | Fclosed ft -> ft
          | Fopen _ -> raise (Analysis_error "Expecting closed forward result.")
        in

        let _ =
          let f fixed_t e = update_fixed_rtype metric fixed_t (forward_rec e) in
          List.map2_exn fixed_ts es ~f
        in

        List.map2_exn fixed_ts fts' make_fun_annos_save

      in

      let result = make_poly_types mono_type_annos in

      (* Problem: The function bodies es are only traversed by 'froward' if
       * an application of one the functions is analyzed by 'backward'. This
       * brakes the top down order of forward which is needed for the treatment
       * of references.
       * We need to change the annotations of recursive functions to ensure the right
       * order of the forward traversal. This is done by patch_forward.
       * We also add an additional argument to 'forward' to disable reference
       * analysis when calling forward inside 'forward_rec_bind'.
      *)

      let patch_forward fts =
        let sigma = Toolbox.map_add_list sigma xs fts in
        let patch ft e =
          match ft,e.exp_desc with
          | Farrow fanno, Elambda ((x,t_x),e) ->
            let fan_map fts =
              assert ( (List.map fts fanno_to_raml_type) = fanno.fan_targs );
              match fts with
              | ft_x::fts' ->
                let sigma' = Map.set sigma x ft_x in
                let _ = forward sigma' e in (* for the ref_assign side-effects *)
                apply_fanno fanno fts
              | [] -> raise (Analysis_error "Expecting functions arguments.")
            in
            Farrow {fanno with fan_map}
          | _,_ -> raise (Analysis_error "Expecting lambda abstraction.")
        in
        List.map2_exn fts es ~f:patch
      in

      patch_forward result







  and backward
    : analysis_arg -> var context_anno =
    fun arg ->

      let
        { anl_rtanno = arg_rtanno
        ; anl_deg = arg_deg
        ; anl_metric = arg_metric
        ; anl_level = arg_level
        ; anl_sigma = arg_sigma
        ; anl_tstack = arg_tstack
        }
        = arg
      in

      (* this is redundant *)
      assert(arg_deg = arg_rtanno.tan_deg);
      assert(arg_deg >= 0);

      (* for debugging *)
      (* print_string "\nBackward:\n";
         Pprint.print_expression arg.anl_exp; *)

      let metric c =
        match arg.anl_exp.exp_kind with
        |     Efree -> 0.0
        | Enormal -> arg.anl_metric c
      in

      let use_fun_type ft =
        let (_, backward_op) =
          match ft with
          | Farrow fanno -> apply_fanno fanno arg_tstack
          | _ -> raise (Analysis_error "Expecting a function type.")
        in
        let backward_op =
          (* For constant and lower pass through is done using cost free types *)
          (* Compare PASS THROUGH comment*)
          (* comment in the next line to correctly deal with excpetions *)
          if app_const_pass_through (*&& Amode.mode = Mupper*) then
            Anno.add_const backward_op
          else
            backward_op
        in
        let atanno = backward_op arg_rtanno arg_metric in
        let fun_name =
          match arg.anl_exp.exp_desc with
          | Evar y -> y
          | _ -> "anonymous"
        in
        !record_fun_type arg_level fun_name atanno arg_rtanno;
        atanno
      in

      (* using simply the variable types in e is a problem *)
      let get_context () =
        let fvs = free_vars arg.anl_exp in
        let f ~key:y ~data:ft_y =
          let t_y = unroll_fanno_type ft_y in
          if Set.mem fvs (y,t_y) then
            Some t_y
          else
            None
        in
        Map.filter_mapi arg_sigma ~f
      in

      (* Expression type fits type of the annotation. *)
      assert (arg.anl_exp.exp_type = arg_rtanno.tan_type);

      if arg_deg = 0 then
        let qzero = tan_find arg_rtanno (zero_index arg_rtanno.tan_type) in
        let context = get_context () in
        let tstack = List.map arg_tstack fanno_to_raml_type in
        Anno.fresh_canno ~qzero 0 tstack context
      else

        let the_canno =
          match (arg.anl_exp).exp_desc with

          (* Note that the return type can be nat because of
             Czero. However there are no constraints on the annotations. *)
          | Ebase_const _ ->
            assert (arg_tstack = []);
            let cost = metric Mbase_const in
            Anno.constant_canno cost arg_rtanno

          | Ebase_fun (Nat_of_intc n) ->
            assert (arg_tstack = []);
            assert (arg_rtanno.tan_type = Tnat);
            let gamma = Anno.nat_of_int n arg_rtanno in
            let cost_load = metric Mbfun_load in
            let cost_eval = metric (Mbfun_eval Nat_of_int) in
            let cost_int = metric Mbase_const in
            let cost_app = metric (Mapp 1) in
            let cost = cost_load +. cost_eval +. cost_int  +. cost_app in
            Anno.add_cost gamma cost

          | Ebase_fun basef ->
            let cost_load = metric Mbfun_load in
            begin
              match arg_tstack with
              | [] ->
                Anno.constant_canno cost_load arg_rtanno
              | [ftarg] ->
                let ft = bf_fanno_type basef (unstack_type arg.anl_exp) in
                let tanno_args = use_fun_type ft in
                let gamma = Anno.canno_of_tanno tanno_args in
                Anno.add_cost gamma cost_load
              | _ ->
                raise (Analysis_error "Stack contains more than one argument in for built-in op.")
            end

          | Ebase_op op ->
            let cost_load = metric Mbop_load in
            begin
              match arg_tstack with
              | [] ->
                Anno.constant_canno cost_load arg_rtanno
              | [ftarg] ->
                let tres = arg.anl_exp.exp_type in
                assert begin
                  match ftarg with
                  | Fbase _ -> true
                  | Ftuple targs ->
                    let f t =
                      match t with
                      | Fbase tb -> true
                      | Fnat -> true
                      | _ -> false
                    in
                    List.for_all targs f
                  | _ -> false
                end;
                let targ = fanno_to_raml_type ftarg in
                let tarrow = Tarrow ([targ],tres,()) in
                let ft = op_fanno_type op tarrow in
                let tanno_args = use_fun_type ft in
                let gamma = Anno.canno_of_tanno tanno_args in
                Anno.add_cost gamma cost_load
              | _ ->
                raise (Analysis_error "Stack contains more than one argument in for built-in op.")
            end

          | Evar x ->
            let canno =
              match arg_tstack with
              | [] ->
                Anno.var_canno arg_rtanno x
              | _ ->
                let ft = Map.find_exn arg_sigma x in
                let t_x = unroll_fanno_type ft in
                let tanno_args = use_fun_type ft in
                assert begin
                  match tanno_args.tan_type with
                  | Ttuple ts ->
                    let tarrow = Tarrow (ts,arg.anl_exp.exp_type, ()) in
                    tarrow = t_x
                  | _ -> false
                end;
                Anno.canno_of_tanno ~xt:(x,t_x) tanno_args
            in
            Anno.add_cost canno (metric Mvar)


          | Elet (x_opt, e1, e2) ->
            let combine_with_gamma1 gamma2 =
              let ind_zero = zero_cindex (gamma2.can_tstack, gamma2.can_context) in
              let qzero = can_find gamma2 ind_zero in
              let tanno1 = Anno.fresh_tanno ~qzero arg_deg e1.exp_type in
              let () = amode_zero_out_tanno tanno1 ~spare_zero:true in
              let gamma1 = backward
                  { arg with anl_exp = e1
                           ; anl_rtanno = tanno1
                           ; anl_tstack = []
                           ; anl_level = arg_level + 1
                  }
              in
              let gamma = Anno.combine_cannos_let gamma1 gamma2 in
              Anno.add_cost gamma (metric Mlet)
            in
            begin
              match x_opt with
              | Some (x,t_x) ->
                let ft1 = forward_closed arg_sigma e1 in
                let sigma' = Map.set arg_sigma x ft1 in
                let gamma2 =
                  backward
                    {arg with
                     anl_sigma = sigma';
                     anl_exp = e2
                    }
                in
                assert (gamma2.can_tstack = List.map arg_tstack fanno_to_raml_type);
                begin
                  match Map.find gamma2.can_context x with
                  | None -> combine_with_gamma1 gamma2
                  | Some t_x ->
                    let anno_map =
                      let ind_zero_x = zero_index t_x in
                      let context = Map.remove gamma2.can_context x in
                      let add_ind (is,ci) anno_map =
                        let deg = arg_deg - (cdegree (is,ci)) in
                        assert (deg >= 0);
                        assert (e1.exp_type = t_x);
                        if deg = 0 then
                          let ci_x = Map.set ci x ind_zero_x in
                          let q = can_find gamma2 (is,ci_x)  in
                          Map.set anno_map (is,ci) (Left q)
                        else
                          let tanno =
                            let tan_type = t_x in
                            let tan_map i =
                              let ci' = Map.set ci x i in
                              can_find gamma2 (is,ci')
                            in
                            { tan_deg = deg; tan_type; tan_map }
                          in
                          let canno =
                            let (level, anl_metric) =
                              if deg = arg_deg then
                                (arg_level + 1, arg_metric)
                              else
                                (record_fun_max_level, m_costfree)
                            in
                            backward
                              { arg with anl_exp = e1
                                       ; anl_rtanno = tanno
                                       ; anl_metric
                                       ; anl_deg = deg
                                       ; anl_tstack = []
                                       ; anl_level = level
                              }
                          in
                          Map.set anno_map (is,ci) (Right canno)
                      in
                      cindices_max_deg ~empty:Cind_map.empty ~add_ind
                        gamma2.can_tstack context arg_deg
                    in
                    let tstack = gamma2.can_tstack in
                    let context2 = Map.remove gamma2.can_context x in
                    let context1 =
                      let ind_zero = zero_cindex (tstack, context2) in
                      match Map.find_exn anno_map ind_zero with
                      | Right canno1 -> canno1.can_context
                      | Left q -> raise (Analysis_error "Maximal degree seems to be zero.")
                    in
                    let gamma = Anno.canno_let (tstack,context2) arg_deg context1 anno_map in
                    Anno.add_cost gamma (metric Mlet)
                end

              | None ->
                let _ = forward_closed arg_sigma e1 in
                (* the above is needed for side effects in forward ref_assign*)
                let gamma2 = backward {arg with anl_exp = e2} in
                assert (gamma2.can_tstack = List.map arg_tstack fanno_to_raml_type);
                combine_with_gamma1 gamma2
            end

          | Eletrec (xts,es,e2) ->
            let fvs = free_vars arg.anl_exp in
            let context = (* using the variable types in e is a problem *)
              let f ~key:y ~data:ft_y =
                let t_y = unroll_fanno_type ft_y in
                if Set.mem fvs (y,t_y) then
                  Some t_y
                else
                  None
              in
              Map.filter_mapi arg_sigma ~f
            in
            let xs = List.map xts fst in
            let fts = forward_rec_bind arg_sigma xts es in
            let sigma' = Toolbox.map_add_list arg_sigma xs fts in
            let gamma2 =
              backward
                {arg with
                 anl_sigma = sigma';
                 anl_exp = e2
                }
            in
            let gamma = Anno.update_context gamma2 xts context in
            let cost =
              let n = List.length xs in
              let cost_letrec = metric (Mletrec n) in
              let cost_closures =
                let f acc e =
                  let n = Set.length (free_vars e) in
                  (metric (Mclosure n)) +. acc
                in
                List.fold es ~init:0.0 ~f
              in
              cost_letrec +. cost_closures
            in
            Anno.add_cost gamma cost


          | Eshare (e1,(x1,t1),(x2,t2),e2) ->
            let y = get_var e1 in
            let sigma' =
              let ft_y = Map.find_exn arg_sigma y in
              let sigma = Map.remove arg_sigma y in
              let sigma = Map.set sigma x1 ft_y in
              Map.set sigma x2 ft_y
            in
            let gamma2 = backward
                { arg with
                  anl_sigma = sigma';
                  anl_exp = e2;
                } (* keep same arg_level *)
            in
            Anno.share y (x1,x2) gamma2


          | Elambda ((x,t_x), e) ->
            begin
              match arg_tstack with
              | [] -> (* binding of arguments *)
                let qzero = arg_rtanno.tan_map Iunit in
                let fvs = free_vars arg.anl_exp in
                let context = get_context () in
                let gamma = amode_fresh_canno ~qzero arg_deg [] context in
                let n =  Set.length fvs in
                Anno.add_cost gamma (metric (Mclosure n))

              | ft::fts ->  (* keep arguments open *)
                let gamma_body = backward
                    { arg with
                      anl_exp = e;
                      anl_tstack = fts;
                      anl_sigma = Map.set arg_sigma x ft;
                      anl_level = arg_level + 1
                    }
                in
                Anno.stack_variable gamma_body x t_x
            end


          | Eapp (name, e, es) ->
            let xs = List.map es get_var in
            let ts = List.map es get_type in
            let fts_xs = List.map xs (Map.find_exn arg_sigma) in
            let get_gamma1 rtanno =
              let gamma =
                backward
                  { arg with
                    anl_exp = e
                  ; anl_level = arg_level + 1
                  ; anl_rtanno = rtanno
                  ; anl_tstack = fts_xs
                  }
              in
              Anno.unstack_variables gamma xs ts
            in
            begin
              match forward arg_sigma e with
              | Fopen open_ft ->
                begin
                  match arg_tstack with
                  | [] ->
                    let gamma = get_gamma1 arg_rtanno in
                    Anno.add_cost gamma (metric (Mapp (List.length es)))
                  | _ ->
                    let gamma2 =
                      let atanno = use_fun_type (open_ft fts_xs) in
                      Anno.canno_of_tanno atanno
                    in
                    let gamma1 =
                      let t_e = e.exp_type in
                      assert begin
                        match t_e with
                        | Tarrow _ -> true
                        | _ -> false
                      end;
                      let qzero = can_find_zero gamma2 in
                      let rtanno = Anno.fresh_tanno ~qzero arg_deg t_e in
                      (*t_e seems to be a function type*)
                      get_gamma1 rtanno
                    in
                    let gamma = Anno.combine_cannos_let gamma1 gamma2 in
                    let cost = metric (Mapp (List.length es)) in
                    Anno.add_cost gamma cost
                end
              | Fclosed _ -> raise (Analysis_error "Expecting open forward result.")
            end


          | Econd (e,e1,e2) ->
            let (x,t) = (get_var e, e.exp_type) in
            let gamma1 = backward
                { arg with
                  anl_exp = e1;
                  anl_level = arg_level + 1
                }
            in
            let gamma2 = backward
                { arg with
                  anl_exp = e2;
                  anl_level = arg_level + 1
                }
            in
            let (gamma1,gamma2) =
              match Anno.amode_patch_cannos [gamma1; gamma2] [[];[]] with
              | [g1; g2] -> (g1, g2)
              | _ -> raise (Analysis_error "Dead code.")
            in
            let gamma = Anno.combine_cannos_if ~add_var:(x,t) [gamma1; gamma2] in
            Anno.add_cost gamma (metric (Mcond))


          | Enat_match (e,e1,(x_m,t),e2) ->
            let x_e = get_var e in
            let gamma1 = backward
                { arg with
                  anl_exp = e1;
                  anl_level = arg_level + 1;
                }
            in
            let sigma' =
              let ft_e = Map.find_exn arg_sigma x_e in
              let sigma = Map.remove arg_sigma x_e in
              Map.set sigma x_m ft_e
            in
            let gamma2 = backward
                { arg with
                  anl_exp = e2;
                  anl_sigma = sigma';
                  anl_level = arg_level + 1;
                }
            in
            let (gamma1,gamma2) =
              match Anno.amode_patch_cannos [gamma1; gamma2] [[]; [x_m]] with
              | [g1; g2] -> (g1, g2)
              | _ -> raise (Analysis_error "Dead code.")
            in
            let gamma2_shift = Anno.nat_shift x_m gamma2 x_e in
            let gamma1' =
              Anno.amode_patch_canno ~unconstraint:true [(x_e,Tnat)] gamma1
            in
            let gamma = Anno.combine_cannos_if [gamma1'; gamma2_shift] in
            Anno.add_cost gamma (metric Mnat_match)


          | Econst (cid,e) ->
            assert (arg_tstack = []);
            let x = get_var e in
            let gamma = Anno.ind_shift_t cid x arg_rtanno in
            Anno.add_cost gamma (metric Mconst)


          | Ematch (e,matches) ->
            let (x_e, t_e) = (get_var e, e.exp_type) in
            let cannos_pre_shift =
              let match_canno (cid, xts, e) =
                let xs = List.map xts fst in
                let sigma' =
                  let ts_c  =
                    let ftind = Map.find_exn arg_sigma x_e in
                    match xs, unfold_ftype ftind cid with
                    | [x], t_c -> [t_c]
                    | _::_, Ftuple ts_c  -> ts_c
                    | _ -> raise (Anno_exn "Type mismatch in pattern match.")
                  in
                  let sigma = Map.remove arg_sigma x_e in
                  Toolbox.map_add_list sigma xs ts_c
                in
                let canno = backward
                    { arg with
                      anl_exp = e;
                      anl_sigma = sigma';
                      anl_level = arg_level + 1
                    }
                in
                let cont_shift canno =
                  Anno.ind_shift xts canno (cid,t_e) x_e
                in
                (canno, xs, cont_shift)
              in
              List.map matches match_canno
            in
            let (gammas, bound_vars, conts) = unzip3 cannos_pre_shift in
            let gammas = Anno.amode_patch_cannos gammas bound_vars in
            let gammas' =
              List.map2_exn conts gammas apply
            in
            let gamma = Anno.combine_cannos_if gammas' in
            Anno.add_cost gamma (metric (Mmatch (List.length matches)))


          | Etuple_match (e1,xts,e2) ->
            let y = get_var e1 in
            let xs = List.map xts fst in
            let sigma' =
              match Map.find_exn arg_sigma y with
              | Ftuple ts -> Toolbox.map_add_list arg_sigma xs ts
              | _ -> raise (Analysis_error "Expecting tuple type.")
            in
            let gamma2 = backward
                { arg with
                  anl_exp = e2;
                  anl_sigma = sigma';
                  anl_level = arg_level + 1
                }
            in
            let gamma = Anno.fold_tuple y gamma2 xts in
            Anno.add_cost gamma (metric (Mtuple_match (List.length xts)))


          | Etuple es ->
            assert (arg_tstack = []);
            let xs = List.map es get_var in
            let gamma = Anno.unfold_tuple xs arg_rtanno in
            Anno.add_cost gamma (metric (Mtuple (List.length es)))


          | Eref e ->
            assert (arg_tstack = []);
            let x = get_var e in
            let t_x = e.exp_type in
            let qzero = arg_rtanno.tan_map Iunit in
            let context = String.Map.singleton x t_x in
            let gamma = amode_fresh_canno ~qzero arg_deg [] context in
            Anno.add_cost gamma (metric Mref)


          | Eref_deref e ->
            let x = get_var e in
            let t_x = e.exp_type in
            begin
              match arg_tstack with
              | [] ->
                let ret_type = arg_rtanno.tan_type in
                let _ = Anno.zero_out_tanno ~spare_zero:true arg_rtanno in
                let qzero = arg_rtanno.tan_map (zero_index ret_type) in
                let context = String.Map.singleton x (Tref ret_type) in
                let gamma = Anno.fresh_canno ~qzero arg_deg [] context in
                Anno.add_cost gamma (metric Mref_deref)
              | _ ->
                let ft =
                  match Map.find_exn arg_sigma x with
                  | Fref rc -> rc_get rc
                  | _ -> raise (Analysis_error "Expecting ref type.")
                in
                let tanno_args = use_fun_type ft in
                let gamma = Anno.canno_of_tanno ~xt:(x,t_x) tanno_args in
                Anno.add_cost gamma (metric Mref_deref)
            end


          | Eref_assign (e1,e2) ->
            assert( arg_tstack = [] );
            assert( arg_rtanno.tan_type = (Tbase Tunit) );
            let x1 = get_var e1 in
            let x2 = get_var e2 in
            let qzero = arg_rtanno.tan_map Iunit in
            assert ( e1.exp_type = (Tref e2.exp_type) );
            let t = e2.exp_type in
            let context =
              String.Map.of_alist_exn [(x1,Tref t);(x2,t)]
            in
            let gamma = amode_fresh_canno ~qzero arg_deg [] context in
            Anno.add_cost gamma (metric Mref_assign)


          | Eundefined ->
            let tstack = arg.anl_exp.exp_info in
            amode_fresh_canno arg_deg tstack String.Map.empty

          | Etick q ->
            assert (arg_tstack = []);
            let cost = metric (Mtick q) in
            Anno.constant_canno cost arg_rtanno

        in
        (* the type stack of the expression fits the type stack of the context annotation *)
        assert (the_canno.can_tstack = List.map arg_tstack unroll_fanno_type);
        assert (the_canno.can_tstack = arg.anl_exp.exp_info);
        the_canno




  let analyze_expression ~degree ~metric ?(collect_fun_types = Pnone) exp =
    let rtanno = Anno.fresh_tanno degree exp.exp_type in
    let () = amode_zero_out_tanno rtanno in
    let arg =
      { anl_exp = exp
      ; anl_metric = metric
      ; anl_deg = degree
      ; anl_rtanno = rtanno
      ; anl_sigma = String.Map.empty
      ; anl_tstack = []
      ; anl_level = 0
      }
    in
    init_type_collection collect_fun_types;
    let canno = backward arg in
    let q0 = can_find canno ([], String.Map.empty) in
    let _ = Anno.S.add_objective q0 1.0 in
    match Anno.S.first_solve () with
    | Feasible ->
      let get_solution = Anno.S.get_solution in
      let update_tanno tanno =
        tanno_map get_solution tanno
      in
      let fun_types = List.map !recorded_fun_types
          (fun (fid,atanno,rtanno) -> (fid, update_tanno atanno, update_tanno rtanno))
      in
      Some (get_solution q0, fun_types)
    | Infeasible -> None


  exception LP_infeasable


  let analyze_function ~degree ~metric ?(collect_fun_types = Pnone) exp =

    let solve_with_obj solver_method qs =
      let module VMap = Anno.S.VMap in
      let objective =
        let f vmap q =
          VMap.set vmap q 1.0
        in
        List.fold qs ~init:VMap.empty ~f
      in
      let () = Anno.S.set_objective objective in
      match solver_method () with
      | Infeasible -> raise LP_infeasable
      | Feasible ->
        let obj_val = Anno.S.get_objective_val () in
        let obj_list = Anno.S.VMap.to_alist objective in
        Anno.S.add_constr_list ~lower:0.0 ~upper:obj_val obj_list
    in

    let sigma = String.Map.empty in
    match forward sigma exp, exp.exp_type with
    | Fopen _ , _ ->
      raise (Analysis_error "Expecting closed forward result.")
    | Fclosed (Farrow fanno), Tarrow (targs,tres,_) ->
      assert (fanno.fan_targs = targs);
      assert (fanno.fan_tres = tres);
      let () = init_type_collection collect_fun_types in
      let ftargs = List.map targs zero_fanno_type in
      let (_,fa) = fanno.fan_map ftargs in
      let atres = Anno.fresh_tanno degree tres in
      let () = amode_zero_out_tanno atres in
      let atarg = fa atres metric in
      let qss =
        let iss = prioritized_indices (Ttuple targs) degree in
        let f is =
          List.map ~f:(tan_find atarg)  is
        in
        List.map ~f iss
      in
      begin
        match qss with
        | [] -> raise (Analysis_error "Empty index set.")
        | qs_max::qss ->
          try
            let () = solve_with_obj Anno.S.first_solve qs_max  in
            let () = List.iter ~f:(solve_with_obj Anno.S.resolve) qss in
            let update_tanno tanno =
              tanno_map Anno.S.get_solution tanno
            in
            let fun_types = List.map !recorded_fun_types
                (fun (fid,atanno,rtanno) -> (fid, update_tanno atanno, update_tanno rtanno))
            in
            Some (update_tanno atarg, update_tanno atres, fun_types)
          with LP_infeasable ->
            None

      end
    | Fclosed _, _ ->
      raise (Analysis_error "Expecting function type.")

end
