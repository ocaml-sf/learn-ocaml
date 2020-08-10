(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   pprint.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Pretty(?) printing.
 *)


open Core

open Expressions
open Eval
open Format
open Rtypes
open Indices
open Annotations
open Polynomials


let constr_map : string String.Map.t ref = (* cstr_id -> tycon *)
  ref String.Map.empty

let set_constr_map new_map = constr_map := new_map

let out_fixed (*form f*) = Fn.flip fprintf (*f form*)

let rec fprint_list_sep f (xs, fprintx, sep) =
  match xs with
    | [] -> ()
    | [x] -> fprintx f x
    | x::xs ->
      fprintf f "%a%t%a" fprintx x sep fprint_list_sep (xs, fprintx, sep)

let print_list_sep ?output:(formatter=std_formatter) xs printx sep =
  fprint_list_sep formatter (xs, Fn.const printx, Fn.flip pp_print_string sep)


let fprint_raml_type ?(indent=2) ?(expand_ind=false) ?(nice_type_vars=true)
      f typ =

  (* nice names for type variables *)

  let fresh_var =
    let vars = ref ["'a";"'b";"'c";"'d";"'e";"'s";"'t";"'u";"'v";"'w"] in
    let count = ref 0 in
    fun () ->
      match !vars with
	| [] -> count := !count+1; "'a" ^ (string_of_int !count)
	| x::xs -> vars := xs; x
  in

  let mem_map =
    let the_map = ref String.Map.empty in
    fun key ->
      match Map.find !the_map key with
	| Some value -> value
	| None ->
	  let fresh_val = fresh_var () in
	  let () = the_map := Map.set !the_map key fresh_val in
	  fresh_val
  in

  (* These three functions are polymorphic recursive: [fprint_type] may call
     [rtype_of_string] to recover types from data constructor suffixes, which
     does not reconstruct annotations and only returns [unit rtype].  Since
     we do not touch the annotation in the input [typ] anyway, we use
     polymorphic recursion so that [fprint_type] accepts [unit rtype] no matter
     what annotation type [typ] has. *)
  let rec fprint_constr : 'a. _ -> ('a rtype) constructor -> _ = fun f c ->
    fprintf f "%s:(%a, %i)" c.cstr_id fprint_type (0, c.cstr_type) c.cstr_deg

  and fprint_types_sep : 'a. _ -> _ * 'a rtype list * _ -> _ =
    fun f (d, ts, sep) ->
    fprint_list_sep f (ts, (fun f t -> fprint_type f (d, t)), sep)

  (* d: precedence
     single identifier = above all (never parenthesized)
     _ array & _ ref   = 10
     { ... | ... }     = 10
     _ * _             = 5
     _ -> _            = 0
  *)
  and fprint_type : 'a. _ -> _ * 'a rtype -> _ = fun f (d, typ) ->

    let open_paren f printing_prec =
      pp_open_box f indent;
      if d > printing_prec then
        pp_print_string f "("
    in

    let close_paren f printing_prec =
      if d > printing_prec then
        pp_print_string f ")";
      pp_close_box f ()
    in

    match typ with
    | Tbase base_t -> pp_print_string f (string_of_base_type base_t)
    | Tnat ->         pp_print_string f (Rconfig.ocaml_nat_module ^ ".t")
    | Tvar vt ->      pp_print_string f (if nice_type_vars then mem_map vt.var_name else vt.var_name)
    | Tarray t ->
      fprintf f "%a%a@ array%a" open_paren 10
                                fprint_type (10, t)
                                close_paren 10
    | Tref t ->
      fprintf f "%a%a@ ref%a" open_paren 10
                              fprint_type (10, t)
                              close_paren 10
    | Ttuple ts ->
      fprintf f "%a%a%a"
        open_paren 5
        fprint_types_sep (6, ts, out_fixed " *@ ")
        close_paren 5
    | Tarrow ([t1],t2,_) ->
      fprintf f "%a%a ->@ %a%a"
        open_paren 0 fprint_type (1, t1) fprint_type (0, t2) close_paren 0
    | Tarrow (ts,t,_) ->
      fprintf f "%a[%a] ->@ %a%a"
        open_paren 0
        fprint_types_sep (0, ts, out_fixed ";@ ")
        fprint_type      (0, t)
        close_paren 0
    | Tind [] -> fprintf f "%a{ }%a" open_paren 10 close_paren 10
    | Tind (c :: _ as constr_list) ->
      let cstr_id, typs = match String.lsplit2 c.cstr_id '|' with
        | Some (cstr_id, typs) -> cstr_id, rtype_list_of_string typs
        | None                 -> c.cstr_id, [] in
      match expand_ind, Map.find !constr_map cstr_id with
      | false, Some tycon -> begin
        match typs with
        | [] -> pp_print_string f tycon
        | [typ] ->
          fprintf f "%a%a@ %s%a"
            open_paren 10
            fprint_type (10, typ)
            tycon
            close_paren 10
        | typlist ->
          fprintf f "%a%a@ %s%a"
            open_paren 10
            fprint_type (10, Ttuple typlist)
            tycon
            close_paren 10
        end
      | _, _ ->  (* expand_ind = true || Map.find ... = None *)
        fprintf f "%a{@ %a@ }%a"
          open_paren 10
          fprint_list_sep (constr_list, fprint_constr, out_fixed " |@ ")
          close_paren 10

  in
  fprint_type f (0, typ)

let print_raml_type ?output:(formatter=std_formatter)
    ?(indent=2) ?(expand_ind=false) typ =
  fprintf formatter "%a@?" (fprint_raml_type ~indent ~expand_ind ~nice_type_vars:true) typ

(* Precedence table from OCaml language definition with precedence number
   assigned ourselves (x means not supported).

  Construction or operator                      Associativity    Precedence
    prefix-symbol                                    -              85
    . .( .[ .{ (see section 7.20)                    -               x
    #                                                -               x
    function application, constructor application, \
    tag application, assert, lazy                  / left           70
    - -. (prefix)                                    -              65
    **... lsl lsr asr                                right           x
    *... /... %... mod land lor lxor                 left           55
    +... -...                                        left           50
    ::                                               right           x
    @... ^...                                        right           x
    =... <... >... |... &... $... !=                 left           35
    & &&                                             right          30
    or ||                                            right          25
    ,                                                -              20
    <- :=                                            right          15
    if                                               -              10
    ;                                                right           x
    let match fun function try                       -               0
*)

let prec_of_builtin_op = function
  | Un_not          -> 70  (* Pervasives.not is a function *)
  | Un_iminus       -> 65
  | Un_fminus       -> 65
  | Bin_iadd        -> 50
  | Bin_isub        -> 50
  | Bin_imult       -> 55
  | Bin_imod        -> 55
  | Bin_idiv        -> 55
  | Bin_fadd        -> 50
  | Bin_fsub        -> 50
  | Bin_fmult       -> 55
  | Bin_fdiv        -> 55
  | Bin_and         -> 30
  | Bin_or          -> 25
  | Bin_eq          -> 35
  | Bin_iless_eq    -> 35
  | Bin_igreater_eq -> 35
  | Bin_iless       -> 35
  | Bin_igreater    -> 35
  | Bin_fless_eq    -> 35
  | Bin_fgreater_eq -> 35
  | Bin_fless       -> 35
  | Bin_fgreater    -> 35

let builtin_op_is_left_assoc = function
  | Un_not          -> Some true  (* Pervasives.not is a function *)
  | Un_iminus       -> Some false  (* noassoc in spec, but totally right assoc *)
  | Un_fminus       -> Some false
  | Bin_iadd        -> Some true
  | Bin_isub        -> Some true
  | Bin_imult       -> Some true
  | Bin_imod        -> Some true
  | Bin_idiv        -> Some true
  | Bin_fadd        -> Some true
  | Bin_fsub        -> Some true
  | Bin_fmult       -> Some true
  | Bin_fdiv        -> Some true
  | Bin_and         -> Some false
  | Bin_or          -> Some false
  | Bin_eq          -> Some true
  | Bin_iless_eq    -> Some true
  | Bin_igreater_eq -> Some true
  | Bin_iless       -> Some true
  | Bin_igreater    -> Some true
  | Bin_fless_eq    -> Some true
  | Bin_fgreater_eq -> Some true
  | Bin_fless       -> Some true
  | Bin_fgreater    -> Some true

let fprint_expression ?(indent=2) ?(print_types=false) f exp =

  (* Use the default values of optional parameters for [fprint_raml_type]. *)
  let fprint_type f tp = fprint_raml_type f tp in

  let fprint_binding f (x, t) =
    fprintf f "%s" x;
    if print_types then
      fprintf f " :@ %a" fprint_type t
  in

  let fprint_bindings_sep f (xts, sep) =
    fprint_list_sep f (xts, fprint_binding, sep)
  in

  let fprint_binding_list f = function
    | []   -> ()
    | [xt] -> fprint_binding f xt
    | xts  -> pp_open_box f indent;
              fprintf f "(%a)@]" fprint_bindings_sep (xts, out_fixed ",@ ")

  in

  let fprint_binding_option f = function
    | None -> pp_print_string f "_"
    | Some xt -> fprint_binding f xt
  in

  let color = ref Enormal in
  let set_color f c =
    if !color <> c then
      begin
        color := c
      ; match c with
        | Efree   -> fprintf f "@<0>%s" Rconfig.ansi_esc_sequence_free
        | Enormal -> fprintf f "@<0>%s" Rconfig.ansi_esc_sequence_normal
      end in

  let rec fprint_exps_sep f (parents, exps, sep) =
    fprint_list_sep f (exps, (fun f e -> fprint_exp f (parents, e)), sep)

  and fprint_letrecs f xes =
    match xes with
      | [] -> ()
      | [(xt,e)] ->
        fprintf f "%a =@ %a" fprint_binding xt fprint_exp (0, e)
      | (xt,e)::xes ->
        fprintf f "%a =@ %a@]@ @[and@ %a"
          fprint_binding xt fprint_exp (0, e) fprint_letrecs xes

  and fprint_matches f matches =
    match matches with
      | [] -> ()
      | (constr, xts, e) :: matches ->
        printf "@]@ @[| %s %a@ ->@ %a%a"
          (if print_types then constr
                          else List.hd_exn (String.split constr '|'))
          fprint_binding_list xts
          fprint_exp (0, e)
          fprint_matches matches

  and fprint_exp f (d, exp) =
    let print_ann = print_types && match exp.exp_desc with
      | Elet (_, _, e) | Eletrec (_, _, e) | Etuple_match (_, _, e)
      | Eshare (_, _, _, e) ->
        (* Only print the result type of let again if it's marked different
           from the innor expression -- likely a faulty expression. *)
        e.exp_type <> exp.exp_type
      | _ -> true in
    let last_color = !color in
    begin
      pp_open_box f 0
    ; set_color f exp.exp_kind
    ; if print_ann then pp_print_string f "("
    ; fprint_exp_desc f ((if print_ann then 0 else d), exp.exp_desc)
    ; if print_ann then fprintf f " :@ %a)" fprint_type exp.exp_type
    ; set_color f last_color
    ; pp_close_box f ()
    end

  and fprint_exp_desc f (d, desc) =

    let open_paren f (indent, printing_prec) =
      pp_open_box f indent;
      if d > printing_prec then
        pp_print_string f "("
    in

    let close_paren f printing_prec =
      if d > printing_prec then
        pp_print_string f ")";
      pp_close_box f ()
    in
    match desc with
    | Ebase_const c ->   pp_print_string f (string_of_constant c)
    | Ebase_fun fn ->    pp_print_string f (string_of_builtin_fun fn)
    | Ebase_op Un_not -> pp_print_string f (string_of_builtin_op Un_not)
    | Ebase_op op ->     fprintf f "(%s)"  (string_of_builtin_op op)

    | Evar x -> pp_print_string f x

    | Eapp (name, e, es) -> begin
      match e.exp_desc, es with
      | Ebase_op op, [{ exp_desc = Etuple [e1; e2] }] when is_binop op ->
        let prec = prec_of_builtin_op op in
        let curr_color = !color in
        let (e1_prec, e2_prec) =
          match builtin_op_is_left_assoc op with
          | Some true  -> (prec, prec + 1)
          | Some false -> (prec + 1, prec)
          | None       -> (prec + 1, prec + 1) in
        fprintf f "%a%a@ %a%s%a@ %a%a"
          open_paren (0, prec)
          fprint_exp (e1_prec, e1)
          set_color e.exp_kind
          (string_of_builtin_op op)
          set_color curr_color
          fprint_exp (e2_prec, e2)
          close_paren prec
      | _, _ ->
        let app_prec = 85 in
        fprintf f "%a%a@ %a%a"
          open_paren (0, app_prec)
          fprint_exp (app_prec + 1, e)
          fprint_exps_sep (app_prec + 1, es, Fn.flip pp_print_space ())
          close_paren app_prec
      end

    | Elambda (xt, e) ->
      let lambda_prec = 0 in
      fprintf f "%afun %a ->@ %a%a"
        open_paren (0, lambda_prec)
        fprint_binding xt fprint_exp (lambda_prec, e)
        close_paren lambda_prec

    | Elet (x_opt, e1, e2) ->
      let let_prec = 0 in
      fprintf f "%alet@ %a =@ %a@ in@]@ @[%a%a"
        open_paren (0, let_prec)
        fprint_binding_option x_opt
        fprint_exp (0, e1)  (* inner exp has 0 precedence *)
        fprint_exp (let_prec, e2)
        close_paren let_prec

    | Eletrec (xs, es, e) ->
      let let_prec = 0 in
      fprintf f "%alet rec@ %a@ in@]@ @[%a%a"
        open_paren (0, let_prec)
        fprint_letrecs (List.zip_exn xs es) fprint_exp (let_prec, e)
        close_paren let_prec

    | Econd (e, e1, e2) ->
      let if_prec = 10 in
      fprintf f "%aif@ %a@ then@ %a@ @]%aelse@ %a%a"
        open_paren (indent, if_prec)
        fprint_exp (0, e)  fprint_exp (0, e1)
        pp_open_box indent fprint_exp (if_prec, e2)
        close_paren if_prec

    | Eshare (e1, x1, x2, e2) ->
      let share_prec = 0 in
      fprintf f "%ashare@ %a@ as@ (%a,@ %a)@ in@]@ @[%a%a"
        open_paren (0, share_prec)
        fprint_exp (0, e1) fprint_binding x1 fprint_binding x2
        fprint_exp (share_prec, e2)
        close_paren share_prec

    | Econst (c, e) ->
      let constr_prec = 70 in
      fprintf f "%a%s@ %a%a"
        open_paren (indent, constr_prec)
        (if print_types then c else List.hd_exn (String.split c '|'))
        fprint_exp (constr_prec + 1, e)
        close_paren constr_prec

    | Ematch (e, matches) ->
      let match_prec = 0 in
      fprintf f "@[<2>%amatch@ %a@ with%a%a@]"
        open_paren (indent, match_prec)
        fprint_exp (0, e) fprint_matches matches
        close_paren match_prec

    | Enat_match (e, e1, xt, e2) ->
      let match_prec = 0 in
      fprintf f "@[<2>%amatch@ %a@ with@]@ @[| %s.%s ->@ %a@]@ @[| %s.%s %a ->@ %a%a@]"
        open_paren (indent, match_prec)
        fprint_exp (0, e)
        Rconfig.ocaml_nat_module Rconfig.ocaml_nat_zero
        fprint_exp (0, e1)
        Rconfig.ocaml_nat_module Rconfig.ocaml_nat_succ
        fprint_binding xt fprint_exp (0, e2)
        close_paren match_prec

    | Eref e ->
      let ref_prec = 70 in
      fprintf f "%aref@ %a%a" open_paren (indent, ref_prec)
        fprint_exp (ref_prec + 1, e) close_paren ref_prec

    | Eref_deref e ->
      let deref_prec = 85 in
      fprintf f "%a!%a%a" open_paren (0, deref_prec)
        fprint_exp (deref_prec, e) close_paren deref_prec

    | Eref_assign (e1, e2) ->
      let assign_prec = 15 in
      fprintf f "%a%a@ :=@ %a%a" open_paren (indent, assign_prec)
        fprint_exp (assign_prec + 1, e1) fprint_exp (assign_prec, e2)
        close_paren assign_prec

    | Etuple es ->
      let always_paren = -1 in
      fprintf f "%a%a%a" open_paren (indent, always_paren)
        fprint_exps_sep (0, es, out_fixed ",@ ")
        close_paren always_paren

    | Etuple_match (e1, xs, e2) ->
      let let_prec = 0 in
      fprintf f "%alet (%a) =@ %a@ in@]@ @[%a%a"
        open_paren (0, let_prec)
        fprint_bindings_sep (xs, out_fixed ",@ ")
        fprint_exp (0, e1)  (* inner exp has 0 precedence *)
        fprint_exp (let_prec, e2)
        close_paren let_prec

    | Eundefined ->
      fprintf f "%s.%s" Rconfig.ocaml_raml_module Rconfig.ocaml_raml_undefined

    | Etick q ->
      fprintf f "%s.%s(%f)" Rconfig.ocaml_raml_module Rconfig.ocaml_raml_tick q

  in fprintf f "%a%a" fprint_exp (0, exp) set_color Enormal

let print_expression ?output:(formatter=std_formatter)
    ?(indent=2) ?(print_types=false) exp =
  let orig_max_indent = pp_get_max_indent formatter () in
  let orig_margin     = pp_get_margin     formatter () in
  let _ = pp_set_max_indent formatter 30 in
  let _ = pp_set_margin     formatter 120 in
    fprintf formatter "%a@." (fprint_expression ~indent ~print_types) exp
  ; pp_set_max_indent formatter orig_max_indent
  ; pp_set_margin     formatter orig_margin

let fprint_value ?(indent=2) f (loc, heap) =

  let rec list_locs l =
    match Map.find heap l with
    | Some (Vconst (c_id, _))      when String.is_prefix c_id (Rconfig.ocaml_list_nil^"|") -> []
    | Some (Vconst (c_id, l_cons)) when String.is_prefix c_id (Rconfig.ocaml_list_cons^"|") ->
      begin match Map.find heap l_cons with
      | Some (Vtuple [l1; l2]) ->
          l1 :: list_locs l2
      | _ -> failwith "Malformed list"
      end
    | _ -> failwith "Malformed list" in

  let rec fprint_vals_sep f (indirect, ls, sep) =
    match ls with
      | [] -> ()
      | [l] -> fprint_val f (false, indirect, l)
      | l::ls ->
        fprintf f "%a%t%a"
          fprint_val (false, indirect, l)
          sep
          fprint_vals_sep (indirect, ls, sep)

  and fprint_array f (size, arr) =
    fprint_val f (false, true, Map.find_exn arr 0)
  ; for i= 1 to size - 1 do
      fprintf f ";@ %a" fprint_val (false, true, Map.find_exn arr i)
    done

  and fprint_val f (parents, indirect, loc) =
    match Map.find heap loc with
    | None -> failwith "Mallformed heap."
    | Some value ->
      match value with
      | Vbc c  -> pp_print_string f (string_of_constant c)
      | Vbase_fun _ | Vbase_op _ | Vclosure _ -> pp_print_string f "<fun>"
      | Vnat n -> pp_print_string f (string_of_int n)
      (*should we distinguish nat and int? *)
      | Vloc l ->
        if indirect then
          fprint_val f (false, false, l)
        else
          fprintf f "ref->{@ %a@ }" fprint_val (false, false, l)

      | Vtuple ls ->
        fprintf f "(@ %a@ )" fprint_vals_sep (false, ls, out_fixed ",@ ")

      | Vconst (c_id, l) ->
        if String.is_prefix c_id (Rconfig.ocaml_list_cons^"|") 
	  || String.is_prefix c_id (Rconfig.ocaml_list_nil^"|") then
          fprintf f "[@ %a@ ]"
            fprint_vals_sep (true, list_locs loc, out_fixed ";@ ")
        else begin
          if parents then fprintf f "(@ "
        ; fprintf f "%s@ %a" (List.hd_exn (String.split c_id '|'))
	                     fprint_val (true, true, l)
        ; if parents then fprintf f "@ )"
        end

      | Varray (arr,size) ->
        fprintf f "[|@ %a@ |]" fprint_array (size, arr)

  in
  fprintf f "@[%a@]" fprint_val (false, false, loc)

let print_value ?output:(formatter=std_formatter) ?(indent=2) (loc, heap) =
  fprintf formatter "%a@." (fprint_value ~indent) (loc, heap)

let fprint_polynomial f p =
  match Map.min_elt p with
  | None -> pp_print_string f "0.0"
  | Some (min_key, _) ->
    pp_open_box f 5
    ; let first_sign = ref true in
      let print_sign f q = 
	if !first_sign then begin
	  first_sign := false;
	  fprintf f "%s" (if q < 0. then "-" else "")
	end
	else
          fprintf f "@ %s" (if q < 0. then "- " else "+ ")
      in
      Map.iteri p
      ~f:(fun ~key:basis ~data:q ->
        fprintf f "%a%.2f" print_sign q (Float.abs q)
        ; Map.iteri basis (fun ~key:var ~data:power ->
            fprintf f "*%s" var
          ; if power <> 1 then
              fprintf f "^%d" power
          )
      )
    ; pp_close_box f ()
  ; pp_print_flush f () (* closes all boxes *)

let print_polynomial ?output:(formatter=std_formatter) p =
  fprint_polynomial formatter p


let fprint_pol_desc f descs =
    pp_open_box f 0
  ; let g desc =
      fprintf f "\n     %s" desc
    in
    List.iter ~f:g descs
  ; pp_close_box f ()


let print_pol_desc ?output:(formatter=std_formatter) descs =
  fprint_pol_desc formatter descs


let rec fprint_index f ind =
  pp_open_box f 0
  ; begin
    match ind with
      | Iunit -> fprintf f "*"
      | Inat n -> fprintf f "%d" n
      | Ituple is -> begin
	match is with
	  | [i] -> 
	    fprint_index f i
	  | _ ->
	    fprintf f "("
	    ; print_list_sep ~output:f is (fprint_index f) ", "
	    ; fprintf f ")"
      end
      | Iind clist -> 
	let print_cind (ind,cid) =
	  let cid' = fst (String.lsplit2_exn cid ~on:'|') in
	  match ind with
	    | Ituple is when (List.length is > 1) ->
	      fprintf f "%s%a" cid' fprint_index ind
	    | _ ->
	      fprintf f "%s(%a)" cid' fprint_index ind
	in
	fprintf f "["
	; print_list_sep ~output:f clist print_cind "; "
	; fprintf f "]"
  end
  ; pp_close_box f ()
  ; pp_print_flush f ()

let print_index ?output:(formatter=std_formatter) ind =
  fprint_index formatter ind


let fprint_type_anno f tanno =
  pp_open_vbox f 2
  ; let print_annos f tanno =
      let add_ind ind () =
	let q = tanno.tan_map ind in
	if Float.abs(q -. 0.0) <. Rconfig.float_max_error then 
	  ()
	else
	  fprintf f "@[%10.2f  <--  %a@]@ " q fprint_index ind
      in
      indices_max_deg ~empty:() ~add_ind tanno.tan_type tanno.tan_deg
    in
    print_annos f tanno
    ; pp_close_box f ()
    ; pp_print_flush f ()


let print_type_anno ?output:(formatter=std_formatter) tanno =
  fprint_type_anno formatter tanno


let fprint_anno_funtype 
    ?(indent="")
    ?(simple_name=false) 
    f
    (fid, atanno, rtanno) 
    =
    let arrow_type =
      match atanno.tan_type with
	| Ttuple ts -> Tarrow (ts, rtanno.tan_type, ())
	| _ -> raise (Invalid_argument "Expecting tuple type.")
    in
    let fid = 
      if simple_name then
	match String.lsplit2 fid ~on:'#' with
	  | None -> fid
	  | Some (s1,s2) -> s1
      else
	fid
    in
    let fprint_raml_type f t = fprint_raml_type ~indent:2 f t in
    fprintf f "@.== %s :\n\n%s%a@."
      fid indent fprint_raml_type arrow_type
    ; fprintf f "\n%sNon-zero annotations of the argument:\n%a"
      indent fprint_type_anno atanno
    ; fprintf f "\n%sNon-zero annotations of result:\n%a"
      indent fprint_type_anno rtanno
    ; let (pol, descs) = describe_pol atanno in
      let _ = fprintf f "\n%sSimplified bound:\n   %s" indent indent in
      let _ = fprint_polynomial f pol in
      if List.length descs > 0 then
	let _ = fprintf f "\n %swhere" indent in
	fprint_pol_desc f descs;
	fprintf f "\n"
      else
	()
    ; fprintf f "@."


let print_anno_funtype ?(indent="") ?(simple_name=false) ?output:(formatter=std_formatter) atyp  =
  fprint_anno_funtype formatter ~indent ~simple_name atyp


