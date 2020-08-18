(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   polynomials.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Multivariate polynomials
 *)

open Core

open Toolbox
open Rtypes
open Indices
open Annotations

module T = struct
  type t = int String.Map.t [@@deriving sexp, compare]
end

module Poly_map = Map.Make(T)

type polynomial = float Poly_map.t

let resolve_with f ~key:_ = 
  function 
    | `Both (q1,q2) -> f q1 q2
    | `Left q -> Some q
    | `Right q -> Some q 


let p_add p1 p2 =
  Map.merge p1 p2 (resolve_with
    ( fun q1 q2 ->
      let q = (q1+.q2) in
      if Float.abs(q -. 0.0) <. Rconfig.float_max_error then None else Some q
    )
  )

let p_sub p1 p2 =
  Map.merge p1 p2 ( fun ~key:_ ->
    function 
      | `Both (q1,q2) -> 
	let q = (q1-.q2) in
	if Float.abs(q -. 0.0) <. Rconfig.float_max_error then None else Some q
      | `Left q1 -> Some q1
      | `Right q2 -> Some (-.q2)
  )

let p_mult p1 p2 =
  Map.fold p1 ~init:Poly_map.empty 
    ~f:(fun ~key:k1 ~data:q1 acc ->
      Map.fold p2 ~init:acc 
	~f:(fun ~key:k2 ~data:q2 acc ->
	  let k = 
	    Map.merge k1 k2 
	      (resolve_with (fun n m -> Some (n + m)))
	  in
	  Map.set acc k (q1 *. q2)
	)
    )


let p_constant q =
  if Float.abs(q -. 0.0) <. Rconfig.float_max_error then
    Poly_map.empty
  else
    Poly_map.singleton (String.Map.empty) q

let p_uni vid coeffs =
  List.fold coeffs ~init:Poly_map.empty 
    ~f:( fun acc (n,q) -> 
      if (Float.abs(q -. 0.0) >=. Rconfig.float_max_error) then begin
	if n > 0 then
	  Map.set acc (String.Map.singleton vid n) q
	else if n = 0 then
	  Map.set acc (String.Map.empty) q	   
	else
	  failwith "Negative degree in polynomial"
      end else
	acc
    )

(* binom (vid,k) *)

let p_binom vid k = 

  let rec coeffs i = 
    if i < 0 then
      []
    else
      let s_ki = Float.of_int (stirling k i) in
      let fact_k = Float.of_int (fact k) in
      let q = s_ki /. fact_k in
      (i,q)::(coeffs (i-1))
  in

  p_uni vid (coeffs k)


let p_vars pol =
  let f ~key:coeffs ~data:_ acc =
    Map.merge acc coeffs (resolve_with (fun n m -> Some n))
  in
  Map.keys (Map.fold ~f ~init:String.Map.empty pol)


(* Naming scheme for types *)

type ('a,'b) name_scheme =
  | Nbase
  | Nnat of 'a
  | Nvar
  | Narray of 'a * (('a,'b) name_scheme)
  | Nref of ('a,'b) name_scheme
  | Ntuple of (('a,'b) name_scheme) list
  | Narrow
  | Nind of ('b rtype) * ((('a,'b) named_const) list)

and ('a,'b) named_const =
  { nconst_id : constr_id
  ; nconst_name : 'a
  ; nconst_name_sum : 'a
  ; nconst_type : ('a,'b) name_scheme
  }


let explain_names names pos used =

  let if_used x l =
    if List.mem ~equal:(=) used x then
      l
    else
      []
  in

  let rec explain names pos =
    match names with
      | Nvar
      | Narrow
      | Nbase -> []
      | Nnat x -> begin
	match String.substr_index pos "-nodes" with
	  | None -> if_used x [x ^ " is the value " ^ pos]
	  | Some _ -> if_used x [x ^ " is the maximal value " ^ pos]
      end
      | Narray (x,ns) -> 
	let pos' = "of the elemets " ^ pos in
	(if_used x [x ^ " is the length " ^ pos]) @ (explain ns pos')
      | Nref ns ->
	let pos' = "of the value stored in " ^ pos in
        explain ns pos'
      | Ntuple [ns] ->
	explain ns pos
      | Ntuple nss ->
	let f (i,acc) ns =
	  let expl_ns = 
	    let pos' =
	      "of the " ^ (Toolbox.enum i) ^ " component " ^ pos
	    in
	    explain ns pos'
	  in
	  (i+1,expl_ns @ acc)
	in
	snd (List.fold ~f ~init:(1,[]) nss)
      | Nind (Tind cs,ncs) ->
	let f acc nc =
	  let nc_x = nc.nconst_name in
	  let nc_x_sum = nc.nconst_name_sum in
	  let cid = 
	    List.hd_exn (String.split nc.nconst_id '|')
	  in
	  let expl_nc =
	    if_used 
	      nc_x
	      begin
                if ind_type_max_deg (Tind cs) = 0 then
		  match String.substr_index pos "-nodes" with
		    | Some _ ->
		      [nc_x ^ " is the fraction of " ^ cid ^ "-nodes " ^ pos]
		    | None ->
		      [nc_x ^ " is 1 iff the node " ^ pos ^ " is " ^ cid ^ " (and 0 otherwise)" ]
		else
		  match String.substr_index pos "-nodes" with
		    | Some _ ->
		      [nc_x ^ " is the maximal number of " ^ cid ^ "-nodes " ^ pos]
		    | None ->
		      [nc_x ^ " is the number of " ^ cid ^ "-nodes " ^ pos]
	      end
	  in
	  let expl_nc_sum =
	    if_used
	      nc_x_sum
	      [nc_x_sum ^ " is the sum of the " ^ cid ^ "-nodes " ^ pos]
	  in
	  let pos' = "in the " ^ cid ^ "-nodes " ^ pos in
	  expl_nc @ expl_nc_sum @ (explain nc.nconst_type pos') @ acc
	in
	List.fold ~f ~init:[] ncs
      | Nind _ -> raise (Invalid_argument "Mismatch of type and naming scheme.")
  in
  explain names pos


let make_names get_name typ =
  let rec mk_names typ =
    match typ with
      | Tbase _ -> Nbase
      | Tvar _ -> Nvar
      | Tarrow _ -> Narrow
      | Tref t -> Nref (mk_names t)
      | Tarray t -> Narray (get_name (), mk_names t)
      | Tnat -> Nnat (get_name ())
      | Ttuple ts ->
	Ntuple (List.map ts mk_names)
      | Tind cs ->
	let f c =
	  let name = get_name () in
	  { nconst_id = c.cstr_id
	  ; nconst_name = name
	  ; nconst_name_sum = name^"'"
	  ; nconst_type = mk_names c.cstr_type
	  }
	in
	Nind (Tind cs, List.map cs f)
  in
  mk_names typ

let get_std_name () =
  let next_name = ref 1 in
  let names_list = 
    ref ( ["N";"M";"K";"L";"X";"Y";"Z";"R";"S";"T"] 
	  @ ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J"]
	  @ ["P";"Q";"U";"V";"W"]
	  @ ["_N";"_M";"_K";"_L";"_X";"_Y";"_Z";"_R";"_S";"_T"] 
	  @ ["_A";"_B";"_C";"_D";"_E";"_F";"_G";"_H";"_I";"_J"]
	  @ ["_P";"_Q";"_U";"_V";"_W"]
    )
  in
  let f () = 
    match !names_list with
      | x::xs -> 
	names_list := xs;
	x
      | [] ->
	let n = !next_name in
	next_name := n+1;
	"N" ^ (Int.to_string n)
  in
  f


let rec pol_of_index names ind =
  match names, ind with
    | Nbase, Iunit
    | Narrow, Iunit
    | Nref _, Iunit
    | Nvar, Iunit -> p_constant 1.0
    | Nnat x, Inat n
    | Narray (x,_), Inat n -> p_binom x n
    | Ntuple names, Ituple is ->
      let f pol nam i =
	p_mult pol (pol_of_index nam i)
      in
      List.fold2_exn ~f ~init:(p_constant 1.0) names is
    | Nind (t,ncs), Iind ics -> begin
      let inner_pol = 
	let f pol ic  = 
	  let nc = List.find_exn ncs ~f:(fun nc -> nc.nconst_id = (snd ic)) in
	  p_mult pol (pol_of_index nc.nconst_type (fst ic))
	in
	List.fold ~f ~init:(p_constant 1.0) ics
      in
      let outer_pol =
	let f pol nc =
	  let nc_pol = 
	    let occ_nc =
	      List.length (List.filter ics (fun ic -> (snd ic) = nc.nconst_id))
	    in
	    p_binom nc.nconst_name occ_nc
	  in
	  p_mult pol nc_pol
	in
	List.fold ~f ~init:(p_constant 1.0) ncs
      in
      p_mult inner_pol outer_pol
    end
    | _ ,_ -> raise (Invalid_argument "Mismatch of index and naming scheme.")
	  

(* Optimization: if all pairs of constructors are in a polynomial,
then you can factor them out and talk about 'nodes' as before. *)

let pol_of_tanno names tanno =
  let t = tanno.tan_type in
  let deg = tanno.tan_deg in
  let sym_pol =
    let add_ind i sym_pol =
      let q = tan_find tanno i in
      if Float.abs(q -. 0.0) <. Rconfig.float_max_error then
	sym_pol
      else
	Map.set sym_pol i q
    in
    let empty = Ind_map.empty in
    indices_max_deg ~empty ~add_ind t deg
  in
  let f ~key:i ~data:q pol =
    let i_pol = pol_of_index names i in
    p_add (p_mult (p_constant q) i_pol) pol
  in
  let init = p_constant 0.0 in
  Map.fold ~init ~f sym_pol


let describe_pol ?(get_name=get_std_name ()) tanno =
  let t = tanno.tan_type in
  let names = make_names get_name t in
  let pol = pol_of_tanno names tanno in
  let used_names = p_vars pol in
  (pol, explain_names names "of the argument" used_names)

