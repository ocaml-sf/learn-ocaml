(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   indices.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Indices for type annotations and operations on indices.
 *)

open Core

open Rtypes


(*The indices of multivariate resource polynomials.*)

type index =
  | Iunit
  | Inat of int
  | Ituple of index list
  | Iind of (index * constr_id) list

[@@deriving sexp, compare]


let rec degree  : index -> int =
fun ind ->
  match ind with
    | Iunit -> 0
    | Inat n -> n
    | Ituple is -> 
      List.fold is ~init:0 ~f:(fun acc i -> acc + (degree i))
    | Iind ics ->
      let d = 
	List.fold ics ~init:0 
	  ~f:(fun acc (i,c) -> acc + (degree i))
      in
      d+(List.length ics)


let rec zero_index : 'a rtype -> index = 
fun t ->
  match t with
    | Tbase _ 
    | Tvar _
    | Tarrow _
    | Tref _ -> Iunit
    | Tnat
    | Tarray _ -> Inat 0
    | Ttuple ts -> Ituple (List.map ts zero_index)
    | Tind cts -> Iind []


let rec is_zero_index : index -> bool = 
fun ind ->
  match ind with
    | Iunit 
    | Inat 0
    | Iind [] -> true
    | Ituple is -> 
      List.for_all is is_zero_index
    | _ -> false


let map_all_zero : ('a, index, 'b) Map.t -> bool = 
fun ci ->
  Map.for_all ci is_zero_index


let list_all_zero : index list -> bool =
fun is ->
  List.for_all is is_zero_index

(********************)

(* A cindex is an index for a context, extended with a list of types that
   are used to collect function arguments.  *)

type cindex = 
    index list * index String.Map.t 

[@@deriving sexp, compare]


(* Converting a cindex into an index. *)
let index_of_cindex : cindex -> index = 
fun (is,cind) ->
  Ituple [(Ituple is); Ituple (Map.data cind)]


let cdegree : cindex -> int =  
fun cind ->
  degree (index_of_cindex cind)


let zero_cindex : raml_type list * type_context -> cindex =
fun (ts, context) ->
  (List.map ts zero_index, Map.map context zero_index)


let is_zero_cindex : cindex -> bool = 
fun (is, ci) ->
  (List.for_all is is_zero_index) && (Map.for_all ci is_zero_index)

(********************)


(*Containers and maps for indices and cindices.*)

module T = struct
  type t = index [@@deriving sexp, compare]
  let hash = Hashtbl.hash
end

module Ind_set = Set.Make(T)
(*module Ind_hash = Hashable.Make (T)*)
module Ind_map = Map.Make(T)

module C = struct
  type t = cindex [@@deriving sexp, compare]
  let hash = Hashtbl.hash
end

(*module Cind_set = Set.Make(C)*)
(*module Cind_hash = Hashable.Make (C)*)
module Cind_map = Map.Make(C)

(********************)


let check_index : ('a rtype -> (index * Rtypes.constr_id) list -> bool) -> 'a rtype -> index -> bool
  = fun check_ind_index t ind ->
  let rec check_ci cstr_list (ind,cid) =
    match List.find cstr_list ~f:(fun c -> c.cstr_id = cid) with
      | Some constr -> valid (constr.cstr_type) ind
      | None -> false

  and valid_list ts is =
    match List.zip ts is with
      | None -> false
      | Some tis ->
	List.for_all tis (fun (t,i) -> valid t i)

  and valid t ind =
    match ind with
      | Iunit -> (
	match t with
	  | Tbase _
	  | Tvar _
	  | Tref _
	  | Tarrow _ -> true
	  | _ -> false
      )
      | Inat n -> (
	match t with
	  | Tnat
	  | Tarray _ -> true
	  | _ -> false
      )
      | Ituple is -> (
	match t with 
	  | Ttuple ts -> valid_list ts is
	  | _ -> false
      )
      | Iind ics -> (
	match t with 
	  | Tind cstr_list ->
	    let b1 = List.for_all ics (check_ci cstr_list) in
	    let b2 = check_ind_index t ics in
	    b1 && b2
	  | _ -> false
      )
  in

  valid t ind


let valid_index : 'a rtype -> index -> bool =
  fun t i -> check_index (fun t ics -> true) t i


let tracked_inductive_index =
  let multi_leaves t = 
    match t with
      | Tind clist ->
	let leafs = List.filter clist (fun c -> c.cstr_deg = 0) in
	List.length leafs > 1
      | _ -> raise (Invalid_argument "Expecting inductive type.")
  in
  let valid_leave t (i,cid) =
    (degree i > 0) || (multi_leaves t) || (constr_degree cid t > 0)
  in
  let valid_inner t (i,cid) = 
    (constr_degree cid t > 0)
  in
  fun t ics ->
    match ics with
      | [] -> true
      | [ic] ->
        valid_leave t ic
      | _  ->
        let (ics',ic) = Toolbox.snoc ics in
        (valid_leave t ic) && (List.for_all ics' (valid_inner t))

    
let tracked_index : ?deg:int -> 'a rtype -> index -> bool =

  fun ?deg t i ->
    assert (valid_index t i);
    let b1 = check_index tracked_inductive_index t i in
    let b2 =
      match deg with 
	| None -> true
	| Some n -> n >= degree i
    in
    b1 && b2


let valid_cindex :
    'b rtype list ->
  'b rtype String.Map.t ->
  cindex ->
  bool =

  fun ts tcont (is,cind) ->
    let f ~key:var ~data:ind acc =
      acc && (
	match Map.find tcont var with
	  | None -> false
	  | Some t -> valid_index t ind
      )
    in
    (Map.fold cind ~init:true ~f) 
    && (valid_index (Ttuple ts) (Ituple is) )
      

let tracked_cindex :
    ?deg:int ->
  'b rtype list ->
  'b rtype String.Map.t ->
  cindex ->
  bool =

  fun ?deg ts tcont (is,cind) ->

    assert (valid_cindex ts tcont (is,cind));

    let f ~key:var ~data:ind acc =
      acc && (
	match Map.find tcont var with
	  | None -> false
	  | Some t -> tracked_index t ind
      )
    in
    (Map.fold cind ~init:true ~f) 
    && (tracked_index (Ttuple ts) (Ituple is) )
    && ( match deg with 
      | None -> true
      | Some n -> n >= cdegree (is,cind)
    )

(*We frequently have to construct data structures that are indexed by
indices of a given degree.  In the following we define functions that
help us to efficiently construct such data structures.*)

(* indices of a given degree deg for a type t*)

let indices_deg : 
     init:'a 
  -> add_ind:(index -> 'a -> 'a) 
  -> 'b rtype 
  -> int 
  -> 'a =

  (*distr_weigth w k computes all the posibilities to distribute w pebbels
    over k slots.*)
  let distr_weight w k =

    let rec dweight prefix w k acc =
      match w,k with
	| 0, 0 -> prefix :: acc
	| _, 0 -> acc
	| w, k ->
	  process w prefix w k acc

    and process n prefix w k acc =
      if n = 0 then
	dweight (0::prefix) w (k-1) acc
      else 
	let acc' = dweight (n::prefix) (w-n) (k-1) acc in
	process (n-1) prefix w k acc'
    in

    dweight [] w k []
  in

  (* All tuples [a;b;c] that you can form with input lists [A;B;C] *)
  (* Or somehting like that*)
  let tuples ~acc ~add_ind fun_list f = 

    let rec tup fun_list h =
      match fun_list with
	| [] -> h []
	| g::gs ->
	  g (fun i acc ->
	    tup gs (fun is acc -> h (i::is) acc) acc
	  )
    in
    let add_ind is acc =
      match f is with
	| Some i -> add_ind i acc
        | None -> acc
    in
    tup fun_list add_ind acc
  in

  let rec inds_tuple g ts deg acc = 
    let dists = distr_weight deg (List.length ts) in
    List.fold dists ~init:acc ~f:( fun acc dist ->
      let ind_funs = List.map (List.zip_exn dist ts) ~f:(fun (k,t) ->
	fun f acc -> inds ~f t k acc
      ) in 
      tuples ~acc ~add_ind:g ind_funs (fun is -> Some (Ituple is))
    )

  and inds_ind g cts deg_out deg_in acc = 
    let dists = distr_weight deg_in deg_out in
    let acc' =
      List.fold dists ~init:acc ~f:( fun acc dist ->
	let ind_funs = List.map dist ~f:(fun k ->
	  fun g acc -> 
	    List.fold cts ~init:acc ~f:(fun acc constr ->
	      let cid = constr.cstr_id in
	      let t = constr.cstr_type in
	      inds ~f:(fun i -> g (i,cid)) t k acc)
	) in
	let check_val ics =
	  if tracked_inductive_index (Tind cts) ics then
	    Some (Iind ics)
	  else
	    None
	in
	tuples ~acc ~add_ind:g ind_funs check_val
      )
    in
    if deg_out = 0 then
      acc'
    else
      inds_ind g cts (deg_out-1) (deg_in+1) acc'

  and inds ~f t deg acc =
    match t with
      | Tbase _ 
      | Tvar _
      | Tarrow _
      | Tref _ -> 
	if deg = 0 then 
	  f Iunit acc
	else
	  acc
      | Tnat
      | Tarray _ -> f (Inat deg) acc
      | Ttuple ts -> inds_tuple f ts deg acc
      | Tind cts -> inds_ind f cts deg 0 acc
  in

  fun ~init ~add_ind t deg ->
    inds ~f:add_ind t deg init


(* indices of a degree smaller than or equal to a given deg *)
let indices_max_deg :
     empty:'a 
  -> add_ind:(index -> 'a -> 'a) 
  -> 'b rtype 
  -> int 
  -> 'a =

fun ~empty ~add_ind t deg ->

  let indices_deg = indices_deg ~add_ind in
  
  let rec inds_max t deg acc =
    if deg = 0 then
       indices_deg ~init:acc t 0
    else
      let acc' = indices_deg ~init:acc t deg in
      inds_max t (deg-1) acc'
  in
  inds_max t deg empty


(* cindices of a given degree for a context and rtype lsit *)
let cindices_deg :
  init:'a
  -> add_ind:(cindex -> 'a -> 'a)
  -> 'b rtype list
  -> 'b rtype String.Map.t
  -> int
  -> 'a =

fun ~init ~add_ind ts tcont deg ->
  let vars = Map.keys tcont in
  let add_ind2 ind acc = 
    let cind = 
      match ind with
	| Ituple [Ituple is1; Ituple is2] ->
	  (is1, String.Map.of_alist_exn (List.zip_exn vars is2))
	| _ -> failwith "Dead code"
    in
    add_ind cind acc
  in
  let rtype = Ttuple [Ttuple ts; Ttuple (Map.data tcont)] in
  indices_deg ~init ~add_ind:add_ind2 rtype deg


(* cindices less then or equal to a given degree for a context and rtype lsit *)
let cindices_max_deg :
  empty:'a
  -> add_ind:(cindex -> 'a -> 'a)
  -> 'b rtype list
  -> 'b rtype String.Map.t
  -> int
  -> 'a =
fun ~empty ~add_ind ts tcont deg ->
  let vars = Map.keys tcont in
  let add_ind2 ind acc = 
    let cind = 
      match ind with
	| Ituple [Ituple is1; Ituple is2] ->
	  (is1, String.Map.of_alist_exn (List.zip_exn vars is2))
	| _ -> failwith "Dead code"
    in
    add_ind cind acc
  in
  let rtype = Ttuple [Ttuple ts; Ttuple (Map.data tcont)] in
  indices_max_deg ~empty ~add_ind:add_ind2 rtype deg


let rec pseudo_lin_num ind t =
  assert( valid_index t ind);
  match ind,t with
    | Iunit,_ -> 0
    | Inat _,_ -> 0
    | Ituple is, Ttuple ts -> 
      let f n i t = n + (pseudo_lin_num i t) in
      List.fold2_exn is ts ~init:0 ~f
    | Iind [(i,_)] , Tind cts when degree i = 0 ->
      if ind_type_max_deg (Tind cts) = 0 then 1 else 0
    | Iind ics, Tind cts ->
      let f n (i,cid) =
        let ct = List.find_exn cts ~f:(fun c -> c.cstr_id = cid) in
	n + (pseudo_lin_num i ct.cstr_type)
      in
      List.fold ics ~init:0 ~f
    | _,_ -> 
      raise (Invalid_argument "Index and type do not match.")


(* prioritized indices for objective function*)

let prioritized_indices t deg =
  let add_ind i acc =
    (i,degree i, pseudo_lin_num i t)::acc
  in
  let inds = indices_max_deg ~empty:[] ~add_ind t deg in
  let cmp (i1,d1,l1) (i2,d2,l2) =
    assert (d1>=l1);
    assert (d2>=l2);
    let diff = (d2-l2)-(d1-l1) in
    if diff = 0 then
      l1-l2
    else
      diff
  in
  let inds = List.sort ~compare:cmp inds in
  let break (i1,d1,l1) (i2,d2,l2) = 
    not ((d1,l1) = (d2,l2))
  in
  let iss = List.group ~break inds in
  let f is =
    List.map ~f:(fun (i,_,_) -> i) is
  in  
  List.map ~f iss
