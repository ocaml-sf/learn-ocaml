(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   rtypes.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Types for RAML expressions.
 *)


open Core

(*Type variables*)
type type_var = 
    { var_num  : int
    ; var_name : string
    }
[@@deriving sexp, compare]

module TVMap = Map.Make(struct type t = type_var [@@deriving sexp, compare] end)

(*Constructor names*)
type constr_id = string [@@deriving sexp, compare]

(* Supported base types.
 * Nat is a special case that is treated differently.
 *)
type base_type =
  | Tint
  | Tfloat
  | Tbool
  | Tunit
  | Tghost_unit
[@@deriving sexp, compare]

let string_of_base_type t =
  match t with
    | Tint -> "int"
    | Tfloat -> "float"
    | Tbool -> "bool"
    | Tunit -> "unit"
    | Tghost_unit -> "ghost_unit"



(* For [rty = Tind cs], a [c] in [cs] means a data constructor [c.cstr_id] with
   type [Ttuple (c.cstr_type :: Toolbox.repeat c.cstr_deg rty) -> rty] or
   [c.cstr_type -> rty] when [c.cstr_deg] is zero (see [unfold] below). *)
type 'b constructor = 
    { cstr_id : constr_id
    ; cstr_type : 'b
    ; cstr_deg : int
    }
[@@deriving sexp, compare]

(* Types as described in the RAML TR. *)
type 'a rtype =
  | Tbase of base_type
  | Tnat
  | Tvar of type_var    
  | Tarray of 'a rtype
  | Tref of 'a rtype
  | Ttuple of ('a rtype) list   
  | Tarrow of ('a rtype) list * ('a rtype) * 'a
  | Tind of (('a rtype) constructor) list
[@@deriving sexp, compare]
  

let rec is_const_type t =
  match t with
    | Tbase _
    | Tvar _
    | Tref _
    | Tarrow _ -> true
    | Tarray _
    | Tnat
    | Tind _ -> false
    | Ttuple ts ->
      List.for_all ts is_const_type 




let rec t_map
    : ('a -> 'b) ->  'a rtype -> ('b rtype) =
  fun f rtype ->
    match rtype with
      | Tbase bt -> Tbase bt
      | Tnat -> Tnat
      | Tvar vid -> Tvar vid
      | Tarray t -> Tarray (t_map f t)
      | Tref t -> Tref (t_map f t)
      | Ttuple ts -> Ttuple (List.map ts (t_map f))
      | Tarrow (targs,tres,a) -> Tarrow (List.map targs (t_map f), t_map f tres, f a)
      | Tind clist ->  Tind (
	List.map clist 
	  (fun c -> {c with cstr_type = t_map f c.cstr_type}) 
      )

type raml_type = unit rtype [@@deriving sexp, compare]

type type_context = raml_type String.Map.t


(* Smart constructor for Tarrow *)
(* For use in simplify.ml *)
let tarrow t1 t2 = match t2 with
  | Tarrow (ls, ret, anno) -> Tarrow (t1 :: ls, ret, ())
  | _                      -> Tarrow ([t1], t2, ())

let rec tapp tarr t = match tarr with
  | Tarrow ([], ret, ()) -> tapp ret t
  | Tarrow (t1 :: ls, ret, ()) ->
    if t1 = t then
      if ls = [] then ret else Tarrow (ls, ret, ())
    else
      raise (Invalid_argument "Rtypes.tapp : mismatch")
  | _ -> raise (Invalid_argument "Rtypes.tapp : not an arrow")

let rec tapp_list tarr = function
  | [] -> tarr
  | t :: ls -> tapp_list (tapp tarr t) ls


let constr_degree cid t_ind =
  match t_ind with
    | Tind clist ->
      begin
	match List.find clist (fun d -> d.cstr_id = cid) with 
	  | Some c -> c.cstr_deg
	  | _ -> raise (Invalid_argument "Rtypes.constr_degree : cid not in inductive type")
      end
    | _ -> raise (Invalid_argument "Rtypes.constr_degree : not in inductive type")

let ind_type_max_deg t_ind =
  match t_ind with
    | Tind clist ->
	List.fold clist ~init:0 ~f:(fun acc c -> Int.max acc c.cstr_deg) 
    | _ -> raise (Invalid_argument "Rtypes.constr_degree : not in inductive type")



exception Rtype_fold_error of string


let unfold
    : 'a rtype -> constr_id -> 'a rtype =
  fun t cid ->
    match t with
      | Tind clist -> (
	match List.find clist (fun d -> d.cstr_id = cid) with 
      	  | None -> 
	    raise (Rtype_fold_error "Constructor not part of type declaration.")
	  | Some d -> 
	    let deg = d.cstr_deg in
	    if deg > 0 then
	      Ttuple (d.cstr_type::(Toolbox.repeat t deg))
	    else if deg = 0 then
	      d.cstr_type
	    else
	      raise (Rtype_fold_error "Negative degree.")
      )
      | _ -> raise (Rtype_fold_error "Unfolding a non-inductive type.")

let type_substitute : ?join:('a -> 'a -> 'a) -> 'a rtype TVMap.t -> 'a rtype -> 'a rtype =
  fun ?(join=fun x y -> x) m orig -> let rec subst_rec =
    function Tbase _ as ty   -> ty
           | Tnat    as ty   -> ty
           | Tvar v  as ty   -> begin match Map.find m v with
                                | Some ty' -> ty'
                                | None     -> ty
                                end
           | Tarray ty       -> Tarray (subst_rec ty)
           | Tref ty         -> Tref (subst_rec ty)
           | Ttuple ls       -> Ttuple (List.map ls subst_rec)
           | Tarrow (ls, ty, a) -> begin
             match subst_rec ty with
             | Tarrow (ls', ty', a') -> Tarrow (List.map ls subst_rec @ ls'
                                               , ty', join a a')
             | ty' -> Tarrow (List.map ls subst_rec, subst_rec ty, a)
             end
           | Tind cs         ->
             Tind (List.map cs (fun c ->
                    { c with cstr_type = subst_rec c.cstr_type }))
    in subst_rec orig

let rtype_list_to_string rtys =
  let buf = Bigbuffer.create 64 in
  let rec add_to_buf = function
  | Tbase Tint   -> Bigbuffer.add_char buf 'I'
  | Tbase Tfloat -> Bigbuffer.add_char buf 'F'
  | Tbase Tbool  -> Bigbuffer.add_char buf 'B'
  | Tbase Tunit  -> Bigbuffer.add_char buf 'U'
  | Tbase Tghost_unit -> Bigbuffer.add_char buf 'G'
  | Tnat         -> Bigbuffer.add_char buf 'N'
  | Tvar v       -> Bigbuffer.add_char buf 'V'; Bigbuffer.add_string buf v.var_name
                  ; Bigbuffer.add_char buf '$'; Bigbuffer.add_string buf (Int.to_string v.var_num)
  | Tarray rty   -> Bigbuffer.add_char buf 'A'; add_to_buf rty
  | Tref rty     -> Bigbuffer.add_char buf 'R'; add_to_buf rty
  | Ttuple ls    -> Bigbuffer.add_char buf '('
                  ; List.iter ls add_to_buf ; Bigbuffer.add_char buf ')'
  | Tarrow (ls, rty, ()) ->
                    Bigbuffer.add_char buf '['; List.iter ls add_to_buf
                  ; Bigbuffer.add_char buf ']'; add_to_buf rty
  | Tind ls      -> Bigbuffer.add_char buf '{'
                  ; List.iter ls cstr_to_buf; Bigbuffer.add_char buf '}'
  and cstr_to_buf { cstr_id = id; cstr_deg = d; cstr_type = rty } =
      Bigbuffer.add_string buf (String.substr_replace_all id "`" "``")
    ; Bigbuffer.add_char buf '`'
    ; Bigbuffer.add_string buf (Int.to_string d); add_to_buf rty
  in List.iter rtys add_to_buf; Bigbuffer.contents buf

let rtype_to_string rty = rtype_list_to_string [rty]

let rtype_list_of_string str =
  let len = String.length str in
  let lfindi start f = String.lfindi str ~pos:start ~f in
  let slice start stop = String.slice str start stop in
  let index_from start c = String.index_from_exn str start c in

  let parse_list start par c =
      let rec parse_list_acc start acc =
          if start >= len || str.[start] = c
            then acc, start + 1
            else let v, res = par start in parse_list_acc res (v :: acc) in
      let ls, res = parse_list_acc start []
      in List.rev ls, res in
  let parse_int start =
      let idx = Option.value ~default:len @@
                lfindi start (fun _ -> Fn.non Char.is_digit)
      in Int.of_string (slice start idx), idx in
  let rec parse start = match str.[start] with
  | 'I' -> Tbase Tint, start + 1
  | 'F' -> Tbase Tfloat, start + 1
  | 'B' -> Tbase Tbool, start + 1
  | 'U' -> Tbase Tunit, start + 1
  | 'G' -> Tbase Tghost_unit, start + 1
  | 'N' -> Tnat, start + 1
  | 'V' -> let idx = index_from (start + 1) '$' in
           let num, res = parse_int (idx + 1)
           in Tvar { var_num = num; var_name = slice (start + 1) idx }
            , res
  | 'A' -> let rty, res = parse (start + 1) in Tarray rty, res
  | 'R' -> let rty, res = parse (start + 1) in Tref rty, res
  | '(' -> let ls, res = parse_list (start + 1) parse ')' in Ttuple ls, res
  | '[' -> let ls, res = parse_list (start + 1) parse ']' in
           let rty, res' = parse res in Tarrow (ls, rty, ()), res'
  | '{' -> let ls, res = parse_list (start + 1) parse_cstr '}' in Tind ls, res
  | _   -> raise (Invalid_argument ("rtype_list_of_string: " ^ str))
  and parse_cstr start =
      let cstr_id_buf = Bigbuffer.create 64 in
      let rec parse_cstr_id start = match str.[start] with
        | '`' -> if str.[start + 1] <> '`' then
                   Bigbuffer.contents cstr_id_buf, start + 1
                 else
                   let _ = Bigbuffer.add_char cstr_id_buf '`'
                   in parse_cstr_id (start + 2)
        | c -> Bigbuffer.add_char cstr_id_buf c; parse_cstr_id (start + 1) in
      let cstr_id, idx = parse_cstr_id start in
      let d, res = parse_int idx in
      let rty, res' = parse res
      in { cstr_id = cstr_id; cstr_deg = d; cstr_type = rty }
       , res'
  in fst (parse_list 0 parse '\x00')

let rtype_of_string str = List.hd_exn (rtype_list_of_string str)
