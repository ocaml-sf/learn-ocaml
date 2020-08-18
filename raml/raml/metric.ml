(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   metric.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Resourec metrics.
 *)


open Core
open Expressions

(* Resource metrics *)

type res_const =
  | Mbase_const
  | Mbfun_load
  | Mbfun_eval of builtin_fun
  | Mbop_load
  | Mbop_eval of builtin_op
  | Mvar
  | Mapp of int
  | Mclosure of int
  | Mlet
  | Mletrec of int
  | Mcond
  | Mshare
  | Mconst
  | Mmatch of int
  | Mnat_match
  | Mref
  | Mref_deref
  | Mref_assign
  | Mtuple of int
  | Mtuple_match of int
  | Mtick of float
  | Marr_make_elem

type metric = res_const -> float


let string_of_res_const c =
  match c with
    | Mbase_const -> "Mbase_const"
    | Mbfun_load -> "Mbfun_load"
    | Mbfun_eval f -> "Mbfun_eval(" ^ (string_of_builtin_fun f) ^ ")"
    | Mbop_load -> "Mbop_load"
    | Mbop_eval op -> "Mbop_eval(" ^ (string_of_builtin_op op) ^ ")"
    | Mvar -> "Mvar"
    | Mapp n -> "Mapp(" ^ (string_of_int n) ^ ")"
    | Mclosure n -> "Mclosure("  ^ (string_of_int n) ^ ")"
    | Mlet -> "Mlet"
    | Mletrec n -> "Mletrec(" ^ (string_of_int n) ^ ")"
    | Mcond -> "Mcond"
    | Mshare -> "Mshare"
    | Mconst -> "Mconst"
    | Mmatch n -> "Mmatch of(" ^ (string_of_int n) ^ ")"
    | Mnat_match -> "Mnat_match"
    | Mref -> "Mref"
    | Mref_deref -> "Mref_deref"
    | Mref_assign -> "Mref_assign"
    | Mtuple n -> "Mtuple(" ^ (string_of_int n) ^ ")"
    | Mtuple_match n -> "Mtuple_match(" ^ (string_of_int n) ^ ")"
    | Mtick q  -> "Mtick(" ^ (Float.to_string q) ^ ")"
    | Marr_make_elem -> "Marr_make_elem"

let m_debug : res_const -> float
  = fun c ->
      print_string (string_of_res_const c)
    ; Out_channel.newline stdout
    ; 0.0

let m_costfree : res_const -> float
  = fun _ -> 0.0


let m_eval  : res_const -> float
  = fun c ->
    match c with
    | Mbfun_eval (Nat_of_intc _) -> 3.0
    | _ -> 1.0


let m_tick k =
  match k with
    | Mtick q -> q
    | _ -> 0.0

let m_heap k = 
  match k with
    | Mbase_const
    | Marr_make_elem -> 1.0
    | Mclosure n -> Float.of_int (n+1)
    | Mletrec n -> Float.of_int n
    | Mconst -> 2.0
    | Mref -> 1.0
    | Mtuple n -> Float.of_int n
    | Mbfun_eval (Nat_of_intc _) -> 1.0                    
    | _ -> 0.0

let (^^) (q,q') (p,p') = 
  if q' <= p then (q+.p-.q', p') 
  else (q, p'+.q'-.p)

let (-^) r (p,p') = 
  if r >= 0.0 then (r+.p, p') 
  else (0.0,-.r) ^^ (p,p')

let (^-) (q,q') s =
  if s >= 0.0 then (q,q') ^^ (s,0.0)
  else  (q, q'-.s)

let (--) r s =
  if s >= 0.0 then
    r -^ (s,0.0)
  else
    r -^ (0.0,-.s)
    

