(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   Rconfig.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Global config file.
 *)

(*Options for recording and printing resource types of function applications.*)
type fun_types_mode =
  | Pall
  | Pnone
  | Pconsume
  | Plevel of int
  | Pregexp of string


type analysis_mode =
  | Mupper
  | Mlower
  | Mconstant

module type AMODE = 
sig
  val mode : analysis_mode
end

let ocaml_raml_module = "Raml"

let ocaml_raml_undefined = "undefined"

let ocaml_raml_tick = "tick"

let ocaml_raml_ref_swap = "ref_swap"  


let ocaml_nat_module = "Rnat"

(* print-only *)
let ocaml_nat_zero = "Zero"

let ocaml_nat_succ = "Succ"


let ocaml_array_module = "Rarray"


let persistent_modules = 
    [ ocaml_raml_module
    ; ocaml_nat_module
    ; ocaml_array_module
    ]

let ocaml_list_cons = "::"

let ocaml_list_nil = "[]"

let print_stack_on_exn = ref false

(* Show free expressions in ESC[2m (faint), not widely supperted by terminals *)
let ansi_esc_sequence_free   = "\x1b[2m"
let ansi_esc_sequence_normal = "\x1b[0m"

(* Show free expressions in ESC[1;30m (bright black / grey), assuming default
   foreground being white *)
(*
let ansi_esc_sequence_free   = "\x1b[1;30m"
let ansi_esc_sequence_normal = "\x1b[0m"
*)


(* Maximal error of floating point computations *)
let float_max_error = 0.0001
