(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   simplify.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   LP solver interfaces.
 *
 *   Currently supported:
 *     - CLP via C bindings
 *   
 *)

open Core


type result =
  | Feasible
  | Infeasible




module type SOLVER =
sig
  module VMap : Map.S
  exception E of string
  type var = VMap.Key.t
  val fresh_var : unit -> var
  val add_constr_list : ?lower:float -> ?upper:float -> (var*float) list -> unit
  val add_constr_array : ?lower:float -> ?upper:float -> (var*float) array -> unit
  val add_objective : var -> float -> unit
  val set_objective : float VMap.t  -> unit
  val reset_objective : unit -> unit
  val first_solve : unit -> result
  val resolve : unit -> result
  val get_objective_val : unit -> float
  val get_solution : var -> float
  val get_num_constraints : unit -> int
  val get_num_vars : unit -> int
end


(*The following is for debugging and performance tests*)
module Dummy_solver : SOLVER =
struct
  exception E of string
  module VMap = Unit.Map
  type var = unit

  let fresh_var () = ()
  let add_constr_list ?lower ?upper _  = ()
  let add_constr_array ?lower ?upper _  = ()
  let add_objective () _ = ()
  let set_objective _ = ()
  let reset_objective () = ()
  let first_solve () = Infeasible
  let resolve () = Infeasible
  let get_objective_val () = 0.0
  let get_solution () = 0.0
  let get_num_constraints () = 0
  let get_num_vars () = 0

end


module type CLP_OPTIONS = 
sig
  val row_buffer_size : int
  val col_buffer_size : int
  val log_level : int
  val direction : Clp.direction
end


module Clp_std_options : CLP_OPTIONS = 
struct
  let row_buffer_size = 8000
  let col_buffer_size = 5000
  let log_level = 0
  let direction = Clp.Minimize
end

module Clp_std_maximize : CLP_OPTIONS = 
struct
  let row_buffer_size = 8000
  let col_buffer_size = 5000
  let log_level = 0
  let direction = Clp.Maximize
end

module Clp = 
  functor (Options: CLP_OPTIONS) ->
struct
  type var = int
  
  let col_buffer_size = Options.col_buffer_size
  let row_buffer_size = Options.row_buffer_size

  let () = assert (col_buffer_size > 0)
  let () = assert (row_buffer_size > 0)

  exception E of string

  module VMap = Int.Map

  let clp_state = ref (Clp.create ())
  let objective = ref (Int.Map.empty : float Int.Map.t)
  let solution = ref [| |]
  let () = Clp.set_log_level !clp_state Options.log_level (* use n>0 for debugging *)
  let () = Clp.set_direction !clp_state Options.direction

  let var_count = ref 0
  let row_count = ref 0

  let num_rows = ref 0

  let reset () = 
    clp_state := Clp.create ()

  let get_num_vars () = Clp.number_columns !clp_state

  let get_num_constraints () = 
    !num_rows + !row_count

  let many_cols =
      let new_col = 
	{ Clp.column_obj = 0.
	; Clp.column_lower = 0.
	; Clp.column_upper = Float.max_value
	; Clp.column_elements = [| |]
	}
      in
      Array.create ~len:col_buffer_size new_col

  let fresh_var () = 
    let count = !var_count in
    let () = 
      if count % col_buffer_size = 0 then
	Clp.add_columns !clp_state many_cols
      else
	()
    in
    let () = var_count := count + 1 in
    count

  let row_buffer = 
    let init_row =
      { Clp.row_lower = 0.
      ; Clp.row_upper = 0.
      ; Clp.row_elements = [| |]
      }
    in
    Array.create ~len:row_buffer_size init_row


  let flush_row_buffer () =
    let () = Clp.add_rows !clp_state row_buffer in
    num_rows := !num_rows + !row_count;
    row_count := 0
    

  let flush_buffers () =
    flush_row_buffer ()


  let add_constr_array ?(lower=(-.Float.max_value)) ?(upper=Float.max_value) row_array =
    let () =
      if !row_count = row_buffer_size then
	flush_row_buffer ()
      else
	()
    in
    let () = row_count := !row_count+1 in
    let row = 	
      { Clp.row_lower = lower
      ; Clp.row_upper = upper
      ; Clp.row_elements = row_array
      }
    in
    Array.set row_buffer (!row_count-1) row
    

  let add_constr_list ?(lower=(-.Float.max_value)) ?(upper=Float.max_value) row_list =
    let row_array = Array.of_list row_list in
    add_constr_array  row_array ~lower ~upper

  let add_objective v q =
    objective := Map.set !objective v q

  let set_objective obj =
    objective := obj

  let reset_objective () =
    objective := Int.Map.empty

  let get_solution () =
    solution := Clp.primal_column_solution !clp_state

  let copy_objective () =
    let arr = Array.create (get_num_vars ()) 0.0 in
    let () = Int.Map.iteri !objective (fun ~key ~data -> Array.set arr key data) in
    Clp.change_objective_coefficients !clp_state arr

  let first_solve () =
    copy_objective ()
    ; flush_buffers ()
    ; Clp.initial_solve !clp_state
    ; get_solution ()
    ; match Clp.status !clp_state with
      | 0 -> Feasible
      | _ -> Infeasible
	
  let resolve () =
    copy_objective () 
    ; flush_buffers ()
    ; Clp.dual !clp_state
    ; get_solution ()
    ; match Clp.status !clp_state with
      | 0 -> Feasible
      | _ -> Infeasible
	
  let get_objective_val () =
    Clp.objective_value !clp_state

  let get_solution v =
    if -1 < v && v < (Array.length !solution) then
      !solution.(v)
    else
      raise (E ("Variable " ^ (string_of_int v) ^ " is not in the solution."))

end

