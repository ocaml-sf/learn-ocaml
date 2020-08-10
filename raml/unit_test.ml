(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   unit_test.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Run a unit test on an example program.
 *
 *   Usage: unit_test eval FILE
 *
 *   This will do the following (in the given order):
 *      1) Parse the program with the ocaml parser
 *      2) Translate it to a typed RAML syntax tree
 *      3) Typecheck the RAML syntax tree
 *      4) Evaluate the main expression with the RAML interpreter
 *      5) Translate the RAML syntax tree to share-let-noraml form (SLNF)
 *      6) Typecheck the program in SLNF
 *      7) Evaluate the main expression of the program in SLNF
 *      8) Compare the results and the cost of the two evaluations
 *)



open Core.Std
open Format
open Rconfig


let metrics =      [ Metric.m_eval
		   ; Metric.m_costfree
		   ; Metric.m_tick
		   ; Metric.m_heap
		   ]

let metric_names = [ "eval-steps"
		   ; "cost-free "
		   ; "ticks     "
		   ; "heap-space"
		   ]

let _ = Config.load_path := [""; !Rpath.ocaml_raml_runtime]

let test_eval (result1, costs1) exp_sln _ =
  print_string "   evaluating SLN ...                        "
  ; flush stdout
  ; let (result2, costs2) = Eval.evaluate exp_sln metrics in
    print_string ("-> okay\n")
    ; print_string "   comparing costs ...                       "
    ; flush stdout
    ; if costs1 <> costs2 then
	failwith "Evaluation cost of the translated expression and the expression in SLN differ.\n"
      else 
	print_string ("-> okay\n")
    ; print_string "   comparing return values ...               "
    ; flush stdout
    ; if not (Eval.compare_values result1 result2) then
	failwith "Evaluation results of the translated expression and the expression in SLN differ.\n"
      else
	print_string ("-> okay\n")


let test_analysis degree (_, costs) _ exp_sln_stack =

  let eval_costs = List.map costs fst in

  let analyze amode acc (metric, m_name) eval_cost = 
    let module Clp = (
      val (
        match amode with
        | Mlower -> (
            module Solver.Clp( Solver.Clp_std_maximize )
          )
        | Mupper
        | Mconstant -> (
            module Solver.Clp( Solver.Clp_std_options )
          )
      ) : Solver.SOLVER
    )
    in
    let module Amode =
    struct
      let mode = amode
    end
    in
    let module Analysis = Analysis.Make( Clp )(Amode) in
    let mode_name =
      match amode with
      | Mupper -> "upper"
      | Mlower -> "lower"
      | Mconstant -> "constant"
    in
    fprintf std_formatter "   metric = %s; mode = %s\n" m_name mode_name
  ; print_string "      analyzing ...                          "
  ; flush stdout
  ; match Analysis.analyze_expression exp_sln_stack ~metric ~degree with
    | None -> 
      failwith "Linear program  is infeasable.\n"
    | Some (q, _) ->
      begin
	print_string ("-> okay\n")
      ; print_string "      checking bound ...                     "
      ; flush stdout
      ; let unsound =
          match amode with
          | Mupper -> (eval_cost -. q) > Rconfig.float_max_error
          | Mlower -> (q -. eval_cost ) > Rconfig.float_max_error
          | Mconstant -> Float.abs (eval_cost -. q) > Rconfig.float_max_error
        in
	if unsound then
	  let q_s = Float.to_string q in
	  let eval_const_s = Float.to_string eval_cost in
	  let error_string = "Evaluation cost: " ^ eval_const_s ^ "\n" ^ "Derived bound: " ^ q_s in
	  failwith ("Evaluation cost greater then bound using metric "^m_name^".\n" ^ error_string)
        else
          let slack = Float.abs (eval_cost -. q) in
	  fprintf std_formatter "-> okay ( slack: %f )\n" slack
      end
    ; let num_constr = Clp.get_num_constraints () in
      num_constr+acc
  in
  let met_names = List.zip_exn metrics metric_names in
  let num_constr_upper = List.fold2_exn ~init:0 met_names eval_costs ~f:(analyze Mupper) in
  let num_constr_lower = List.fold2_exn ~init:0 met_names eval_costs ~f:(analyze Mlower) in  
  print_string @@ "   Total #constraints upper: " ^ (Int.to_string num_constr_upper) ^ "\n"
  ; print_string @@ "   Total #constraints lower: " ^ (Int.to_string num_constr_lower) ^ "\n"                  



let run_test the_test (exp, env) =
  print_string "   type-checking ...                         "
  ; flush stdout
  ; Typecheck.typecheck exp env
  ; print_string ("-> okay\n")
  ; print_string "   stack-based type-checking ...             "
  ; flush stdout
  ; let _ = Typecheck.typecheck_stack exp env in
    print_string ("-> okay\n")
    ; print_string "   evaluating ...                            "
    ; flush stdout
    ; let (result1, costs1) = Eval.evaluate exp metrics in
      print_string ("-> okay\n")
      ; print_string "   computing SLN ...                         "
      ; flush stdout
      ; let exp_sln = Shareletnormal.share_let_normal "#" exp in
	print_string ("-> okay\n")
	; print_string "   type-checking SLN ...                     "
	; flush stdout
	; Typecheck.typecheck ~linear:true exp_sln env
	; print_string ("-> okay\n")
	; print_string "   stack type-checking SLN ...               "
	; flush stdout
	; let exp_sln_stack = Typecheck.typecheck_stack ~linear:true exp_sln env in
	  print_string ("-> okay\n")
	  ; the_test (result1,costs1) exp_sln exp_sln_stack

  

let main = 
  match Sys.argv with
    | [|_; "eval"; file_name|] -> 
      print_endline ("TESTING EVALUATION: "^ file_name ^ "\n")
      ; print_flush ()
      ; run_test test_eval (Parseraml.parse_raml_from_file file_name)
      ; print_newline ()
    | [|_; "analyze"; degree; file_name|] -> 
      let degree = int_of_string degree in
      print_endline ("TESTING ANALYSIS: "^ file_name ^ "\n")
      ; print_flush ()
      ; run_test (test_analysis degree) (Parseraml.parse_raml_from_file file_name)
      ; print_newline ()
    | _ -> 
      print_endline "Usage: unit_test (eval | analyze DEGREE) FILE"
      ; exit(-1)
