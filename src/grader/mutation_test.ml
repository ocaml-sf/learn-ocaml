open Learnocaml_report

type 'a test_result =
  | Pass
  | Fail of 'a
  | Err of exn

type 'a mutant = string * 'a

let run_test_against f (input, expected) =
  try
    let output = f input in
    if output = expected then Pass
    else Fail output
  with exn -> Err exn

let run_test_against_mutant f (input, expected) =
  match run_test_against f (input, expected) with
  | Pass -> false
  | _ -> true


let uncurry2 f = fun (x, y) -> f x y
let uncurry3 f = fun (x, y, z) -> f x y z
let uncurry4 f = fun (x, y, z, w) -> f x y z w
let map_snd f = fun (x, y) -> (x, f y)

module type S = sig
  val test_unit_tests_1:
    ?points: int ->
    ('a -> 'b) Ty.ty -> string -> ('a -> 'b) mutant list -> Learnocaml_report.t
  val test_unit_tests_2:
    ?points: int ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) mutant list -> Learnocaml_report.t
  val test_unit_tests_3:
    ?points: int ->
    ('a -> 'b -> 'c -> 'd) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd) mutant list
    -> Learnocaml_report.t
  val test_unit_tests_4:
    ?points: int ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd -> 'e) mutant list
    -> Learnocaml_report.t
  val passed_mutation_testing: Learnocaml_report.t -> bool
end

module Make (Test_lib: Test_lib.S) : S = struct
  open Test_lib

  let typed_printer ty =
    let typed_printer ty ppf v = Introspection.print_value ppf v ty in
    Format.asprintf "%a" (typed_printer ty)
  let string_of_exn = typed_printer [%ty: exn]

  let test_against_mutant ~points (name, mut) num tests =
    let result = List.exists (run_test_against_mutant mut) tests in
    if result then
      Message
        ([Text "Your tests successfully revealed the bug in implementation";
          Text num;
          Text ": ";
          Text name],
         Success points)
    else
      Message
        ([Text "Your tests did not expose the bug in implementation"; Text num],
         Failure)

  let test_against_solution soln printer out_printer (input, expected) =
    let msg = Message ([Text "Running test"; Code (printer input)], Informative) in
    let expected_str = out_printer expected in
    let result = run_test_against soln (input, expected) in
    let report =
      match result with
      | Pass -> [Message ([Text "Test passed with output";
                           Code expected_str],
                          Important)]
      | Fail _ ->
          [Message ([Text "Test failed: expected output";
                     Code expected_str;
                     Text "but got something else"],
                    Failure)]
      | Err exn ->
          [Message ([Text "Test failed: expected output";
                     Code expected_str;
                     Text "but got an unexpected exception";
                     Code (string_of_exn exn)],
                    Failure)]
    in
    msg :: report


  let test_against_mutants ~points muts tests =
    let string_of_num x = "#" ^ (string_of_int x) in
    let test_against_mutant_i i mut =
      test_against_mutant ~points mut (string_of_num (succ i)) tests
    in
    List.mapi test_against_mutant_i muts

  let test_report soln_report maybe_mut_report =
    let soln_section =
      Section ([Text "...against the solution"], soln_report)
    in
    let mut_report =
      match maybe_mut_report with
      | None ->
          [Message ([Text "Some of your tests are incorrect and need to be fixed"],
                    Failure)]
      | Some report ->
          [Section ([Text "...against our buggy implementations"], report)]
    in
    soln_section :: mut_report

  let test ~points test_ty printer out_printer name soln muts =
    let test_name = name ^ "_tests" in
    let report =
      test_variable_property test_ty test_name @@
      fun tests ->
      if List.length tests = 0 then
        [Message ([Text "You have not yet written any test cases."], Failure)]
      else
        let tester = test_against_solution soln printer out_printer in
        let soln_report =
          List.fold_right (fun test acc -> (tester test) @ acc) tests []
        in
        let maybe_mut_report =
          if snd (Learnocaml_report.result soln_report) then None
          else Some (test_against_mutants ~points muts tests)
        in
        test_report soln_report maybe_mut_report
    in
    [Section ([Text "Your tests..."], report)]


  let test_unit_tests_1 ?(points=1) ty name muts =
    let (domain, range) = Ty.domains ty in
    let test_ty = Ty.lst (Ty.pair2 domain range) in
    let in_printer = typed_printer domain in
    let printer input = name ^ " " ^ (in_printer input) in
    let out_printer = typed_printer range in
    let soln = lookup_solution ty name () in
    match soln with
    | `Unbound (_, report) -> report (* this should never happen *)
    | `Found (_, _, soln) ->
        test ~points test_ty printer out_printer name soln muts

  let test_unit_tests_2 ?(points=1) ty name muts =
    let (dom1, rng) = Ty.domains ty in
    let (dom2, range) = Ty.domains rng in
    let test_ty = Ty.lst (Ty.pair2 (Ty.pair2 dom1 dom2) range) in
    let in1_printer = typed_printer dom1 in
    let in2_printer = typed_printer dom2 in
    let printer (in1, in2) =
      name ^ " " ^ (in1_printer in1) ^ " " ^ (in2_printer in2)
    in
    let out_printer = typed_printer range in
    let muts = List.map uncurry2 muts in
    let soln = lookup_solution ty name () in
    match soln with
    | `Unbound (_, report) -> report (* this should never happen *)
    | `Found (_, _, soln) ->
        let soln = uncurry2 soln in
        test ~points test_ty printer out_printer name soln muts

  let test_unit_tests_3 ?(points=1) ty name muts =
    let (dom1, rng1) = Ty.domains ty in
    let (dom2, rng2) = Ty.domains rng1 in
    let (dom3, range) = Ty.domains rng2 in
    let test_ty =
      Ty.lst (Ty.pair2 (Ty.pair3 dom1 dom2 dom3) range)
    in
    let in1_printer = typed_printer dom1 in
    let in2_printer = typed_printer dom2 in
    let in3_printer = typed_printer dom3 in
    let printer (in1, in2, in3) =
      name ^ " " ^ (in1_printer in1)
      ^ " " ^ (in2_printer in2)
      ^ " " ^ (in3_printer in3)
    in
    let out_printer = typed_printer range in
    let muts = List.map uncurry3 muts in
    let soln = lookup_solution ty name () in
    match soln with
    | `Unbound (_, report) -> report (* this should never happen *)
    | `Found (_, _, soln) ->
        let soln = uncurry3 soln in
        test ~points test_ty printer out_printer name soln muts

  let test_unit_tests_4 ?(points=1) ty name muts =
    let (dom1, rng1) = Ty.domains ty in
    let (dom2, rng2) = Ty.domains rng1 in
    let (dom3, rng3) = Ty.domains rng2 in
    let (dom4, range) = Ty.domains rng3 in
    let test_ty =
      Ty.lst (Ty.pair2 (Ty.pair4 dom1 dom2 dom3 dom4) range)
    in
    let in1_printer = typed_printer dom1 in
    let in2_printer = typed_printer dom2 in
    let in3_printer = typed_printer dom3 in
    let in4_printer = typed_printer dom4 in
    let printer (in1, in2, in3, in4) =
      name ^ " " ^ (in1_printer in1)
      ^ " " ^ (in2_printer in2)
      ^ " " ^ (in3_printer in3)
      ^ " " ^ (in4_printer in4)
    in
    let out_printer = typed_printer range in
    let muts = List.map uncurry4 muts in
    let soln = lookup_solution ty name () in
    match soln with
    | `Unbound (_, report) -> report (* this should never happen *)
    | `Found (_, _, soln) ->
        let soln = uncurry4 soln in
        test ~points test_ty printer out_printer name soln muts

end
