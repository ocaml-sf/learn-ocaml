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
    ?test_student_soln: bool ->
    ('a -> 'b) Ty.ty -> string -> ('a -> 'b) mutant list -> Learnocaml_report.t
  val test_unit_tests_2:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) mutant list -> Learnocaml_report.t
  val test_unit_tests_3:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b -> 'c -> 'd) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd) mutant list
    -> Learnocaml_report.t
  val test_unit_tests_4:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd -> 'e) mutant list
    -> Learnocaml_report.t
  val passed_mutation_testing: Learnocaml_report.t -> bool
end

module Make (Test_lib: Test_lib.S) : S = struct
  open Test_lib

  let typed_printer ty =
    let typed_printer ppf v = Introspection.print_value ppf v ty in
    Format.asprintf "%a" typed_printer
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

  let test_against_fn ?(show_output=false) f printer out_printer (input, expected) =
    let msg = Message ([Text "Running test"; Code (printer input)], Informative) in
    let expected_str = out_printer expected in
    let result = run_test_against f (input, expected) in
    let report =
      match result with
      | Pass -> [Message ([Text "Test passed with output";
                           Code expected_str],
                          Success 0)]
      | Fail out ->
          [Message ([Text "Test failed: expected output";
                     Code expected_str;
                     Text "but got";
                     if show_output then Code (out_printer out)
                     else Text "something else"],
                    Failure)]
      | Err exn ->
          [Message ([Text "Test failed: expected output";
                     Code expected_str;
                     Text "but got an unexpected exception";
                     Code (string_of_exn exn)],
                    Failure)]
    in
    msg :: report

  let section_header = "Your tests..."
  let soln_header = "...against the solution"
  let mutation_header = "...against our buggy implementations"
  let stud_header = "...against your implementation"

  let test_against_mutants ~points muts tests =
    let string_of_num x = "#" ^ (string_of_int x) in
    let test_against_mutant_i i mut =
      test_against_mutant ~points mut (string_of_num (succ i)) tests
    in
    List.mapi test_against_mutant_i muts

  let test_report soln_report stud_section maybe_mut_report =
    let soln_section =
      Section ([Text soln_header], soln_report)
    in
    let mut_report =
      match maybe_mut_report with
      | None ->
          Message ([Text "Some of your tests are incorrect and need to be fixed"],
                    Failure)
      | Some report ->
          Section ([Text mutation_header], report)
    in
    soln_section :: mut_report :: stud_section

  let passed_mutation_testing report =
    match report with
    | [Section ([Text title], items)] when String.equal title section_header ->
        (* Remove the student implementation section, if present *)
        let report' =
          List.filter
            (function
              | Section ([Text title], _) ->
                  not (String.equal title stud_header)
              | _ -> true)
            items
        in
        not (snd (Learnocaml_report.result report'))
    | _ -> false

  type 'a lookup =
    | Unbound of Learnocaml_report.t
    | Found of 'a

  let no_test_cases_report =
    [Message ([Text "You have not yet written any test cases."], Failure)]
  let soln_not_found_msg =
    Message ([Text "Reference solution not found.";
              Text "This is an error with the grader.";
              Text "Please contact your instructor."],
             Failure)

  let append_map f l =
    List.fold_right (fun x acc -> (f x) @ acc) l []

  let test_soln_report soln printer out_printer tests =
    match soln with
    | Unbound report -> soln_not_found_msg :: report
    | Found soln ->
        let tester = test_against_fn soln printer out_printer in
        append_map tester tests

  let test_stud_section stud printer out_printer tests =
    match stud with
    | None -> []
    | Some lookup ->
        let stud_report =
          match lookup with
          | Unbound report -> report
          | Found stud ->
              let tester =
                test_against_fn ~show_output: true stud printer out_printer
              in
              append_map tester tests
        in
        [Section ([Text stud_header], stud_report)]

  let test ~points test_ty printer out_printer name soln stud muts =
    let test_name = name ^ "_tests" in
    let report =
      test_variable_property test_ty test_name @@
      fun tests ->
      if List.length tests = 0 then
        no_test_cases_report
      else
        let soln_report = test_soln_report soln printer out_printer tests in
        let stud_section = test_stud_section stud printer out_printer tests in
        let maybe_mut_report =
          if snd (Learnocaml_report.result soln_report) then None
          else Some (test_against_mutants ~points muts tests)
        in
        test_report soln_report stud_section maybe_mut_report
    in
    [Section ([Text section_header], report)]

  let process_lookup process lookup ty name =
    match lookup ty name () with
    | `Unbound (_, report) -> Unbound report
    | `Found (_, _, data) -> Found (process data)


  let test_unit_tests_1
      ?(points = 1)
      ?(test_student_soln = true)
      ty name muts =
    let (domain, range) = Ty.domains ty in
    let test_ty = Ty.lst (Ty.pair2 domain range) in
    let in_printer = typed_printer domain in
    let printer input = name ^ " " ^ (in_printer input) in
    let out_printer = typed_printer range in
    let soln = process_lookup (fun x -> x) lookup_solution ty name in
    let stud =
      if test_student_soln then
        Some (process_lookup (fun x -> x) lookup_student ty name)
      else None
    in
    test ~points test_ty printer out_printer name soln stud muts

  let test_unit_tests_2
      ?(points = 1)
      ?(test_student_soln = true)
      ty name muts =
    let (dom1, rng) = Ty.domains ty in
    let (dom2, range) = Ty.domains rng in
    let test_ty = Ty.lst (Ty.pair2 (Ty.pair2 dom1 dom2) range) in
    let in1_printer = typed_printer dom1 in
    let in2_printer = typed_printer dom2 in
    let printer (in1, in2) =
      name ^ " " ^ (in1_printer in1) ^ " " ^ (in2_printer in2)
    in
    let out_printer = typed_printer range in
    let muts = List.map (map_snd uncurry2) muts in
    let soln = process_lookup uncurry2 lookup_solution ty name in
    let stud =
      if test_student_soln then
        Some (process_lookup uncurry2 lookup_student ty name)
      else None
    in
    test ~points test_ty printer out_printer name soln stud muts

  let test_unit_tests_3
      ?(points = 1)
      ?(test_student_soln = true)
      ty name muts =
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
    let muts = List.map (map_snd uncurry3) muts in
    let soln = process_lookup uncurry3 lookup_solution ty name in
    let stud =
      if test_student_soln then
        Some (process_lookup uncurry3 lookup_student ty name)
      else None
    in
    test ~points test_ty printer out_printer name soln stud muts

  let test_unit_tests_4
      ?(points = 1)
      ?(test_student_soln = true)
      ty name muts =
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
    let muts = List.map (map_snd uncurry4) muts in
    let soln = process_lookup uncurry4 lookup_solution ty name in
    let stud =
      if test_student_soln then
        Some (process_lookup uncurry4 lookup_student ty name)
      else None
    in
    test ~points test_ty printer out_printer name soln stud muts

end
