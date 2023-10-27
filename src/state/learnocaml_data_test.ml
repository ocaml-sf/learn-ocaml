(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* Unit tests for Learnocaml_data

   See <https://dune.readthedocs.io/en/stable/tests.html#inline-tests> *)

open Learnocaml_data

let%test_module "Exercise.Status" =
  (module struct

     open Exercise.Status

     let testdata_str_of_globalstatus = function
       | GloballyOpen -> "GloballyOpen"
       | GloballyClosed -> "GloballyClosed"
       | GloballyOpenOrAssigned -> "GloballyOpenOrAssigned"
       | GloballyClosedOrAssigned -> "GloballyClosedOrAssigned"
       | GloballyInconsistent -> "GloballyInconsistent"


     let testdata_smap_of list =
       List.fold_right (fun (e, st) res -> SMap.add e st res) list SMap.empty

     let testdata_list_of_smap map =
       let list =
         SMap.fold (fun e st res -> (e, st) :: res) map []
       in
       List.sort (fun (e1, _) (e2, _) -> compare e1 e2) list

     let testdata_tokmap_of list =
       List.fold_right (fun (tok, st) res -> Token.Map.add tok st res) list Token.Map.empty

     let testdata_list_of_tokmap map =
       let list =
         Token.Map.fold (fun tok st res -> (tok, st) :: res) map []
       in
       List.sort (fun (tok1, _) (tok2, _) -> compare tok1 tok2) list

     let testdata_tokset_of list =
       List.fold_right (fun (tok) res -> Token.Set.add tok res) list Token.Set.empty

     let testdata_sset_of list =
       List.fold_right (fun (tok) res -> SSet.add tok res) list SSet.empty

     let testdata_token : Token.t * Token.t  =
       (Token.parse "JBG-B9B-3S1-NZO", Token.parse "KOK-W3L-NE1-SVY")

     let testdata_assigned_7d = Assigned {start=1691877600.; stop=1692482400.}

     let testdata_assigned_8d = Assigned {start=1691877600.; stop=1692568800.}

     let testdata_7d = (1691877600., 1692482400.)

     let testdata_8d = (1691877600., 1692568800.)

     let testdata_wrong_assignments =
       List.map (fun (mp, dflt) -> make_assignments mp dflt)
         [(testdata_tokmap_of [fst testdata_token, Open], Closed);
          (testdata_tokmap_of [fst testdata_token, Closed], Open);
          (testdata_tokmap_of [fst testdata_token, Open;
                            snd testdata_token, Closed], testdata_assigned_7d)]

     let testdata_fixed_assignments =
       List.map (fun (mp, dflt) -> make_assignments mp dflt)
         (* Replace Open with Closed in testdata_wrong_assignments *)
         [(testdata_tokmap_of [fst testdata_token, Closed], Closed);
          (testdata_tokmap_of [fst testdata_token, Closed], Closed);
          (testdata_tokmap_of [fst testdata_token, Closed;
                            snd testdata_token, Closed], testdata_assigned_7d)]

     let testdata_good_assignments =
       List.map (fun (mp, dflt) -> make_assignments mp dflt)
         [(*1*) (testdata_tokmap_of [], Open);
          (*2*) (testdata_tokmap_of [], Closed);
          (*3*) (testdata_tokmap_of [], testdata_assigned_7d);
          (*4*) (testdata_tokmap_of [fst testdata_token, Open], testdata_assigned_7d);
          (*5*) (testdata_tokmap_of [fst testdata_token, Closed], testdata_assigned_7d);
          (*6*) (testdata_tokmap_of [fst testdata_token, testdata_assigned_7d], Open);
          (*7*) (testdata_tokmap_of [fst testdata_token, testdata_assigned_7d], Closed);
          (*8*) (testdata_tokmap_of [fst testdata_token, Open], Open);
          (*9*) (testdata_tokmap_of [fst testdata_token, Closed], Closed);
          (*10*)(testdata_tokmap_of [fst testdata_token, testdata_assigned_7d], testdata_assigned_7d)]

     let%expect_test "is_open_or_assigned_globally/full" =
       List.iter (fun glob -> Printf.printf "%s\n"
                              @@ testdata_str_of_globalstatus
                              @@ is_open_or_assigned_globally glob)
         (testdata_good_assignments @ testdata_wrong_assignments);
       [%expect{|
                GloballyOpen
                GloballyClosed
                GloballyClosedOrAssigned
                GloballyOpenOrAssigned
                GloballyClosedOrAssigned
                GloballyOpenOrAssigned
                GloballyClosedOrAssigned
                GloballyOpen
                GloballyClosed
                GloballyClosedOrAssigned
                GloballyInconsistent
                GloballyInconsistent
                GloballyInconsistent |}]

     let%test "check_open_close/good" =
       List.for_all check_open_close testdata_good_assignments

     let%test "check_open_close/wrong" =
       List.for_all (fun a -> not (check_open_close a))
         testdata_wrong_assignments

     let%test "check_open_close/fixed" =
       List.for_all check_open_close testdata_fixed_assignments

     let%test "fix_open_close" =
       List.for_all2 (fun a fx -> fix_open_close a = fx)
         testdata_wrong_assignments testdata_fixed_assignments

     let%test "check_and_fix_open_close/wrong=fixed" =
       List.for_all2 (fun a fx -> check_and_fix_open_close a = fx)
         testdata_wrong_assignments testdata_fixed_assignments

     let%test "check_and_fix_open_close/good=id" =
       List.for_all (fun a -> check_and_fix_open_close a = a)
         testdata_good_assignments

     let testdata_make_t id a dflt =
       {id;
        skills_prereq = []; (* independent of the SUT *)
        skills_focus = []; (* independent of the SUT *)
        assignments = make_assignments a dflt}

     (* - initial status_map *)
     let testdata_s0 : t SMap.t =
       testdata_smap_of
         [(* testdata_t0_a7d *)
          ("exo1", testdata_make_t "exo1"
                     (testdata_tokmap_of [snd testdata_token, Open]) testdata_assigned_7d);
          ("exo2", testdata_make_t "exo2"
                     (testdata_tokmap_of [snd testdata_token, Closed]) testdata_assigned_7d);
          ("exo3", testdata_make_t "exo3"
                     (testdata_tokmap_of []) testdata_assigned_7d);
          (* testdata_t0_a8d *)
          ("exo4", testdata_make_t "exo4"
                     (testdata_tokmap_of [fst testdata_token, testdata_assigned_8d]) Open);
          ("exo5", testdata_make_t "exo5"
                     (testdata_tokmap_of [fst testdata_token, testdata_assigned_8d]) Closed);
          (* testdata_t0_noa *)
          ("exo6", testdata_make_t "exo6"
                     (testdata_tokmap_of []) Open);
          ("exo7", testdata_make_t "exo7"
                     (testdata_tokmap_of []) Closed);
         ]

     (* let testdata_s00 : t SMap.t = SMap.empty *)

     type triple = Token.Set.t * SSet.t * bool

     let testdata_t0_a7d : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo1"; "exo2"; "exo3"],
        true)

     let testdata_t0_a8d : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo4"; "exo5"],
        false)

     (* let testdata_t0_noa : triple =
       (testdata_tokset_of [],
        testdata_sset_of [],
        false) *)

     let%expect_test "is_open_or_assigned_globally/testdata_s0" =
       List.iter (fun glob -> Printf.printf "%s\n"
                              @@ testdata_str_of_globalstatus
                              @@ is_open_or_assigned_globally glob)
         (List.map (fun (_e, s) -> s.assignments) @@ testdata_list_of_smap testdata_s0);
       [%expect {|
         GloballyOpenOrAssigned
         GloballyClosedOrAssigned
         GloballyClosedOrAssigned
         GloballyOpenOrAssigned
         GloballyClosedOrAssigned
         GloballyOpen
         GloballyClosed |}]

     let testdata_str_of_status = function
       | Open -> "Open"
       | Closed -> "Closed"
       | Assigned {start; stop} -> Printf.sprintf "Assigned(%s,%s)"
                                    (string_of_float start)
                                    (string_of_float stop)

     let testdata_str_of_toklist list =
       List.fold_right (fun (tok, st) r ->
           Printf.sprintf "(%s, %s); %s"
             (Token.to_string tok)
             (testdata_str_of_status st)
             r) list ""

     let testdata_str_of_assignments (s : assignments) =
       Printf.sprintf "{default = %s; token_map = { %s}}"
         (testdata_str_of_status s.default)
         (testdata_str_of_toklist @@
            testdata_list_of_tokmap s.token_map)

     let testdata_str_of_t (s : t) =
       Printf.sprintf "{id = %s; assignments = %s}"
         s.id
         (testdata_str_of_assignments s.assignments)

     (* TODO: Change globalstatus *)
     let testdata_str_of_smap ?(globalstatus=false) (s : t SMap.t) =
       let list =
         SMap.fold (fun eid tt acc ->
             if eid <> tt.id
             then failwith (Printf.sprintf "eid(%s) <> tt.id(%s)" eid tt.id)
             else (eid, testdata_str_of_t tt ^
                          (if globalstatus
                           then " -> " ^
                                  testdata_str_of_globalstatus
                                  @@ is_open_or_assigned_globally
                                  @@ tt.assignments
                           else "")) :: acc) s []
       in
       let list =
         List.sort (fun (eid1, _) (eid2, _) -> compare eid1 eid2) list
       in
       List.fold_right (fun (_, st) r -> st ^ "\n" ^ r) list ""

     let testdata_get_status_t status_map id =
       try SMap.find id status_map with Not_found ->
         default id

     let testdata_stud_map =
       let stud_of_tok tk = (tk, Student.default tk) in
       testdata_tokmap_of [stud_of_tok @@ fst testdata_token;
                           stud_of_tok @@ snd testdata_token]

     (* Test cases candidates :
        Usage of function set_assignment in learnocaml_teacher_tab.ml
        - set_assignment aid ~exos (*selected exos*)
        - set_assignment aid ~students ~default (*selected students*)
        - set_assignment aid ~assg:(start,stop) (*change assignment*)
        - set_assignment aid ~students:() ~exos:() (*assignment_remove*)
      *)

     let testdata_t1_a7d_rm_exo1 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo2"; "exo3"],(*<-*)
        true)
     let testdata_t1_a7d_rm_exo2 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo1"; "exo3"],(*<-*)
        true)
     let testdata_t1_a7d_rm_exo3 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo1"; "exo2"],(*<-*)
        true)
     let testdata_t1_a7d_add_exo6_7 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo1"; "exo2"; "exo3"; "exo6"; "exo7"],(*<-*)
        true)

     let testdata_t1_a8d_rm_exo4 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo5"],(*<-*)
        false)
     let testdata_t1_a8d_rm_exo5 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo4"],(*<-*)
        false)
     let testdata_t1_a8d_add_exo6_7 : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo4"; "exo5"; "exo6"; "exo7"],(*<-*)
        false)

     let%expect_test "update_exercise_assignments/set_exos" =
       Printf.printf "# initial status_map:\n%s\n" @@
         testdata_str_of_smap testdata_s0;
       List.iter (fun (label, tri0, tri, dates) ->
           Printf.printf "# %s:\n%s\n"
             label (testdata_str_of_smap @@
                      update_exercise_assignments (testdata_get_status_t testdata_s0)
                        tri0 tri dates testdata_stud_map testdata_s0))
         ["a7d_rm_exo1", testdata_t0_a7d, testdata_t1_a7d_rm_exo1, testdata_7d;
          "a7d_rm_exo2", testdata_t0_a7d, testdata_t1_a7d_rm_exo2, testdata_7d;
          "a7d_rm_exo3", testdata_t0_a7d, testdata_t1_a7d_rm_exo3, testdata_7d;
          "a7d_add_exo6_7_true", testdata_t0_a7d, testdata_t1_a7d_add_exo6_7, testdata_7d;
          "a8d_rm_exo4", testdata_t0_a8d, testdata_t1_a8d_rm_exo4, testdata_8d;
          "a8d_rm_exo5", testdata_t0_a8d, testdata_t1_a8d_rm_exo5, testdata_8d;
          "a8d_add_exo6_7_false", testdata_t0_a8d, testdata_t1_a8d_add_exo6_7, testdata_8d;
         ];
       [%expect {|
         # initial status_map:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_rm_exo1:
         {id = exo1; assignments = {default = Open; token_map = { }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_rm_exo2:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Closed; token_map = { }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_rm_exo3:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Closed; token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_add_exo6_7_true:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo7; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}

         # a8d_rm_exo4:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a8d_rm_exo5:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a8d_add_exo6_7_false:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo7; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}} |}]

     let testdata_t2_a7d_add_stu2 : triple =
       (testdata_tokset_of [fst testdata_token; snd testdata_token],(*<-*)
        testdata_sset_of ["exo1"; "exo2"; "exo3"],
        true)

     let testdata_t2_a7d_rm_stu1 : triple =
       (testdata_tokset_of [],(*<-*)
        testdata_sset_of ["exo1"; "exo2"; "exo3"],
        true)

     let testdata_t2_a7d_rm_dflt : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo1"; "exo2"; "exo3"],
        false(*<-*))

     let testdata_t2_a8d_add_stu2 : triple =
       (testdata_tokset_of [fst testdata_token; snd testdata_token],(*<-*)
        testdata_sset_of ["exo4"; "exo5"],
        false)

     let testdata_t2_a8d_add_dflt : triple =
       (testdata_tokset_of [fst testdata_token],
        testdata_sset_of ["exo4"; "exo5"],
        true(*<-*))

     let%expect_test "update_exercise_assignments/set_students" =
       Printf.printf "# initial status_map:\n%s\n" @@
         testdata_str_of_smap testdata_s0;
       List.iter (fun (label, tri0, tri, dates) ->
           Printf.printf "# %s:\n%s\n"
             label (testdata_str_of_smap @@
                      update_exercise_assignments (testdata_get_status_t testdata_s0)
                        tri0 tri dates testdata_stud_map testdata_s0))
         ["a7d_add_stu2", testdata_t0_a7d, testdata_t2_a7d_add_stu2, testdata_7d;
          "a7d_rm_stu1", testdata_t0_a7d, testdata_t2_a7d_rm_stu1, testdata_7d;
          "a7d_rm_dflt", testdata_t0_a7d, testdata_t2_a7d_rm_dflt, testdata_7d;
          "a8d_add_stu2", testdata_t0_a8d, testdata_t2_a8d_add_stu2, testdata_8d;
          "a8d_add_dflt", testdata_t0_a8d, testdata_t2_a8d_add_dflt, testdata_8d;
         ];
       [%expect {|
         # initial status_map:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_add_stu2:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_rm_stu1:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (JBG-B9B-3S1-NZO, Open); (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (JBG-B9B-3S1-NZO, Closed); (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (JBG-B9B-3S1-NZO, Closed); }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_rm_dflt:
         {id = exo1; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692482400.)); }}}
         {id = exo2; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692482400.)); }}}
         {id = exo3; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692482400.)); (KOK-W3L-NE1-SVY, Assigned(1691877600.,1692482400.)); }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a8d_add_stu2:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); (KOK-W3L-NE1-SVY, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); (KOK-W3L-NE1-SVY, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a8d_add_dflt:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Assigned(1691877600.,1692568800.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo5; assignments = {default = Assigned(1691877600.,1692568800.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}} |}]

     let testdata_t3_a7d_rm_assignment : triple =
       (testdata_tokset_of [],(*<-*)
        testdata_sset_of [],(*<-*)
        true)

     let testdata_t3_a8d_rm_assignment : triple =
       (testdata_tokset_of [],(*<-*)
        testdata_sset_of [],(*<-*)
        false)

     let%expect_test "update_exercise_assignments/rm_assignment" =
       Printf.printf "# initial status_map:\n%s\n" @@
         testdata_str_of_smap testdata_s0;
       List.iter (fun (label, tri0, tri, dates) ->
           Printf.printf "# %s:\n%s\n"
             label (testdata_str_of_smap @@
                      update_exercise_assignments (testdata_get_status_t testdata_s0)
                        tri0 tri dates testdata_stud_map testdata_s0))
         ["a7d_rm_assignment", testdata_t0_a7d, testdata_t3_a7d_rm_assignment, testdata_7d;
          "a8d_rm_assignment", testdata_t0_a8d, testdata_t3_a8d_rm_assignment, testdata_8d];
       [%expect {|
         # initial status_map:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_rm_assignment:
         {id = exo1; assignments = {default = Open; token_map = { }}}
         {id = exo2; assignments = {default = Closed; token_map = { }}}
         {id = exo3; assignments = {default = Closed; token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a8d_rm_assignment:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { }}}
         {id = exo5; assignments = {default = Closed; token_map = { }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}} |}]

     let%expect_test "update_exercise_assignments/set_assignment" =
       Printf.printf "# initial status_map:\n%s\n" @@
         testdata_str_of_smap testdata_s0;
       List.iter (fun (label, tri0, tri, dates) ->
           Printf.printf "# %s:\n%s\n"
             label (testdata_str_of_smap @@ (* let _ = *)
                      update_exercise_assignments (testdata_get_status_t testdata_s0)
                      tri0 tri dates testdata_stud_map testdata_s0 (* in testdata_s0 *)))
         ["a7d_to_a8d", testdata_t0_a7d, testdata_t0_a7d, testdata_8d];
       [%expect {|
         # initial status_map:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692482400.); token_map = { }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}}

         # a7d_to_a8d:
         {id = exo1; assignments = {default = Assigned(1691877600.,1692568800.); token_map = { (KOK-W3L-NE1-SVY, Open); }}}
         {id = exo2; assignments = {default = Assigned(1691877600.,1692568800.); token_map = { (KOK-W3L-NE1-SVY, Closed); }}}
         {id = exo3; assignments = {default = Assigned(1691877600.,1692568800.); token_map = { (KOK-W3L-NE1-SVY, Assigned(1691877600.,1692482400.)); }}}
         {id = exo4; assignments = {default = Open; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo5; assignments = {default = Closed; token_map = { (JBG-B9B-3S1-NZO, Assigned(1691877600.,1692568800.)); }}}
         {id = exo6; assignments = {default = Open; token_map = { }}}
         {id = exo7; assignments = {default = Closed; token_map = { }}} |}]

   end)
