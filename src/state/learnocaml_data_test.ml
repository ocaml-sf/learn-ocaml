(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* Unit tests for Learnocaml_data

   See <https://dune.readthedocs.io/en/stable/tests.html#inline-tests> *)

open Learnocaml_data
open Exercise.Status

let%test_module _ =
  (module struct

     let testdata_str_of_globalstatus = function
       | GloballyOpen -> "GloballyOpen"
       | GloballyClosed -> "GloballyClosed"
       | GloballyOpenOrAssigned -> "GloballyOpenOrAssigned"
       | GloballyClosedOrAssigned -> "GloballyClosedOrAssigned"

     let testdata_map_of list =
       List.fold_right (fun (tok, st) res -> Token.Map.add tok st res) list Token.Map.empty

     let testdata_token : Token.t * Token.t  =
       (Token.parse "KOK-W3L-NE1-SVY", Token.parse "JBG-B9B-3S1-NZO")

     let testdata_assigned = Assigned {start=1691877600.; stop=1692482400.}

     let testdata_wrong_assignments =
       List.map (fun (mp, dflt) -> make_assignments mp dflt)
         [(testdata_map_of [fst testdata_token, Open], Closed);
          (testdata_map_of [fst testdata_token, Closed], Open);
          (testdata_map_of [fst testdata_token, Open;
                            snd testdata_token, Closed], testdata_assigned)]

     let testdata_fixed_assignments =
       List.map (fun (mp, dflt) -> make_assignments mp dflt)
         (* Replace Open with Closed in testdata_wrong_assignments *)
         [(testdata_map_of [fst testdata_token, Closed], Closed);
          (testdata_map_of [fst testdata_token, Closed], Closed);
          (testdata_map_of [fst testdata_token, Closed;
                            snd testdata_token, Closed], testdata_assigned)]

     let testdata_good_assignments =
       List.map (fun (mp, dflt) -> make_assignments mp dflt)
         [(*1*) (testdata_map_of [], Open);
          (*2*) (testdata_map_of [], Closed);
          (*3*) (testdata_map_of [], testdata_assigned);
          (*4*) (testdata_map_of [fst testdata_token, Open], testdata_assigned);
          (*5*) (testdata_map_of [fst testdata_token, Closed], testdata_assigned);
          (*6*) (testdata_map_of [fst testdata_token, testdata_assigned], Open);
          (*7*) (testdata_map_of [fst testdata_token, testdata_assigned], Closed);
          (*8*) (testdata_map_of [fst testdata_token, Open], Open);
          (*9*) (testdata_map_of [fst testdata_token, Closed], Closed);
          (*10*)(testdata_map_of [fst testdata_token,
                                  testdata_assigned], testdata_assigned)]

     let%expect_test "is_open_or_assigned_globally" =
       List.iter (fun glob -> Printf.printf "%s\n"
                              @@ testdata_str_of_globalstatus
                              @@ is_open_or_assigned_globally glob)
         testdata_good_assignments;
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
                GloballyClosedOrAssigned |}]

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

   end)
