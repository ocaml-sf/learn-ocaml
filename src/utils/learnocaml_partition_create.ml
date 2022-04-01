(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Learnocaml_data.Partition

open Learnocaml_report

open Lwt.Infix

open Asak.Monad_error

module IntMap = Map.Make(struct type t = int let compare = compare end)

(* Return a list of all saves with their definition of the function *)
let get_all_saves exo_name prelude =
  Learnocaml_store.Student.Index.get () >>=
    Lwt_list.fold_left_s (* filter_map_rev *)
      (fun acc t ->
        let open ErrS in
        let t = t.Student.token in
        Learnocaml_store.Save.get t >|= fun save ->
        either  (fun x -> x :: acc) (fun _ -> acc) @@ run @@
          begin
            err_of_option "" save
            >>= fun x ->
            err_of_option "" (SMap.find_opt exo_name Save.(x.all_exercise_states))
            >>= fun x ->
            ret (t,x, (prelude ^ "\n" ^ Answer.(x.solution)))
          end
      ) []

(* Return a tuple where
   - The first element contains answer without a grade
   - The second the others *)
let partition_was_graded =
  let aux (nonlst,acc) (a,x,b) =
    match Answer.(x.report) with
    | None -> a::nonlst,acc
    | Some g -> nonlst,(a,g,b)::acc
  in
  List.fold_left aux ([], [])

let partition_by_grade funname =
  let rec get_relative_section = function
    | [] -> []
    | (Message _)::xs -> get_relative_section xs
    | (Section (t,res))::xs | (SectionMin (t,res, _))::xs ->
       match t with
       | Text _::Code  fn::_ ->
          if fn = funname
          then res
          else get_relative_section xs
       | _ -> get_relative_section xs
  in
  let rec get_grade xs =
    let aux acc =
      function
      | Section (_,s) -> get_grade s
      | SectionMin (_, s, min) -> max (get_grade s) min
      | Message (_,s) ->
         match s with
         | Success i -> acc + i
         | _ -> acc
    in
    List.fold_left aux 0 xs
  in
  let aux acc ((_,x,_) as e) =
    let sec = get_relative_section x in
    let g = get_grade sec in
    let lst =
      match IntMap.find_opt g acc with
      | None -> [e]
      | Some xs -> e::xs
    in IntMap.add g lst acc
  in
  List.fold_left aux IntMap.empty

let asak_partition prof fun_name sol by_grade =
  let open Asak in
  IntMap.fold
    (fun i lst (bad_type, res) ->
      let lst = List.map (fun (a,_,b) -> (a,b)) lst in
      let ans = Partition.create prof fun_name sol lst in
      (ans.Partition.bad_type @ bad_type, (i,ans.Partition.clusters) :: res)
    ) by_grade ([],[])

let partition _exo_name _fun_name _prof = assert false (* TODO
  Learnocaml_store.Exercise.get exo_name
  >>= fun exo ->
  let prelude = Learnocaml_exercise.(access File.prelude exo) in
  let prepare = Learnocaml_exercise.(decipher File.prepare exo) in
  let prelude = prelude ^ "\n" ^ prepare in
  let solution = Learnocaml_exercise.(decipher File.solution exo) in
  let solution = prelude ^ "\n" ^ solution in
  get_all_saves exo_name prelude
  >|= fun saves ->
  let not_graded,lst = partition_was_graded saves in
  let by_grade = partition_by_grade fun_name lst in
  let bad_type,partition_by_grade = asak_partition prof fun_name solution by_grade in
  {not_graded; bad_type; partition_by_grade}
*)
