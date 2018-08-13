(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

module J = Json_encoding

module SMap = struct

  include Map.Make (String)

  let enc val_enc =
    J.conv
      bindings
      (List.fold_left
         (fun acc (n, v) -> add n v acc)
         empty)
      (J.assoc val_enc)

end

module SSet = struct

  include Set.Make (String)

  let enc = J.conv elements of_list (J.list J.string)

end

module Answer = struct

  type t =
    { solution : string ;
      grade : int (* \in [0, 100] *) option ;
      report : Learnocaml_report.t option ;
      mtime : float }

  let enc =
    let grade_enc =
      J.conv
        (fun s -> s)
        (fun s ->
           if s < 0 || s > 100 then
             raise (J.Cannot_destruct ([], Failure "grade overflow"))
           else s)
        J.int in
    J.conv
      (fun { grade ; solution ; report ; mtime } ->
         (grade, solution, report, mtime))
      (fun (grade, solution, report, mtime) ->
         { grade ; solution ; report ; mtime })
      (J.obj4
         (J.opt "grade" grade_enc)
         (J.req "solution" J.string)
         (J.opt "report" Learnocaml_report.enc)
         (J.dft "mtime" J.float 0.))

end

module Report = Learnocaml_report

module Save = struct

  type t =
    { nickname : string ;
      all_exercise_states : Answer.t SMap.t ;
      all_toplevel_histories :
        Learnocaml_toplevel_history.snapshot SMap.t ;
      all_exercise_toplevel_histories :
        Learnocaml_toplevel_history.snapshot SMap.t }

  let enc =
    J.conv
      (fun t ->
        t.nickname,
        t.all_exercise_states,
        t.all_toplevel_histories,
        t.all_exercise_toplevel_histories)
      (fun (nickname,
            all_exercise_states,
            all_toplevel_histories,
            all_exercise_toplevel_histories) ->
        { nickname ;
          all_exercise_states ;
          all_toplevel_histories ;
          all_exercise_toplevel_histories }) @@
    J.obj4
      (J.dft "nickname" J.string "")
      (J.dft "exercises"
         (SMap.enc Answer.enc) SMap.empty)
      (J.dft "toplevel-histories"
         (SMap.enc Learnocaml_toplevel_history.snapshot_enc) SMap.empty)
      (J.dft "exercise-toplevel-histories"
         (SMap.enc Learnocaml_toplevel_history.snapshot_enc) SMap.empty)

  let sync a b =
    let sync_snapshot snapshot_a snapshot_b =
      let open Learnocaml_toplevel_history in
      if snapshot_a.mtime > snapshot_b.mtime then
        snapshot_a
      else
        snapshot_b in
    let sync_exercise_state state_a state_b =
      let open Answer in
      if state_a.mtime > state_b.mtime then
        state_a
      else
        state_b in
    let sync_map sync_item index_a index_b =
      SMap.merge
        (fun _id a b -> match a, b with
           | None, None -> assert false
           | None, Some i | Some i, None -> Some i
           | Some a, Some b -> Some (sync_item a b))
        index_a index_b in
    { nickname = if b.nickname = "" then a.nickname else b.nickname;
      all_exercise_states =
        sync_map sync_exercise_state
          a.all_exercise_states
          b.all_exercise_states ;
      all_toplevel_histories =
        sync_map sync_snapshot
          a.all_toplevel_histories
          b.all_toplevel_histories ;
      all_exercise_toplevel_histories =
        sync_map sync_snapshot
          a.all_exercise_toplevel_histories
          b.all_exercise_toplevel_histories }

  let fix_mtimes save =
    let now = Unix.gettimeofday () in
    let fix t = min t now in
    let fix_snapshot s =
      Learnocaml_toplevel_history.{ s with mtime = fix s.mtime }
    in
    let fix_exercise_state s =
      Answer.{ s with mtime = fix s.mtime }
    in
    {
      save with
      all_exercise_states =
        SMap.map fix_exercise_state save.all_exercise_states;
      all_toplevel_histories =
        SMap.map fix_snapshot save.all_toplevel_histories;
      all_exercise_toplevel_histories =
        SMap.map fix_snapshot save.all_exercise_toplevel_histories;
    }

end

module Token = struct

  type t = string list

  let teacher_token_prefix = "X"

  let to_string = String.concat "-"
  let to_path = String.concat (Filename.dir_sep)
  let teacher_tokens_path = teacher_token_prefix

  let alphabet =
    "ABCDEFGH1JKLMNOPORSTUVWXYZO1Z34SG1B9"
  let visually_equivalent_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  let parse =
    let table = Array.make 256 None in
    String.iter
      (fun c -> Array.set table (Char.code c) (Some c))
      visually_equivalent_alphabet ;
    let translate part =
      String.map (fun c ->
          match Array.get table (Char.code c) with
          | None -> failwith "bad token character"
          | Some c -> c)
        part in
    fun token ->
      let translate_base_token token =
        if String.length token = 15 then
          if String.get token 3 <> '-'
          || String.get token 7 <> '-'
          || String.get token 11 <> '-' then
            failwith "bad token format"
          else
            List.map translate
              [ String.sub token 0 3 ;
                String.sub token 4 3 ;
                String.sub token 8 3 ;
                String.sub token 12 3 ]
        else
          failwith "bad token length"
      in
      if String.length token >= 2 &&
         String.sub token 0 2 = teacher_token_prefix ^ "-"
      then
        teacher_token_prefix ::
        translate_base_token (String.sub token 2 (String.length token - 2))
      else
        translate_base_token token

  let enc = J.conv to_string parse J.string

  let check token =
    try ignore (parse token) ; true
    with _ -> false

  let random () =
    let rand () = String.get alphabet (Random.int (String.length alphabet)) in
    let part () = String.init 3 (fun _ -> rand ()) in
    [ part () ; part () ; part () ; part () ]

  let random_teacher () = teacher_token_prefix :: random ()

  let is_teacher = function
    | x::_ when x = teacher_token_prefix -> true
    | _ -> false
end

type 'a token = Token.t

type student
type teacher

module Student = struct

  type t = {
    token: student token;
    nickname: string option;
    results: (float * int option) SMap.t;
    tags: string list;
  }

  let enc =
    let open Json_encoding in
    obj4
      (req "token" string)
      (opt "nickname" string)
      (dft "results" (assoc (tup2 float (option int))) [])
      (dft "tags" (list string) [])
    |> conv
      (fun t ->
         Token.to_string t.token,
         t.nickname, SMap.bindings t.results, t.tags)
      (fun (token, nickname, results, tags) -> {
           token = Token.parse token;
           nickname;
           results =
             List.fold_left (fun m (s, r) -> SMap.add s r m)
               SMap.empty
               results;
           tags;
         })
end

