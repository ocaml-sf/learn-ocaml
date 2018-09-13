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

  let merge3 ~ancestor ~theirs ~ours =
    let (++), (--), (%%) = union, diff, inter in
    (ancestor %% theirs %% ours) ++ (theirs -- ancestor) ++ (ours -- ancestor)

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
        (function
          | Some n when n < 0 || n > 100 -> None
          | g -> g)
        (function
          | Some s when s < 0 || s > 100 -> failwith "grade overflow"
          | g -> g)
        J.(option int) in
    J.conv
      (fun { grade ; solution ; report ; mtime } ->
         (grade, solution, report, mtime))
      (fun (grade, solution, report, mtime) ->
         { grade ; solution ; report ; mtime })
      (J.obj4
         (J.dft "grade" grade_enc None)
         (J.req "solution" J.string)
         (J.opt "report" Learnocaml_report.enc)
         (J.dft "mtime" J.float 0.))

end

module Report = Learnocaml_report

module Save = struct

  type t =
    { nickname : string ;
      all_exercise_editors : (float * string) SMap.t ;
      all_exercise_states : Answer.t SMap.t ;
      all_toplevel_histories :
        Learnocaml_toplevel_history.snapshot SMap.t ;
      all_exercise_toplevel_histories :
        Learnocaml_toplevel_history.snapshot SMap.t }

  let enc =
    J.conv
      (fun t ->
        t.nickname,
        t.all_exercise_editors,
        t.all_exercise_states,
        t.all_toplevel_histories,
        t.all_exercise_toplevel_histories)
      (fun (nickname,
            all_exercise_editors,
            all_exercise_states,
            all_toplevel_histories,
            all_exercise_toplevel_histories) ->
        { nickname ;
          all_exercise_editors ;
          all_exercise_states ;
          all_toplevel_histories ;
          all_exercise_toplevel_histories }) @@
    J.obj5
      (J.dft "nickname" J.string "")
      (J.dft "exercises-editors"
         (SMap.enc (J.tup2 J.float J.string)) SMap.empty)
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
    let sync_exercise_edits (ts_a, _ as a) (ts_b, _ as b) =
      if ts_a > ts_b then a else b
    in
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
      all_exercise_editors =
        sync_map sync_exercise_edits
          a.all_exercise_editors
          b.all_exercise_editors;
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
    let fix_exercise_edits (ts, e) = fix ts, e in
    let fix_exercise_state s =
      Answer.{ s with mtime = fix s.mtime }
    in
    {
      save with
      all_exercise_editors =
        SMap.map fix_exercise_edits save.all_exercise_editors;
      all_exercise_states =
        SMap.map fix_exercise_state save.all_exercise_states;
      all_toplevel_histories =
        SMap.map fix_snapshot save.all_toplevel_histories;
      all_exercise_toplevel_histories =
        SMap.map fix_snapshot save.all_exercise_toplevel_histories;
    }

  let empty = {
    all_exercise_editors = SMap.empty;
    all_exercise_states = SMap.empty;
    all_toplevel_histories = SMap.empty;
    all_exercise_toplevel_histories = SMap.empty;
    nickname = "";
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
    String.iteri
      (fun i c -> Array.set table (Char.code c) (Some alphabet.[i]))
      visually_equivalent_alphabet ;
    let translate part =
      String.map (fun c ->
          match Array.get table (Char.code c) with
          | None -> failwith "bad token character"
          | Some c -> c)
        part in
    fun token ->
      let token = String.trim token in
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

  let is_student t = not (is_teacher t)

  module T = struct
    type nonrec t = t
    let compare = Pervasives.compare
  end

  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

type 'a token = Token.t

type student
type teacher

module Student = struct

  type t = {
    token: student token;
    nickname: string option;
    results: (float * int option) SMap.t;
    tags: SSet.t;
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
         t.nickname, SMap.bindings t.results, SSet.elements t.tags)
      (fun (token, nickname, results, tags) -> {
           token = Token.parse token;
           nickname;
           results =
             List.fold_left (fun m (s, r) -> SMap.add s r m)
               SMap.empty
               results;
           tags = SSet.of_list tags;
         })

  let default token = {
    token;
    nickname = None;
    results = SMap.empty;
    tags = SSet.empty;
  }

  let three_way_merge ~ancestor ~theirs ~ours =
    let token = ancestor.token in
    if token <> theirs.token || token <> ours.token then
      invalid_arg "three_way_merge";
    let tags = SSet.merge3
        ~ancestor:ancestor.tags
        ~theirs:theirs.tags
        ~ours:ours.tags
    in
    let nickname =
      if ours.nickname <> ancestor.nickname
      then ours.nickname
      else theirs.nickname
    in
    let results =
      SMap.merge (fun id a o ->
          if a <> o then o else
          SMap.find_opt id theirs.results)
        ancestor.results ours.results
    in
    { token; tags; nickname; results }

  module Index = struct

    type nonrec t = t list

    let enc = J.(list enc)

  end
end

let enc_check_version_1 enc =
  J.conv
    (fun exercise -> ("1", exercise))
    (fun (version, exercise) ->
       if version <> "1" then begin
         let msg = Format.asprintf "unknown version %s" version in
         raise (J.Cannot_destruct ([], Failure msg))
       end ;
       exercise)
    (J.merge_objs (J.obj1 (J.req "learnocaml_version" J.string)) enc)

let enc_check_version_2 enc =
  J.conv
    (fun exercise -> ("2", exercise))
    (fun (version, exercise) ->
       begin
         match version with
         | "1" | "2" -> ()
         | _ ->
             let msg = Format.asprintf "unknown version %s" version in
             raise (J.Cannot_destruct ([], Failure msg))
       end ;
       exercise)
    (J.merge_objs (J.obj1 (J.req "learnocaml_version" J.string)) enc)

module Exercise = struct

  type id = string

  type t = Learnocaml_exercise.t

  let enc = Learnocaml_exercise.encoding

  module Meta = struct

    type kind =
      | Project
      | Problem
      | Exercise

    type t = {
      kind: kind;
      title: string;
      short_description: string option;
      stars: float (* \in [0.,4.] *);
      id: id option;
      author: (string * string) list;
      focus: string list;
      requirements: string list;
      forward: id list;
      backward: id list;
    }

    let enc =
      let kind_enc =
        J.string_enum
          [ "problem", Problem ;
            "project", Project ;
            "exercise", Exercise ]
      in
      let exercise_enc_v1 =
        J.(obj10
             (req "kind" kind_enc)
             (dft "title" string "")
             (opt "short_description" string)
             (req "stars" float)
             (opt "identifier" string)
             (dft "authors" (list (tup2 string string)) [])
             (dft "focus" (list string) [])
             (dft "requirements" (list string) [])
             (dft "forward_exercises" (list string) [])
             (dft "backward_exercises" (list string) []))
      in
      let exercise_enc_v2 =
        J.(obj1
             (opt "max_score" int))
        (* deprecated & ignored *)
      in
      J.conv
        (fun t ->
           ((t.kind, t.title, t.short_description, t.stars, t.id,
             t.author, t.focus, t.requirements, t.forward, t.backward),
            None))
        (fun ((kind, title, short_description, stars, id,
               author, focus, requirements, forward, backward),
              _max_score) ->
          { kind; title; short_description; stars; id;
            author; focus; requirements; forward; backward;
          })
        (enc_check_version_2
           (J.merge_objs
              exercise_enc_v1
              exercise_enc_v2))

  end

  module Status = struct

    type tag = string

    type status =
      | Open
      | Closed
      | Assigned of {start: float; stop: float}

    type assignments = {
      token_map: status Token.Map.t;
      default: status;
    }

    type t = {
      id: id;
      tags: tag list;
      assignments: assignments;
    }

    let empty_assignments = {
      token_map = Token.Map.empty;
      default = Open;
    }

    let default_assignment a = a.default

    let set_default_assignment a default = {a with default}

    let get_status token a =
      match Token.Map.find_opt token a.token_map with
      | Some a -> a
      | None -> a.default

    let is_open_assignment token a =
      match get_status token a with
      | Assigned a ->
         let t = Unix.gettimeofday () in
         if t < a.start then `Closed
         else `Deadline (a.stop -. t)
      | Open -> `Open
      | Closed -> `Closed

    module STM = Map.Make(struct
        type t = status
        let compare = compare
      end)

    let by_status_explicit base a =
      Token.Map.fold (fun tok st sm ->
          match STM.find_opt st sm with
          | None -> STM.add st (Token.Set.singleton tok) sm
          | Some toks -> STM.add st (Token.Set.add tok toks) sm)
        a.token_map
        base

    let by_status tokens a =
      let rem_tokens =
        Token.Set.filter (fun tok -> not (Token.Map.mem tok a.token_map)) tokens
      in
      let base =
        if Token.Set.is_empty rem_tokens then STM.empty
        else STM.singleton a.default rem_tokens
      in
      by_status_explicit base a |> STM.bindings

    let three_way_merge ~ancestor ~theirs ~ours =
      let id = ancestor.id in
      if id <> theirs.id || id <> ours.id then
        invalid_arg "three_way_merge";
      let tags =
        SSet.merge3
          ~ancestor:(SSet.of_list ancestor.tags)
          ~theirs:(SSet.of_list theirs.tags)
          ~ours:(SSet.of_list ours.tags)
        |> SSet.elements
      in
      let default =
        if ours.assignments.default <> ancestor.assignments.default
        then ours.assignments.default
        else theirs.assignments.default
      in
      let token_map =
        Token.Map.merge (fun tok a o ->
            let a = match a with
              | None -> ancestor.assignments.default
              | Some st -> st
            in
            let t =
              match Token.Map.find_opt tok theirs.assignments.token_map with
              | None -> theirs.assignments.default
              | Some st -> st
            in
            let o = match o with
              | None -> ours.assignments.default
              | Some st -> st
            in
            if o <> a then Some o else Some t)
          ancestor.assignments.token_map
          ours.assignments.token_map
      in
      let token_map =
        Token.Map.merge (fun _ t o -> match t, o with
            | _, (Some st as s) | (Some st as s), _ ->
                if st = default then None else s
            | None, None -> assert false)
          theirs.assignments.token_map
          token_map
      in
      { id; tags; assignments = { default; token_map } }

    (* let make_status tokens default *) 

    (* let exists_assignment a pred =
     *   Token.Map.exists pred a.token_map ||
     *   match a.default with
     *   | None -> false
     *   | Some df -> *) 

    (* let fold_over_assignments a f init =
     *   Token.Map.fold f a.token_map init *)

    (* let token_map_of_assignments a =
     *   a.token_map *)

    let make_assignments token_map default =
      { token_map; default }

    (* let is_token_eligible _ _ =
     *   true
     * 
     * let default_assignment a =
     *   a.default_assignment
     * 
     * let assignment_for_token a _ =
     *   a.default_assignment *)

    (* let consider_token_for_assignment a t =
     *   if is_token_eligible a t then
     *     let assignment = assignment_for_token a t in
     *     Some { a with token_map = Token.Map.add t assignment a.token_map }
     *   else
     *     None
     * 
     * type status =
     *   | Open
     *   | Closed
     *   | Assigned of assignments *)

    let enc =
      let status_enc =
        J.union [
          J.case (J.constant "Open")
            (function Open -> Some () | _ -> None) (fun () -> Open);
          J.case (J.constant "Closed")
            (function Closed -> Some () | _ -> None) (fun () -> Closed);
          J.case
            (J.obj2 (J.req "start" J.float) (J.req "stop" J.float))
            (function Assigned a -> Some (a.start, a.stop) | _ -> None)
            (fun (start, stop) -> Assigned {start; stop})
        ]
      in
      let assignments_enc =
        J.conv
          (fun t ->
             t.default,
             List.map (fun (tk, st) -> Token.to_string tk, st)
               (Token.Map.bindings t.token_map))
          (fun (default, token_assoc) -> {
               default;
               token_map =
                 (List.fold_left (fun acc (tok, st) ->
                      Token.Map.add (Token.parse tok) st acc)
                     Token.Map.empty token_assoc)
             })
        @@
        J.obj2
          (J.dft "default" status_enc empty_assignments.default)
          (J.dft "token_map" (J.assoc status_enc) [])
      in
      enc_check_version_2 @@
      J.conv
        (fun t -> t.id, t.tags, t.assignments)
        (fun (id, tags, assignments) ->
           {id; tags; assignments})
      @@
      J.obj3
        (J.req "id" J.string)
        (J.dft "tags" (J.list J.string) [])
        (J.dft "assignments" assignments_enc empty_assignments)



(*
    let enc =
      let assignments_enc =
        let token_map_enc =
          J.conv
            (fun m ->
              Token.Map.bindings m |> List.map (fun (tok, a) ->
                                          Token.to_string tok,
                                          (a.start, a.stop)))
            (List.fold_left (fun acc (tok, (start, stop)) ->
                 Token.Map.add (Token.parse tok) {start; stop} acc)
               Token.Map.empty)
            (J.assoc
               (J.obj2
                  (J.req "start" J.float)
                  (J.req "stop" J.float)))
        in
        let assignment_enc =
          J.conv
            (fun a -> (a.start, a.stop))
            (fun (start, stop) -> { start; stop })
            (J.obj2
               (J.req "start" J.float)
               (J.req "stop" J.float))
        in
        J.conv
          (fun a -> (a.token_map, a.default_assignment))
          (fun (t, d) -> { token_map = t; default_assignment = d})
          (J.obj2
             (J.req "token_map" token_map_enc)
             (J.req "default_assignment" assignment_enc))
      in
      let status_enc =
        J.union [
          J.case (J.constant "open")
            (function Open -> Some () | _ -> None) (fun () -> Open);
          J.case (J.constant "closed")
            (function Closed -> Some () | _ -> None) (fun () -> Closed);
          J.case (J.obj1 (J.req "assigned" assignments_enc))
            (function Assigned a -> Some a | _ -> None) (fun a -> Assigned a);
        ]
      in
      J.conv
        (fun t -> t.id, t.tags, t.status)
        (fun (id, tags, status) ->
           {id; tags; status})
      @@
      J.obj3
        (J.req "id" J.string)
        (J.dft "tags" (J.list J.string) [])
        (J.dft "status" status_enc Open)
*)
    let default id = {
      id;
      tags = [];
      assignments = {
        token_map = Token.Map.empty;
        default = Open;
      }
    }

  end

  module Index = struct

    type t =
      | Exercises of (id * Meta.t option) list
      | Groups of (string * group) list
    and group =
      { title : string;
        contents : t }

    let enc =
      let exercise_enc =
        J.union [
          J.case J.string
            (function id, None -> Some id | _ -> None)
            (fun id -> id, None);
          J.case J.(tup2 string Meta.enc)
            (function id, Some meta -> Some (id, meta) | _ -> None)
            (fun (id, meta) -> id, Some meta);
        ]
      in
      let group_enc =
        J.mu "group" @@ fun group_enc ->
        J.conv
          (fun (g : group) -> g.title, g.contents)
          (fun (title, contents) -> { title; contents }) @@
        J.union
          [ J.case
              J.(obj2
                   (req "title" string)
                   (req "exercises" (list exercise_enc)))
              (function
                | (title, Exercises map) -> Some (title, map)
                | _ -> None)
              (fun (title, map) -> (title, Exercises map)) ;
            J.case
              J.(obj2
                   (req "title" string)
                   (req "groups" (assoc group_enc)))
              (function
                | (title, Groups map) -> Some (title, map)
                | _ -> None)
              (fun (title, map) -> (title, Groups map)) ]
      in
      enc_check_version_2 @@
      J.union
        [ J.case
            J.(obj1 (req "exercises" (list exercise_enc)))
            (function
              | Exercises map -> Some map
              | _ -> None)
            (fun map -> Exercises map) ;
          J.case
            J.(obj1 (req "groups" (assoc group_enc)))
            (function
              | Groups map -> Some map
              | _ -> None)
            (fun map -> Groups map) ]

    let find t id =
      let rec aux t = match t with
        | Groups ((_, g)::r) ->
            (try aux g.contents with Not_found -> aux (Groups r))
        | Groups [] -> raise Not_found
        | Exercises l -> (match List.assoc id l with
            | None -> raise Not_found
            | Some e -> e)
      in
      aux t

    let find_opt t id = try Some (find t id) with Not_found -> None

    let rec fold_exercises f acc = function
      | Groups gs ->
          List.fold_left
            (fun acc (_, (g: group)) -> fold_exercises f acc g.contents)
            acc gs
      | Exercises l ->
          List.fold_left (fun acc ->function
              | (id, Some ex) -> f acc id ex
              | _ -> acc)
            acc l

    let rec filterk f g k =
      match g with
      | Groups gs ->
          let rec aux acc = function
            | (id, (g: group)) :: r ->
                (filterk f g.contents @@ function
                  | Exercises [] -> aux acc r
                  | contents -> aux ((id, { g with contents }) :: acc) r)
            | [] -> match acc with
              | [] -> k (Exercises [])
              | l -> k (Groups (List.rev l))
          in
          aux [] gs
      | Exercises l ->
          let rec aux acc = function
            | (id, Some ex) :: r ->
                (f id ex @@ function
                  | true -> aux ((id, Some ex) :: acc) r
                  | false -> aux acc r)
            | (_, None) :: r -> aux acc r
            | [] -> k (Exercises (List.rev acc))
          in
          aux [] l

    let filter f g = filterk (fun x y k -> f x y |> k) g (fun x -> x)

    (* let rec filter f = function
     *   | Groups (gs) ->
     *       List.fold_left (fun acc (id, (g: group)) ->
     *           match filter f g.contents with
     *           | Exercises [] -> acc
     *           | contents -> (id, { g with contents}) :: acc)
     *         [] (List.rev gs)
     *       |> (function [] -> Exercises [] | l -> Groups l)
     *   | Exercises l ->
     *       List.fold_left (fun acc (id, ex) ->
     *           match ex with
     *           | Some ex when f id ex -> (id, Some ex) :: acc
     *           | _ -> acc)
     *         [] (List.rev l)
     *       |> (function l -> Exercises l) *)

  end

end

module Lesson = struct

  type id = string

  type phrase =
    | Text of string
    | Code of string

  type step = {
    step_title: string;
    step_phrases: phrase list;
  }

  type t = {
    title: string;
    steps: step list;
  }

  let enc =
    enc_check_version_2 @@
    J.conv
      (fun t -> (t.title, t.steps))
      (fun (title, steps) -> { title; steps }) @@
    J.obj2
      J.(req "title" string)
      J.(req "steps"
           (list @@
            conv
              (fun s -> (s.step_title, s.step_phrases))
              (fun (step_title, step_phrases) -> {step_title; step_phrases}) @@
            (obj2
               (req "title" string)
               (req "contents"
                  (list @@ union
                     [ case
                         (obj1 (req "html" string))
                         (function Text text -> Some text | Code _ -> None)
                         (fun text -> Text text) ;
                       case
                         (obj1 (req "code" string))
                         (function Code code -> Some code | Text _ -> None)
                         (fun code -> Code code) ])))))

  module Index = struct

    type t = (id * string) list

    let enc =
      enc_check_version_2 @@
      J.(obj1 (req "lessons" (list @@ tup2 string string)))

  end

end

module Tutorial = struct

  type id = string

  type code = {
    code: string;
    runnable: bool;
  }

  type word =
    | Text of string
    | Code of code
    | Emph of text
    | Image of { alt : string ; mime : string ; contents : bytes }
    | Math of string

  and text =
    word list

  type phrase =
    | Paragraph of text
    | Enum of phrase list list
    | Code_block of code

  type step = {
    step_title: text;
    step_contents: phrase list;
  }

  type t = {
    title: text;
    steps: step list;
  }

  let text_enc =
    J.mu "text" @@ fun content_enc ->
    let word_enc =
      J.union
        [ J.case J.string
            (function Text text -> Some text | _ -> None)
            (fun text -> Text text) ;
          J.case
            J.(obj1 (req "text" string))
            (function Text text -> Some text | _ -> None)
            (fun text -> Text text) ;
          J.case
            J.(obj1 (req "emph" content_enc))
            (function Emph content -> Some content | _ -> None)
            (fun content -> Emph content) ;
          J.case
            J.(obj2 (req "code" string) (dft "runnable" bool false))
            (function Code { code ; runnable } -> Some (code, runnable)
                    | _ -> None)
            (fun (code, runnable) -> Code { code ; runnable }) ;
          J.case
            J.(obj1 (req "math" string))
            (function Math math-> Some math | _ -> None)
            (fun math -> Math math) ;
          J.case
            J.(obj3 (req "image" bytes) (req "alt" string) (req "mime" string))
            (function
              | Image { alt ; mime ; contents = image } ->
                  Some (image, alt, mime)
              | _ -> None)
            (fun (image, alt, mime) ->
               Image { alt ; mime ; contents = image }) ] in
    J.union
    [ J.case
        word_enc
        (function [ ctns ] -> Some ctns | _ -> None) (fun ctns -> [ ctns ]) ;
      J.case
        (J.list @@ word_enc)
        (fun ctns -> Some ctns) (fun ctns -> ctns) ]

  let phrase_enc =
    J.mu "phrase" @@ fun phrase_enc ->
    J.union
      [ J.case
          J.(obj1 (req "paragraph" text_enc))
          (function Paragraph phrase -> Some phrase | _ -> None)
          (fun phrase -> Paragraph phrase) ;
        J.case
          J.(obj1 (req "enum" (list (list phrase_enc))))
          (function Enum items -> Some items | _ -> None)
          (fun items -> Enum items) ;
        J.case
          J.(obj2 (req "code" string) (dft "runnable" bool false))
          (function Code_block { code ; runnable } ->
             Some (code, runnable) | _ -> None)
          (fun (code, runnable) ->
             Code_block { code ; runnable }) ;
        J.case
          text_enc
          (function Paragraph phrase -> Some phrase | _ -> None)
          (fun phrase -> Paragraph phrase) ]

  let enc =
    enc_check_version_2 @@
    J.conv
      (fun t -> t.title, t.steps)
      (fun (title, steps) -> {title; steps}) @@
    J.obj2
      (J.req "title" text_enc)
      (J.req "steps"
         (J.list @@
          J.conv
            (fun t -> t.step_title, t.step_contents)
            (fun (step_title, step_contents) -> {step_title; step_contents}) @@
          J.(obj2
               (req "title" text_enc)
               (req "contents" (list phrase_enc)))))

  module Index = struct

    type entry = {
      name: string;
      title: text;
    }

    type series = {
      series_title: string;
      series_tutorials: entry list;
    }

    type t = (id * series) list

    let enc =
      let entry_enc =
        J.union [
          J.case
            J.(tup2 string text_enc)
            (function ({title = []; _}: entry) -> None
                    | {name; title} -> Some (name, title))
            (fun (name, title) -> {name; title});
          J.case
            J.string
            (function {name; title = []} -> Some name
                    | _ -> None)
            (fun name -> {name; title = []});
        ]
      in
      let series_enc =
        J.conv
          (fun t ->
             (t.series_title, t.series_tutorials))
          (fun (series_title, series_tutorials) ->
             {series_title; series_tutorials}) @@
        J.obj2
          J.(req "title" string)
          J.(req "tutorials" (list entry_enc)) in
      enc_check_version_1 @@
      J.(obj1 (req "series" (assoc series_enc)))

  end

end
