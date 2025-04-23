(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data

let version = Learnocaml_version.v

module type COMPAT = sig
  (** List-based versions endowed with a lexicographic order. *)
  type t

  val to_string : t -> string

  (** Supported formats: [Compat.v "str"] where "str" is nonempty and
      either "n", "-n" (a signed integer), or "n.str".
      However, [Compat.v "0.14.rc1"] or so is not supported for now. *)
  val v : string -> t

  (** Note that trailing zeros are ignored, i.e. (v "1") and (v "1.0")
      are equal versions. But (v "1") is higher than (v "1.-1"), itself
      higher than (v "1.-2"), and so on. *)
  val le : t -> t -> bool

  val eq : t -> t -> bool

  val lt : t -> t -> bool

  type pred =
    | Since of t | Upto of t | And of pred * pred

  val compat : pred -> t -> bool
end

module Compat: COMPAT = struct

  (** List-based versions endowed with a lexicographic order. *)
  type t = int list

  let to_string = function
    | [] -> failwith "Compat.to_string"
    | n :: l ->
       List.fold_left (fun r e -> r ^ "." ^ string_of_int e) (string_of_int n) l

  (** Supported formats: [Compat.v "str"] where "str" is nonempty and
      either "n", "-n" (a signed integer), or "n.str".
      However, [Compat.v "0.14.rc1"] or so is not supported for now. *)
  let v = function
    | "" -> failwith "Compat.of_string"
    | s -> String.split_on_char '.' s |> List.map int_of_string

  (** Note that trailing zeros are ignored, i.e. (v "1") and (v "1.0")
      are equal versions. But (v "1") is higher than (v "1.-1"), itself
      higher than (v "1.-2"), and so on. *)
  let rec le v1 v2 = match v1, v2 with
    | [], [] -> true
    | [], 0 :: l2 ->  le [] l2
    | [], n2 :: _ -> 0 < n2
    | 0 :: l1, [] -> le l1 []
    | n1 :: _, [] -> n1 < 0
    | n1 :: l1, n2 :: l2 -> n1 < n2 || (n1 = n2 && le l1 l2)

  let eq v1 v2 = le v1 v2 && le v2 v1

  let lt v1 v2 = not (le v2 v1)

  type pred =
    | Since of t (** >= v0 *)
    | Upto of t  (** < v1 *)
    | And of pred * pred

  let rec compat pred v =
    match pred with
    | Since v0 -> le v0 v
    | Upto v1 -> lt v v1
    | And (pred1, pred2) -> compat pred1 v && compat pred2 v

end

(* Tests
assert Compat.(le (v "0.12") (v "0.13.0"));;
assert Compat.(le (v "0.13.0") (v "0.13.1"));;
assert Compat.(le (v "0.13.1") (v "0.14.0"));;
assert Compat.(le (v "0.14.0") (v "1.0.0"));;
assert Compat.(le (v "1.1.1") (v "1.1.1"));;
assert Compat.(le (v "0.2") (v "0.10"));;
assert Compat.(le (v "1.9.5") (v "1.10.0"));;
 *)

type _ request =
  | Static:
      string list -> string request
  | Version:
      unit -> (string * int) request
  | Nonce:
      unit -> string request
  | Create_token:
      string * student token option * string option -> student token request
  | Create_teacher_token:
      teacher token * string option -> teacher token request
  | Login: 
      'a token -> Session.t request
  | Fetch_save:
      'a session -> Save.t request
  | Get_token:
      'a session -> Token.t request
  | Archive_zip:
      'a session -> string request
  | Update_save:
      'a session * Save.t -> Save.t request
  | Git: 'a token * string list -> string request

  | Students_list:
      'a session -> Student.t list request
  | Set_students_list:
      'a session * (Student.t * Student.t) list -> unit request
  | Students_csv:
      'a session * Exercise.id list * Token.t list -> string request

  | Exercise_index:
      'a session option -> (Exercise.Index.t * (Exercise.id * float) list) request
  | Exercise:
      'a session option * string * bool ->
      (Exercise.Meta.t * Exercise.t * float option) request

  | Lesson_index:
      unit -> (string * string) list request
  | Lesson:
      string -> Lesson.t request

  | Tutorial_index:
      unit -> Tutorial.Index.t request
  | Tutorial:
      string -> Tutorial.t request

  | Playground_index:
      unit -> Playground.Index.t request
  | Playground:
      string -> Playground.t request

  | Exercise_status_index:
      'a session -> Exercise.Status.t list request
  | Exercise_status:
      'a session * Exercise.id -> Exercise.Status.t request
  | Set_exercise_status:
      'a session * (Exercise.Status.t * Exercise.Status.t) list -> unit request

  | Partition:
      'a session * Exercise.id * string * int -> Partition.t request

  | Invalid_request:
      string -> string request

let supported_versions
  : type resp. resp request -> Compat.pred
  = function
  | Static _
  | Version _
  | Nonce _
  | Create_token (_, _, _)
  | Create_teacher_token _
  | Login _
  | Fetch_save _
  | Get_token _
  | Archive_zip _
  | Update_save (_, _)
  | Git (_, _)
  | Students_list _
  | Set_students_list (_, _)
  | Students_csv (_, _, _)
  | Exercise_index _
  | Exercise (_, _, _)
  | Lesson_index _
  | Lesson _
  | Tutorial_index _
  | Tutorial _
  | Playground_index _
  | Playground _
  | Exercise_status_index _
  | Exercise_status (_, _)
  | Set_exercise_status (_, _)
  | Partition (_, _, _, _)
  | Invalid_request _ -> Compat.(Since (v "0.12"))

let is_supported
  : type resp. ?current:Compat.t -> server:Compat.t -> resp request ->
         (unit, string) result =
  fun ?(current = Compat.v Learnocaml_version.v) ~server request ->
  let supp = supported_versions request in
  if Compat.(compat (Since server) current) (* server <= current *)
     && Compat.compat supp current (* request supported by current codebase *)
     && Compat.compat supp server (* request supported by server *)
  then Ok () else
    Error (Printf.sprintf
             {|API request not supported by server v.%s using client v.%s|}
             (* NOTE: we may want to add some string_of_request call as well *)
             (Compat.to_string server) (Compat.to_string current))

type http_request = {
  meth: [ `GET | `POST of string];
  path: string list;
  args: (string * string) list;
}

module J = Json_encoding

module type JSON_CODEC = sig
  val decode: 'a J.encoding -> string -> 'a
  val encode: ?minify:bool -> 'a J.encoding -> 'a -> string
end

module Conversions (Json: JSON_CODEC) = struct

  let response_codec
    : type resp.
      resp request -> (resp -> string) * (string -> resp)
    = fun req ->
      let str = (fun x -> x), (fun x -> x) in
      let json enc = (Json.encode enc), (Json.decode enc) in
      let ( +> ) (cod, decod) (cod', decod') =
        (fun x -> cod (cod' x)),
        (fun s -> decod' (decod s))
      in
      match req with
      | Static _ -> str
      | Version _ -> json J.(obj2 (req "version" string) (req "server_id" int))
      | Nonce _ -> json J.(obj1 (req "nonce" string))
      | Create_token _ ->
          json J.(obj1 (req "token" string)) +>
          Token.(to_string, parse)
      | Create_teacher_token _ ->
          json J.(obj1 (req "token" string)) +>
          Token.(to_string, parse)
      | Login _ ->
          json J.(obj1 (req "session" string)) 
      | Fetch_save _ ->
          json Save.enc
      | Get_token _ ->
          json J.(obj1 (req "token" string)) +>
          Token.(to_string, parse)
      | Archive_zip _ ->
          str
      | Update_save _ ->
          json Save.enc
      | Git _ -> str
      | Students_list _ ->
          json (J.list Student.enc)
      | Set_students_list _ ->
          json J.unit
      | Students_csv _ ->
          str
      | Exercise_index _ ->
         json (J.tup2 Exercise.Index.enc (J.assoc J.float))
      | Exercise _ ->
          json (J.tup3 Exercise.Meta.enc Exercise.enc (J.option J.float))
      | Lesson_index _ ->
          json Lesson.Index.enc
      | Lesson _ ->
          json Lesson.enc
      | Tutorial_index _ ->
          json Tutorial.Index.enc
      | Tutorial _ ->
         json Tutorial.enc
      | Playground_index _ ->
          json Playground.Index.enc
      | Playground _ ->
          json Playground.enc

      | Exercise_status_index _ ->
          json (J.list Exercise.Status.enc)
      | Exercise_status _ ->
          json Exercise.Status.enc
      | Set_exercise_status _ ->
          json J.unit

      | Partition _ ->
          json Partition.enc

      | Invalid_request _ ->
          str

  let response_encode r = fst (response_codec r)
  let response_decode r = snd (response_codec r)


  let to_http_request
    : type resp. resp request -> http_request
    =
    let get ?token ?session ?(args=[]) path = {
      meth = `GET;
      path;
      args = (match token with None -> [] | Some t -> ["token", Token.to_string t]) @
      (match session with None -> [] | Some s -> ["session", s]) @ args;
    } in
    let post ?token ?session path body = {
      meth = `POST body;
      path;
      args = (match token with None -> [] | Some t -> ["token", Token.to_string t]) @
      (match session with None -> [] | Some s -> ["session", s]);
    } in
    function
    | Static path ->
        get path
    | Version () ->
        get ["version"]

    | Nonce () ->
        get ["nonce"]
    | Create_token (secret_candiate, token, nick) ->
        get ?token (["sync"; "new"; secret_candiate] @
                    (match nick with None -> [] | Some n -> [n]))
    | Create_teacher_token (token, nick) ->
        assert (Token.is_teacher token);
        get ~token (["teacher"; "new"] @
                    (match nick with None -> [] | Some n -> [n]))
    | Login token ->
        get ~token ["login"]
    | Fetch_save session ->
        get ~session ["save.json"]
    | Get_token session ->
        get ~session ["token"]
    | Archive_zip session ->
        get ~session ["archive.zip"]
    | Update_save (session, save) ->
        post ~session ["sync"] (Json.encode Save.enc save)
    | Git _ ->
        assert false (* Reserved for the [git] client *)

    | Students_list session ->
        get ~session ["teacher"; "students.json"]
    | Set_students_list (session, students) ->
        post ~session
          ["teacher"; "students.json"]
          (Json.encode (J.list (J.tup2 Student.enc Student.enc)) students)
    | Students_csv (session, exercises, students) ->
        post ~session ["teacher"; "students.csv"]
          (Json.encode
             (J.obj2
                (J.dft "exercises" (J.list J.string) [])
                (J.dft "students" (J.list Token.enc) []))
             (exercises, students))

    | Exercise_index (Some session) ->
       get ~session ["exercise-index.json"]
    | Exercise_index None ->
       get ["exercise-index.json"]

    | Exercise (Some session, id, js) ->
       get ~session
         ("exercises" :: String.split_on_char '/' (id^".json"))
         ~args:["mode", if js then "js" else "byte"]
    | Exercise (None, id, js) ->
       get ("exercises" :: String.split_on_char '/' (id^".json"))
         ~args:["mode", if js then "js" else "byte"]

    | Lesson_index () ->
        get ["lessons.json"]
    | Lesson id ->
       get ["lessons"; id^".json"]

    | Playground_index () ->
        get ["playgrounds.json"]
    | Playground id ->
        get ["playgrounds"; id^".json"]

    | Tutorial_index () ->
        get ["tutorials.json"]
    | Tutorial id ->
        get ["tutorials"; id^".json"]

    | Exercise_status_index session ->
        get ~session ["teacher"; "exercise-status.json"]
    | Exercise_status (session, id) ->
        get ~session
          ("teacher" :: "exercise-status" :: String.split_on_char '/' id)
    | Set_exercise_status (session, status) ->
        post ~session
          ["teacher"; "exercise-status"]
          (Json.encode
             (J.list (J.tup2 Exercise.Status.enc Exercise.Status.enc))
             status)

    | Partition (session, eid, fid, prof) ->
        get ~session
          ["partition"; eid; fid; string_of_int prof]

    | Invalid_request s ->
        failwith ("Error request "^s)

end

module type REQUEST_HANDLER = sig
  type 'resp ret
  val map_ret: ('a -> 'b) -> 'a ret -> 'b ret

  val callback: Conduit.endp ->
                Learnocaml_data.Server.config -> 'resp request -> 'resp ret
end

module Server (Json: JSON_CODEC) (Rh: REQUEST_HANDLER) = struct

  module C = Conversions(Json)

  let rec last =
    function [f] -> Some f | [] -> None | _::r -> last r

  let handler conn config request =
      let k req =
        Rh.callback conn config req |> Rh.map_ret (C.response_encode req)
      in
      let token =
        match List.assoc_opt "token" request.args with
        | None -> None
        | Some stoken ->
            try Some (Token.parse stoken)
            with Failure _ -> None
      in
      let session =
        match List.assoc_opt "session" request.args with
        | None -> None
        | Some session -> Some session
      in
      match request.meth, request.path, token, session with
      | `GET, ([] | [""]), _,_ ->
          Static ["index.html"] |> k
      | `GET, ["version"], _, _ ->
          Version () |> k

      | `GET, ["nonce"], _, _ ->
          Nonce () |> k
      | `GET, ["sync"; "new"; secret_candidate], token, _ ->
          Create_token (secret_candidate, token, None) |> k
      | `GET, ["sync"; "new"; secret_candidate; nick], token, _ ->
          Create_token (secret_candidate, token, Some nick) |> k
      | `GET, ["teacher"; "new"], Some token, _ when Token.is_teacher token ->
          Create_teacher_token (token, None) |> k
      | `GET, ["teacher"; "new"; nick], Some token, _ when Token.is_teacher token ->
          Create_teacher_token (token, Some nick) |> k
      | `GET, ["login"], Some token, _ ->
        Login token |> k
      | `GET, ["save.json"], _, Some session ->
          Fetch_save session |> k
      | `GET, ["token"], _, Some session ->
          Get_token session |> k
      | `GET, ["archive.zip"], _, Some session ->
          Archive_zip session |> k
      | `POST body, ["sync"], _, Some session ->
          (match Json.decode Save.enc body with
           | save -> Update_save (session, save) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)
      | `GET, (stoken::"learnocaml-workspace.git"::p), None, _ ->
          (match Token.parse stoken with
           | token -> Git (token, p) |> k
           | exception Failure e -> Invalid_request e |> k)

      | `GET, ["teacher"; "students.json"], _, Some session ->
          Students_list session |> k
      | `POST body, ["teacher"; "students.json"], _, Some session ->
          (match Json.decode (J.list (J.tup2 Student.enc Student.enc)) body with
           | students -> Set_students_list (session, students) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)
      | `GET, ["teacher"; "students.csv"], _, Some session ->
          Students_csv (session, [], []) |> k
      | `POST body, ["teacher"; "students.csv"], _, Some session ->
          (match Json.decode
                   (J.obj2
                      (J.dft "exercises" (J.list J.string) [])
                      (J.dft "students" (J.list Token.enc) []))
                   body
           with
           | exercises, students ->
               Students_csv (session, exercises, students) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)

      | `GET, ["exercise-index.json"], _, session ->
         Exercise_index session |> k
      | `GET, ("exercises"::path), _, session ->
          (match last path with
           | Some s when String.lowercase_ascii (Filename.extension s) = ".json" ->
               (match session with
                | Some session ->
                    let id = Filename.chop_suffix (String.concat "/" path) ".json" in
                    let js = List.assoc_opt "mode" request.args = Some "js" in
                    Exercise (Some session, id, js) |> k
                | None -> Invalid_request "Missing session" |> k)
           | Some "" ->
               Static ["exercise.html"] |> k
           | _ ->
              Static ("static"::path) |> k)
      | `GET, ("description"::_), _token, _ ->
         (* match token with
          | None -> Invalid_request "Missing token" |> k *)
          Static ["description.html"] |> k
      | `GET, ("playground"::path), _token, _ ->
         begin
           match last path with
           | Some s when String.lowercase_ascii (Filename.extension s) = ".json" ->
              let id = Filename.chop_suffix (String.concat "/" path) ".json" in
              Playground id |> k
           | Some "" ->
              Static ["playground.html"] |> k
           | _ ->
              Static ("static"::path) |> k
         end
      | `GET, ["lessons.json"], _, _ ->
          Lesson_index () |> k
      | `GET, ["lessons"; f], _, _ when Filename.check_suffix f ".json" ->
          Lesson (Filename.chop_suffix f ".json") |> k

      | `GET, ["tutorials.json"], _, _ ->
          Tutorial_index () |> k
      | `GET, ["tutorials"; f], _, _ when Filename.check_suffix f ".json" ->
         Tutorial (Filename.chop_suffix f ".json") |> k

      | `GET, ["playgrounds.json"], _, _ ->
          Playground_index () |> k
      | `GET, ["playgrounds"; f], _, _ when Filename.check_suffix f ".json" ->
          Playground (Filename.chop_suffix f ".json") |> k

      | `GET, ["partition"; eid; fid; prof], _, Some session ->
          Partition (session, eid, fid, int_of_string prof) |> k

      | `GET, ["teacher"; "exercise-status.json"], _, Some session ->
          Exercise_status_index session |> k
      | `GET, ("teacher" :: "exercise-status" :: id), _, Some session ->
          Exercise_status (session, String.concat "/" id) |> k
      | `POST body, ["teacher"; "exercise-status"], _, Some session ->
          (match Json.decode
                   (J.list (J.tup2 Exercise.Status.enc Exercise.Status.enc))
                   body
           with
           | status ->
               Set_exercise_status (session, status) |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)

      | `GET,
        ( ["index.html"]
          | ["exercise.html"]
        | ["playground.html"]
        | ["student-view.html"]
        | ["description.html"]
        | ["partition-view.html"]
        | ("js"|"fonts"|"icons"|"css"|"static") :: _ as path),
        _, _ ->
          Static path |> k

      | `GET, ["favicon.ico"], _, _ ->
          Static ["icons"; "favicon.ico"] |> k

      | meth, path, _, _ ->
          Invalid_request
            (Printf.sprintf "%s /%s%s"
               (match meth with `GET -> "GET" | `POST _ -> "POST")
               (String.concat "/" path)
               (match request.args with [] -> "" | l ->
                   "?" ^ String.concat "&"
                     (List.map (fun (k, v) -> k ^"="^ v) l)))
          |> k

end

module Client (Json: JSON_CODEC) = struct

  open Lwt.Infix

  module C = Conversions(Json)

  let make_request
    : type resp.
      (http_request -> (string, 'b) result Lwt.t) ->
      resp request -> (resp, 'b) result Lwt.t
    = fun send req ->
      let http_request = C.to_http_request req in
      send http_request >|= function
      | Ok str -> Ok (C.response_decode req str)
      | Error e -> Error e

end

(*
let client: type resp. resp request -> resp result = fun req ->

  let query_enc =
 function
  | Static str as req -> Server_caller.fetch (path req) |> query
*)

(* let server: meth * string list * string -> _ request = function
 *   | `GET, [] -> Static "index.json"
 *   | `GET, ["sync"; "gimme"] -> Create_token ()
 *   | `GET, ["sync"; token] -> Fetch_save token
 *   | `POST, ["sync"; token] -> *)
