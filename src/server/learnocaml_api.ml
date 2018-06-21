(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
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

type 'a token = Learnocaml_sync.Token.t

type student
type teacher

type _ request =
  | Static: string list -> string request
  | Version: unit -> string request
  | Create_token: unit -> student token request
  | Create_teacher_token: string -> teacher token request
  | Fetch_save: student token -> Learnocaml_sync.save_file option request
  | Update_save:
      student token * Learnocaml_sync.save_file ->
      Learnocaml_sync.save_file request
  | Exercise_index: 'a token -> Learnocaml_index.group_contents request
  (** to help transition: do not use *)
  | Static_json: string * 'a Json_encoding.encoding -> 'a request
  | Invalid_request: string -> string request

type http_request = {
  meth: [ `GET | `POST of string];
  path: string list;
}

module type JSON_CODEC = sig
  val decode: 'a Json_encoding.encoding -> string -> 'a
  val encode: 'a Json_encoding.encoding -> 'a -> string
end

module Conversions (Json: JSON_CODEC) = struct

  let response_codec
    : type resp.
      resp request -> (resp -> string) * (string -> resp)
    = fun req ->
      let str = (fun x -> x), (fun x -> x) in
      let json enc =
        (fun x ->
           match Json_encoding.construct enc x with
           | `O _ | `A _ as json -> Ezjsonm.to_string json
           | _ -> assert false),
        (fun s ->
           Ezjsonm.from_string s |>
           Json_encoding.destruct enc)
      in
      let ( +> ) (cod, decod) (cod', decod') =
        (fun x -> cod (cod' x)),
        (fun s -> decod' (decod s))
      in
      match req with
      | Static _ -> str
      | Version _ -> json Json_encoding.(obj1 (req "version" string))
      | Create_token _ ->
          json Json_encoding.(obj1 (req "token" string)) +>
          Learnocaml_sync.Token.(to_string, parse)
      | Create_teacher_token _ ->
          json Json_encoding.(obj1 (req "token" string)) +>
          Learnocaml_sync.Token.(to_string, parse)
      | Fetch_save _ ->
          json (Json_encoding.option Learnocaml_sync.save_file_enc)
      | Update_save _ ->
          json Learnocaml_sync.save_file_enc
      | Exercise_index _ ->
          json Learnocaml_index.exercise_index_enc
      | Static_json (_, enc) ->
          json enc
      | Invalid_request _ ->
          str

  let response_encode r = fst (response_codec r)
  let response_decode r = snd (response_codec r)

  let to_http_request
    : type resp. resp request -> http_request
    = function
      | Static path ->
          { meth = `GET; path }
      | Version () ->
          { meth = `GET; path = ["version"] }
      | Create_token () ->
          { meth = `GET; path = ["sync"; "gimme"] }
      | Create_teacher_token key ->
          { meth = `GET; path = ["teacher"; "gen"; key] }
      | Fetch_save token ->
          let stoken = Learnocaml_sync.Token.to_string token in
          { meth = `GET; path = ["sync"; stoken] }
      | Update_save (token, save) ->
          let stoken = Learnocaml_sync.Token.to_string token in
          let body = Json.encode Learnocaml_sync.save_file_enc save in
          { meth = `POST body; path = ["sync"; stoken] }
      | Exercise_index token ->
          let stoken = Learnocaml_sync.Token.to_string token in
          { meth = `GET; path = ["exercise-index"; stoken] }
      | Static_json (path, _) ->
          { meth = `GET; path = [path] }
      | Invalid_request s ->
          failwith ("Error request "^s)

end

module type REQUEST_HANDLER = sig
  type 'resp ret
  val map_ret: ('a -> 'b) -> 'a ret -> 'b ret

  val callback: 'resp request -> 'resp ret
end

module Server (Json: JSON_CODEC) (Rh: REQUEST_HANDLER) = struct

  module C = Conversions(Json)

  let handler request =
      let k req =
        Rh.callback req |> Rh.map_ret (C.response_encode req)
      in
      match request with
      | { meth = `GET; path = [] } ->
          Static ["index.html"] |> k
      | { meth = `GET; path = ["version"] } ->
          Version () |> k
      | { meth = `GET; path = ["sync"; "gimme"] } ->
          Create_token () |> k
      | { meth = `GET; path = ["teacher"; "gen"; key] } ->
          Create_teacher_token key |> k
      | { meth = `GET; path = ["sync"; token] } ->
          (match Learnocaml_sync.Token.parse token with
           | token -> Fetch_save token |> k
           | exception (Failure s) -> Invalid_request s |> k)
      | { meth = `POST body; path = ["sync"; token] } ->
          (match
             Learnocaml_sync.Token.parse token,
             Json.decode Learnocaml_sync.save_file_enc body
           with
           | token, save -> Update_save (token, save) |> k
           | exception (Failure s) -> Invalid_request s |> k
           | exception e -> Invalid_request (Printexc.to_string e) |> k)
      | { meth = `GET; path = ["exercise-index"; token] } ->
          (match Learnocaml_sync.Token.parse token with
           | token -> Exercise_index token |> k
           | exception (Failure s) -> Invalid_request s |> k)
      | { meth = `GET; path } ->
          (* FIXME: also handles the deprecated Static_json (they are the same,
             server-side). This is dirty *)
          Static path |> k
      | { meth = `POST _; path } ->
          Invalid_request (Printf.sprintf "POST %s" (String.concat "/" path))
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
