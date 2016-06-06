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

type 'a storage_key =
  { store : 'a -> unit ;
    retrieve : unit -> 'a ;
    delete : unit -> unit }

let store { store } v = store v

let retrieve { retrieve } = retrieve ()

let delete { delete } = delete ()

let store_single path enc =
  let mangled = String.concat ":" ("learnocaml" :: path) in
  fun value ->
    Js.Optdef.case
      (Dom_html.window##localStorage)
      (fun () -> failwith "local storage support required")
      (fun localStorage ->
         let json = Browser_json.Json_encoding.construct enc value in
         localStorage##setItem (Js.string mangled, Js._JSON##stringify (json)))

let retrieve_single ?default path enc =
  let mangled = String.concat ":" ("learnocaml" :: path) in
  fun () ->
    Js.Optdef.case
      (Dom_html.window##localStorage)
      (fun () -> failwith "local storage support required")
      (fun localStorage ->
         Js.Opt.case
           (localStorage##getItem (Js.string mangled))
           (fun () ->
              match default with
              | Some default -> default
              | None -> raise Not_found)
           (fun v ->
              let open Browser_json.Json_encoding in
              try
                destruct enc (Js._JSON##parse (v))
              with exn ->
                raise (Json_encoding.Cannot_destruct
                         ([ `Field "localStorage" ; `Field mangled ], exn))))

let delete_single path enc =
  let mangled = String.concat ":" ("learnocaml" :: path) in
  fun () ->
    Js.Optdef.case
      (Dom_html.window##localStorage)
      (fun () -> failwith "local storage support required")
      (fun localStorage ->
         localStorage##removeItem (Js.string mangled))

let cached_exercise name =
  let path = [ "cached-exercise" ; name ] in
  let enc = Exercise.enc in
  let store value = store_single path enc value
  and retrieve () = retrieve_single path enc ()
  and delete () = delete_single path enc () in
  { store ; retrieve ; delete }

let exercise_toplevel_history name =
  let path = [ "exercise-toplevel-history" ; name ] in
  let enc = Json_encoding.(list string) in
  let store value = store_single path enc value
  and retrieve () = retrieve_single ~default: [] path enc ()
  and delete () = delete_single path enc () in
  { store ; retrieve ; delete }

let toplevel_history =
  let path = [ "toplevel-history" ] in
  let enc = Json_encoding.(list string) in
  let store value = store_single path enc value
  and retrieve () = retrieve_single ~default: [] path enc ()
  and delete () = delete_single path enc () in
  { store ; retrieve ; delete }

let exercise_list =
  let path = [ "exercise-list" ] in
  let enc = Json_encoding.(list string) in
  let store value = store_single path enc value
  and retrieve () = retrieve_single ~default: [] path enc ()
  and delete () = delete_single path enc () in
  { store ; retrieve ; delete }

let exercise_state name =
  let path = [ "exercise-state" ; name ] in
  let enc = Client_index.exercise_state_enc in
  let store value =
    store_single path enc value ;
    let all = retrieve exercise_list in
    if not (List.mem name all) then
      store exercise_list (name :: all)
  and retrieve () =
    retrieve_single path enc ()
  and delete () =
    delete_single path enc () ;
    let all = retrieve exercise_list in
    if List.mem name all then
      store exercise_list (List.filter ((<>) name) all)  in
  { store ; retrieve ; delete }

module StringMap = Map.Make (String)

let client_index =
  let retrieve () =
    let all = retrieve exercise_list in
    List.fold_left
      (fun acc name ->
         StringMap.add name (retrieve (exercise_state name)) acc)
      StringMap.empty all
  and delete () =
    let all = retrieve exercise_list in
    List.iter (fun name -> delete (exercise_state name)) all ;
    delete exercise_list in
  let store index =
    delete () ;
    let all =
      StringMap.fold
        (fun name state acc ->
           store (exercise_state name) state ;
           name :: acc)
        index [] in
    store exercise_list all in
  { store ; retrieve ; delete }
