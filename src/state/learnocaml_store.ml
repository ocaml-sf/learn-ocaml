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

open Lwt.Infix
open Learnocaml_data

module J = Json_encoding

let static_dir = ref (Filename.concat (Sys.getcwd ()) "www")

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

module Json_codec = struct
  let decode enc s =
    (match s with
     | "" -> `O []
     | s -> Ezjsonm.from_string s)
    |> J.destruct enc

  let encode enc x =
    match J.construct enc x with
    | `A _ | `O _ as json -> Ezjsonm.to_string json
    | `Null -> ""
    | _ -> assert false
end

let read_static_file path enc =
  let path = String.split_on_char '/' path in (* FIXME *)
  let shorten path =
    let rec resolve acc = function
      | [] -> List.rev acc
      | "." :: rest -> resolve acc rest
      | ".." :: rest ->
          begin match acc with
            | [] -> resolve [] rest
            | _ :: acc -> resolve acc rest end
      | name :: rest -> resolve (name :: acc) rest in
    resolve [] path in
  let path =
    String.concat Filename.dir_sep (!static_dir :: shorten path) in
  Lwt_io.(with_file ~mode: Input path read) >|=
  Json_codec.decode enc


module Lesson = struct

  module Index = struct

    include Lesson.Index

    let get () =
      read_static_file Learnocaml_index.lesson_index_path enc

  end

  include (Lesson: module type of struct include Lesson end
           with module Index := Index)

  let get id =
    read_static_file (Learnocaml_index.lesson_path id) enc

end

module Tutorial = struct

  module Index = struct

    include Tutorial.Index

    let get () =
      read_static_file Learnocaml_index.tutorial_index_path enc

  end

  include (Tutorial: module type of struct include Tutorial end
           with module Index := Index)

  let get id =
    read_static_file (Learnocaml_index.tutorial_path id) enc

end

module Token = struct

  include Token

  let path token =
    Filename.concat !sync_dir (Token.to_path token)

  let exists token =
    Lwt_unix.file_exists (path token)

  let check_teacher token =
    if is_teacher token then exists token
    else Lwt.return_false

  let create_gen rnd =
    let rec aux () =
      let token = rnd () in
      let file = path token in
      Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname file) >>= fun () ->
      Lwt.catch (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o700 file
            ~flags:Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_EXCL])
            (fun _chan -> Lwt.return token))
      @@ function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> aux ()
      | e -> raise e
    in
    aux ()

  let register ?(allow_teacher=false) token =
    if not allow_teacher && is_teacher token then
      Lwt.fail (Invalid_argument "Registration of teacher token not allowed")
    else
      Lwt.catch (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o700 (path token)
            ~flags:Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_EXCL])
            (fun _chan -> Lwt.return_unit))
      @@ function
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          Lwt.fail_with "token already exists"
      | e -> raise e

  let create_student () = create_gen random
  let create_teacher () = create_gen random_teacher

  let delete token = Lwt_unix.unlink (path token)
  (* todo: cleanup empty dirs? *)

  module Index = struct

    type nonrec t = t list

    let enc = J.(list enc)

    let get () =
      let base = !sync_dir in
      let ( / ) dir f = if dir = "" then f else Filename.concat dir f in
      let rec scan f d acc =
        let rec aux s acc =
          Lwt.catch (fun () ->
              Lwt_stream.get s >>= function
              | Some ("." | "..") -> aux s acc
              | Some x -> scan f (d / x) acc >>= aux s
              | None -> Lwt.return acc)
          @@ function
          | Unix.Unix_error (Unix.ENOTDIR, _, _) -> f d acc
          | Unix.Unix_error _ -> Lwt.return acc
          | e -> Lwt.fail e
        in
        aux (Lwt_unix.files_of_directory (base / d)) acc
      in
      scan (fun d acc ->
          let stok = String.map (function '/' | '\\' -> '-' | c -> c) d in
          try Lwt.return (Token.parse stok :: acc)
          with Failure _ -> Lwt.return acc
        ) "" []

  end

end

module Save = struct

  include Save

  let get token =
    Lwt.catch (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Input (Token.path token)
          (fun chan -> Lwt_io.read chan >|= Json_codec.decode (J.option enc)))
    @@ function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none
    | e -> Lwt.fail e

  let set token save =
    let flags =
      if Token.is_teacher token then
        Unix.([O_WRONLY; O_NONBLOCK; O_TRUNC])
      else
        Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC])
    in
    let file = Token.path token in
    Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname file) >>= fun () ->
    Lwt.catch (fun () ->
        Lwt_io.(with_file ~flags ~mode:Output file
                  (fun chan -> write chan (Json_codec.encode enc save))))
    @@ function
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Lwt.fail_with "Unregistered teacher token"
    | e -> Lwt.fail e

end

module Exercise_status = struct

  type id = string

  type tag = string

  type status = Open | Closed | Readonly

  type assignment = {
    start: float;
    stop: float;
  }

  type t = {
    id: id;
    path: string list;
    meta: Exercise.Meta.t;
    tags: tag list;
    status: status;
    assigned: assignment Token.Map.t;
  }

  let enc =
    J.conv
      (fun t -> t.id, t.path, t.meta, t.tags, t.status, t.assigned)
      (fun (id, path, meta, tags, status, assigned) ->
         {id; path; meta; tags; status; assigned})
    @@
    J.obj6
      (J.req "id" J.string)
      (J.req "path" (J.list J.string))
      (J.req "meta" Exercise.Meta.enc)
      (J.dft "tags" (J.list J.string) [])
      (J.dft "status" (J.string_enum [
           "open", Open;
           "closed", Closed;
           "readonly", Readonly;
         ]) Open)
      (J.dft "assigned"
         (J.conv
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
                  (J.req "stop" J.float))))
         Token.Map.empty)


  module T = struct
    type nonrec t = t
    let compare a b = compare a.id b.id
  end

  module Map = Map.Make(T)

  let all: (id, t) Hashtbl.t = Hashtbl.create 223

  (* module Index = struct
   * 
   *   type t = Learnocaml_index.group_contents
   * 
   *   let enc = Learnocaml_index.exercise_index_enc
   * 
   *   let load exercise_index_file =
   *     let ic = open_in exercise_index_file in
   *     let json = Ezjsonm.from_channel ic in
   *     close_in ic;
   *     let index = J.destruct enc json in
   *     Lwt.return index
   *     (\* let rec register path = function
   *      *   | Learnocaml_index.Groups groups ->
   *      *       List.iter (fun (group_name, { _group_title; group_contents }) ->
   *      *           register (path @ [group_name]) group_contents)
   *      *         groups
   *      *   | Learnocaml_index.Learnocaml_exercises exos ->
   *      *       List.iter (fun (name, exercise) ->
   *      *           Hashtbl.add all id {
   *      *             id;
   *      *             path;
   *      *             meta = exercise;
   *      *             tags = [];
   *      *           })
   *      *         exos
   *      * in
   *      * register [] index *\)
   * 
   *   let reload () = ()
   * 
   * (\* let select ?(filter=None) ?(sort=None) () =
   *  *   Hashtbl.fold (fun _id ex acc -> ex::acc) all [] *\)
   * 
   * end *)

end

module Student = Learnocaml_data.Student



(* struct
 * 
 *   type tag = string
 * 
 *   type token = Token.t
 * 
 *   type t = {
 *     token: token;
 *     nickname: string;
 *     exercises: Learnocaml_exercise_state.exercise_state SMap.t;
 *     tags: tag list;
 *   }
 * 
 *   module T = struct
 *     type nonrec t = t
 *     let compare a b = compare a.token b.token
 *   end
 * 
 *   module Set = Set.Make(T)
 * 
 *   let all: (token, t) Hashtbl.t = Hashtbl.create 223
 * end *)

(* module Teacher: sig
 * 
 *   type token = Token.t
 * 
 *   type t = {
 *     token: token;
 *     nickname: string;
 *     students: Student.set;
 *   }
 * 
 *   let all: (token, t) Hashtbl.t = Hashtbl.create 71
 * end
 * 
 * let init ~exercise_index =
 *   Exercise.load exercise_index *)
