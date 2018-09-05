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

module Exercise = struct

  type id = Exercise.id

  let index =
    ref (lazy (
        read_static_file Learnocaml_index.exercise_index_path Exercise.Index.enc
      ))

  module Meta = struct
    include Exercise.Meta

    let get id =
      Lazy.force !index >|= fun index -> Exercise.Index.find index id
  end

  module Index = struct
    include Exercise.Index


    let get () = Lazy.force !index

    let reload () =
      read_static_file Learnocaml_index.exercise_index_path Exercise.Index.enc
      >|= fun i -> index := lazy (Lwt.return i)

  end

  module Status = struct

    include Exercise.Status

    let store_file () = Filename.concat !sync_dir "exercises.json"

    let tbl = lazy (
      let tbl = Hashtbl.create 223 in
      Lwt.catch (fun () ->
          Lwt_io.(with_file ~mode:Input (store_file ()) read) >|=
          Json_codec.decode (J.list enc) >|= fun l ->
          List.iter (fun st -> Hashtbl.add tbl st.id st) l;
          tbl)
      @@ function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return tbl
      | e -> raise e
    )

    let save () =
      Lazy.force tbl >>= fun tbl ->
      let l = Hashtbl.fold (fun _ s acc -> s::acc) tbl [] in
      let s = Json_codec.encode (J.list enc) l in
      Lwt_io.(with_file ~mode:Output (store_file ())
                (fun oc -> write oc s))

    let get id =
      Lazy.force tbl >>= fun tbl ->
      try Lwt.return (Hashtbl.find tbl id)
      with Not_found -> Meta.get id >|= fun _ -> default id

    let set =
      let mutex = Lwt_mutex.create () in
      fun x ->
        Lwt_mutex.with_lock mutex @@ fun () ->
        Lazy.force tbl >>= fun tbl ->
        Hashtbl.replace tbl x.id x;
        save ()

    let all () =
      Lazy.force tbl >|= fun tbl ->
      Hashtbl.fold (fun _ t acc -> t::acc) tbl []

    let is_open id token =
      if Token.is_teacher token then Lwt.return `Open else
      Lazy.force tbl >|= fun tbl ->
      let assignments =
        match Hashtbl.find_opt tbl id with
        | None -> (default id).assignments
        | Some ex -> ex.assignments
      in
      is_open_assignment token assignments

  end

  include (Exercise: module type of struct include Exercise end
           with type id := id
            and module Meta := Meta
            and module Status := Status
            and module Index := Index)

  let get id =
    Lwt.catch
      (fun () -> read_static_file (Learnocaml_index.exercise_path id) enc)
      (function
        | Unix.Unix_error _ -> raise Not_found
        | e -> raise e)

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

  let create_student () =
    create_gen random

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
