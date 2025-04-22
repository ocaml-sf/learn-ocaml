(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt.Infix
open Learnocaml_data

module J = Json_encoding

let static_dir = ref (Filename.concat (Sys.getcwd ()) "www")

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

let data_dir = ref (Filename.concat !sync_dir "data")

module Json_codec = struct
  let decode enc s =
    (match s with
     | "" -> `O []
     | s -> Ezjsonm.from_string s)
    |> J.destruct enc

  let encode ?minify enc x =
    match J.construct enc x with
    | `A _ | `O _ as json -> Ezjsonm.to_string ?minify json
    | `Null -> ""
    | _ -> assert false
end
let get_from_file enc p =
  Lwt_io.(with_file ~mode: Input p read) >|=
    Json_codec.decode enc

let write_to_file enc s p =
  let open Lwt_io in
  let s = Json_codec.encode enc s in
  with_file ~mode:output p @@ fun oc -> write oc s

let sanitise_path prefix subpath =
  let rec resolve acc = function
    | [] -> List.rev acc
    | "" :: rest -> resolve acc rest
    | "." :: rest -> resolve acc rest
    | ".." :: rest ->
        begin match acc with
          | [] -> resolve [] rest
          | _ :: acc -> resolve acc rest end
    | name :: rest -> resolve (name :: acc) rest
  in
  String.concat Filename.dir_sep (prefix :: resolve [] subpath)

let read_static_file path enc =
  let path = String.split_on_char '/' path in
  get_from_file enc (sanitise_path !static_dir path)

let with_git_register =
  let dir_mutex = Lwt_utils.gen_mutex_table () in
  fun dir (f: unit -> string list Lwt.t) ->
    let git args () =
      Lwt_process.exec ("", Array.of_list ("git"::"-C"::dir::args)) >>= function
      | Unix.WEXITED 0 -> Lwt.return_unit
      | _ -> Lwt.fail_with ("git command failed: " ^ String.concat " " args)
    in
    dir_mutex.Lwt_utils.with_lock dir @@ fun () ->
    (if Sys.file_exists (Filename.concat dir ".git") then
       git ["reset";"--hard"] ()
     else
       git ["init"] () >>=
       git ["config";"--local";"user.name";"Learn-OCaml user"] >>=
       git ["config";"--local";"user.email";"none@learn-ocaml.org"]) >>=
    f >>= fun files ->
    git ("add"::"--"::files) () >>=
    git ["commit";"--allow-empty";"-m";"Update"] >>=
    git ["update-server-info"]

let write ?(no_create=false) file ?(extra=[]) contents =
  let dir = Filename.dirname file in
  (if not (Sys.file_exists file) then
     if no_create then Lwt.fail Not_found
     else Lwt_utils.mkdir_p ~perm:0o700 dir
   else Lwt.return_unit)
  >>= fun () ->
  let file = Filename.basename file in
  let write_file ?(flags=[Unix.O_TRUNC]) (fname, contents) =
    let file = sanitise_path dir (String.split_on_char '/' fname) in
    Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname file) >>= fun () ->
    Lwt_io.(with_file file
              ~flags:Unix.(O_WRONLY::O_NONBLOCK::O_CREAT::flags)
              ~mode:Output
              (fun chan -> write chan contents)) >|= fun () ->
    fname
  in
  let rec write_tmp () =
    let tmpfile = Printf.sprintf "%s.%07x.tmp" file (Random.int 0x0fffffff) in
    Lwt.catch
      (fun () -> write_file ~flags:[Unix.O_EXCL] (tmpfile, contents))
      (function Unix.Unix_error (Unix.EEXIST, _, _) -> write_tmp ()
              | e -> Lwt.fail e)
  in
  with_git_register dir @@ fun () ->
  write_tmp () >>= fun tmpfile ->
  Lwt_unix.rename (Filename.concat dir tmpfile) (Filename.concat dir file)
  >>= fun () ->
  Lwt_list.map_s write_file extra >>= fun extra ->
  Lwt.return (file :: extra)


module Lesson = struct

  module Index = struct

    include Lesson.Index

    let get () =
      read_static_file Learnocaml_index.lesson_index_path enc

  end

  include (Lesson: module type of struct include Lesson end
           with module Index := Lesson.Index)

  let get id =
    read_static_file (Learnocaml_index.lesson_path id) enc

end

module Playground = struct

  module Index = struct

    include Playground.Index

    let get () =
      read_static_file Learnocaml_index.playground_index_path enc

  end

  include (Playground: module type of struct include Playground end
           with module Index := Playground.Index)

  let get id =
    read_static_file (Learnocaml_index.playground_path id) enc

end

module Server = struct
  let get () =
    Lwt.catch
      (fun () -> read_static_file Learnocaml_index.server_config_path Server.config_enc)
      (fun e ->
        match e with
        | Unix.Unix_error (Unix.ENOENT,_,_) ->
           Lwt.return @@ Server.build_config Server.empty_preconfig
        | e -> raise e
      )
end

module Tutorial = struct

  module Index = struct

    include Tutorial.Index

    let get () =
      read_static_file Learnocaml_index.tutorial_index_path enc

  end

  include (Tutorial: module type of struct include Tutorial end
           with module Index := Tutorial.Index)

  let get id =
    read_static_file (Learnocaml_index.tutorial_path id) enc

end

module Exercise = struct

  type id = Exercise.id

  let index =
    ref (lazy (
        read_static_file Learnocaml_index.exercise_index_path Exercise.Index.enc
      ))

  module Meta0 = struct
    include Exercise.Meta

    let get id =
      Lazy.force !index >|= fun index -> Exercise.Index.find index id
  end

  module Status = struct
    include Exercise.Status

    let store_file () = Filename.concat !sync_dir "exercises.json"

    let tbl = lazy (
      let tbl = Hashtbl.create 223 in
      Lwt.catch (fun () ->
          get_from_file (J.list enc) (store_file ()) >|= fun l ->
          List.iter (fun st -> Hashtbl.add tbl st.id st) l;
          tbl)
      @@ function
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          Lazy.force !index >>= fun index ->
          Exercise.Index.fold_exercises (fun () id _ ->
              Hashtbl.add tbl id (Exercise.Status.default id))
            () index;
          Lwt.return tbl
      | e -> Lwt.fail e
    )

    let save () =
      Lazy.force tbl >>= fun tbl ->
      let l = Hashtbl.fold (fun _ s acc -> s::acc) tbl [] in
      let s = Json_codec.encode (J.list enc) l in
      write (store_file ()) s

    let get id =
      Lazy.force tbl >>= fun tbl ->
      try Lwt.return (Hashtbl.find tbl id)
      with Not_found -> Meta0.get id >|= fun _ -> default id

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

  module Meta = struct
    include Meta0

    let get id =
      get id >>= fun m ->
      Status.get id >|= fun s ->
      { m with
        requirements = Status.skills_prereq m s;
        focus = Status.skills_focus m s }

  end

  module Index = struct
    include Exercise.Index

    let get_from_index index =
      Exercise.Index.mapk_exercises (fun id m k ->
          Status.get id >>= fun s ->
          { m with Meta.requirements = Status.skills_prereq m s;
                   Meta.focus = Status.skills_focus m s }
          |> k)
        index
        Lwt.return

    let get () =
      Lazy.force !index >>= get_from_index

    let reload () =
      read_static_file Learnocaml_index.exercise_index_path Exercise.Index.enc
      >|= fun i -> index := lazy (Lwt.return i)

  end

  include (Exercise: module type of struct include Exercise end
           with type id := id
            and module Meta := Exercise.Meta
            and module Status := Exercise.Status
            and module Index := Exercise.Index)

  let get id =
    Lwt.catch
      (fun () -> read_static_file (Learnocaml_index.exercise_path id)
                   J.(tup3 Meta.enc enc (option float)) >>= fun (_, ex, _) ->
                 Lwt.return ex)
      (function
        | Unix.Unix_error _ -> Lwt.fail Not_found
        | e -> Lwt.fail e)

end

module Session = struct

  include Session

  let file = "sessions.json"

  let enc =
    let open Json_encoding in
    list (obj3
      (req "session" Session.enc)
      (req "token" Token.enc)
      (req "last_connection" float))

  let path dir = Filename.concat dir file

  let load dir =
    let p = path dir in
    Lwt_unix.file_exists dir >>= fun dir_exists ->
    (if not dir_exists then Lwt_unix.mkdir dir 0o700 else Lwt.return_unit) >>= fun () ->
    Lwt_unix.file_exists p >>= function
    | false ->
        Printf.printf "No session file, creating empty list\n%!";
        Lwt.return []
    | true ->
        Printf.printf "Loading sessions from: %s\n%!" p;
        get_from_file enc p

  let save dir table =
    write_to_file enc table (path dir)

  let get_user_token session =
    load !data_dir >>= fun table ->
    match List.find_opt (fun (s, _, _) -> s = session) table with
    | Some (_, token, _) -> Lwt.return_some token
    | None -> Lwt.return_none

  let set_session session token =
    let now = Unix.gettimeofday () in
    load !data_dir >>= fun table ->
    let table = (session, token, now) :: table in
    save !data_dir table

  let gen_session () =
    let len = 32 in
    Cryptokit.Random.string Cryptokit.Random.secure_rng len
  |> Cryptokit.transform_string @@ Cryptokit.Hexa.encode ()

end

module Token = struct

  include Token

  let path token =
    Filename.concat !sync_dir (Token.to_path token)

  let save_path token = Filename.concat (path token) "save.json"

  let find_save token =
    let save = save_path token in
    Lwt_unix.file_exists save >>= function
    | true -> Lwt.return_some save
    | false ->
        (* old layout: json stored directly as [path] instead of
           [path/save.json] *)
        let old_save = path token in
        Lwt_unix.file_exists old_save >>= fun ex ->
        if ex && not (Sys.is_directory old_save) then
          Lwt.return_some old_save
        else
          Lwt.return_none

  let exists token =
    find_save token >|= function None -> false | Some _ -> true

  let check_teacher token =
    if is_teacher token then exists token
    else Lwt.return_false

  let create_gen rnd =
    let rec aux () =
      let token = rnd () in
      let file = save_path token in
      Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname file) >>= fun () ->
      Lwt.catch (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o700 file
            ~flags:Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_EXCL])
            (fun _chan -> Lwt.return token))
      @@ function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> aux ()
      | e -> Lwt.fail e
    in
    aux ()

  let register ?(allow_teacher=false) token =
    if not allow_teacher && is_teacher token then
      Lwt.fail
        (Invalid_argument "Registration of teacher token forbidden. \
          Logout and use a new teacher token?")
    else
      Lwt.catch (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o700 (save_path token)
            ~flags:Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_EXCL])
            (fun _chan -> Lwt.return_unit))
      @@ function
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          Lwt.fail_with "token already exists"
      | e -> Lwt.fail e

  let create_student () =
    create_gen random

  let create_teacher () = create_gen random_teacher

  let delete token =
    let rec rec_rmdir d =
      Lwt.catch (fun () ->
          Lwt_unix.rmdir d >>= fun () ->
          let parent = Filename.dirname d in
          if parent = d then Lwt.return_unit else rec_rmdir parent)
        (function
          | Unix.Unix_error (Unix.EINVAL, _, _) -> Lwt.return_unit
          | e -> Lwt.fail e)
    in
    find_save token >>= function
    | None -> Lwt.return_unit
    | Some f ->
        Lwt_unix.unlink f >>= fun () ->
        rec_rmdir (Filename.dirname f)

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
          let d =
            if Filename.basename d = "save.json" then Filename.dirname d
            else d
          in
          let stok = String.map (function '/' | '\\' -> '-' | c -> c) d in
          try Lwt.return (Token.parse stok :: acc)
          with Failure _ -> Lwt.return acc
        ) "" []

  end

end

module Save = struct

  include Save

  let get token =
    Token.find_save token >>= function
    | Some save -> get_from_file (J.option enc) save
    | None -> Lwt.return_none

  let set token save =
    let file = Token.save_path token in
    (Token.find_save token >>= function
      | Some f when f <> file ->
          (* save file uses an old layout, move it to preserve attrs *)
          let tmp = f ^ ".tmp" in
          Lwt_unix.rename f tmp >>= fun () ->
          Lwt_utils.mkdir_p (Filename.dirname file) >>= fun () ->
          Lwt_unix.rename tmp file
      | _ -> Lwt.return_unit)
    >>= fun () ->
    let extra =
      SMap.fold (fun ex ans acc ->
          let filename = ex ^ ".ml" in
          let contents =
            String.concat "\n" [
              Printf.sprintf "(* GRADE: % 02d%% *)"
                (match ans.Answer.grade with Some g -> g | None -> 0);
              ans.Answer.solution;
              ""
            ]
          in
          (filename, contents) :: acc)
        save.all_exercise_states
        []
    in
    Lwt.catch (fun () ->
        write ~no_create:(Token.is_teacher token) ~extra file
          (Json_codec.encode ~minify:false enc save))
      (function
        | Not_found -> Lwt.fail_with "Unregistered teacher token"
        | e -> Lwt.fail e)

end

module Student = struct

  open Student

  let get_saved token =
    Save.get token >>= function
    | None ->
        Lwt.return (default token)
    | Some save ->
        let nickname = match save.Save.nickname with
          | "" -> None
          | n -> Some n
        in
        let results =
          SMap.map
            (fun st -> Answer.(st.mtime, st.grade))
            save.Save.all_exercise_states
        in
        let tags = SSet.empty in
        (Token.find_save token >>= function
          | None -> Lwt.return 0.
          | Some f -> Lwt_unix.stat f >|= fun st -> st.Unix.st_ctime)
        >|= fun creation_date ->
        {token; nickname; results; creation_date; tags}

  module Index = struct

    include Index

    (* Results and nickname are stored as part of the student's save, only the
       tags appear here at the moment *)
    let store_enc =
      J.(assoc (obj1 (dft "tags" (list string) [])))
      |> J.conv
        (fun ttmap ->
           Token.Map.fold (fun tok tags l ->
               (Token.to_string tok, SSet.elements tags) :: l)
             ttmap [])
        (fun ttl ->
           List.fold_left (fun map (tok, tags) ->
               Token.Map.add (Token.parse tok) (SSet.of_list tags) map)
             Token.Map.empty ttl)

    let store_file () = Filename.concat !sync_dir "students.json"

    let load () =
      Lwt.catch
        (fun () -> get_from_file store_enc (store_file ()))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return Token.Map.empty
          | e -> Lwt.fail e)

    let map = lazy (load () >|= fun m -> ref m)

    let save () =
      Lazy.force map >>= fun map ->
      let s = Json_codec.encode store_enc !map in
      write (store_file ()) s

    let get_student map token =
      Lwt.try_bind (fun () -> get_saved token)
        (fun std ->
           match Token.Map.find_opt token map with
           | Some tags -> Lwt.return_some {std with tags}
           | None -> Lwt.return_some std)
        (fun e ->
           Format.eprintf "[ERROR] Corrupt save, cannot load %s: %s@."
             (Token.to_string token)
             (Printexc.to_string e);
           Lwt.return_none)

    let get () =
      Lazy.force map >>= fun map ->
      Token.Index.get ()
      >|= List.filter Token.is_student
      >>= Lwt_list.filter_map_p (get_student !map)

    let set =
      let map_mutex = Lwt_mutex.create () in
      fun l ->
        Lwt_mutex.with_lock map_mutex @@ fun () ->
        Lazy.force map >>= fun map ->
        map := List.fold_left
            (fun map std -> Token.Map.add std.token std.tags map)
            !map l;
        save ()

  end

  let get token =
    Lazy.force Index.map >>= fun map ->
    Index.get_student !map token

  let set std = Index.set [std]

  include (Student: module type of struct include Student end
           with module Index := Student.Index)

end
