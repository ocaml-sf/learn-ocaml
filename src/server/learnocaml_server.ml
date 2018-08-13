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

open Learnocaml_data

let static_dir = ref (Filename.concat (Sys.getcwd ()) "www")

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

let port = ref 8080

let args = Arg.align @@
  [ "-static-dir", Arg.Set_string static_dir,
    "PATH where static files should be found (./www)" ;
    "-sync-dir", Arg.Set_string sync_dir,
    "PATH where sync tokens are stored (./sync)" ;
    "-port", Arg.Set_int port,
    "PORT the TCP port (8080)" ]

open Lwt.Infix

module Json_codec = struct
  let decode enc s =
    Ezjsonm.from_string s |>
    Json_encoding.destruct enc

  let encode enc x =
    match Json_encoding.construct enc x with
    | `A _ | `O _ as json -> Ezjsonm.to_string json
    | `Null -> "{}"
    | _ -> assert false
end

let read_static_file path =
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
  Lwt_io.(with_file ~mode: Input path read)

let retrieve token =
  Lwt.catch (fun () ->
      let path =
        Filename.concat !sync_dir
          Token.(to_path token) in
      Lwt_io.(with_file ~mode:Input path (fun chan ->
          read chan >|= fun str ->
          let str = if str = "" then "{}" else str in
          Json_codec.decode Save.enc str))
      >>= Lwt.return_some)
  @@ function
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none
  | e -> Lwt.fail e

let token_file_path token =
  Filename.concat !sync_dir (Token.to_path token)

let create_token_file token =
  let path = token_file_path token in
  Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname path) >>= fun () ->
  Lwt_io.(with_file ~mode: Output ~perm:0o700 path (fun chan -> write chan ""))

let token_exists token =
  Sys.file_exists (token_file_path token)

let store token save =
  let contents = Json_codec.encode Save.enc save in
  (if not (token_exists token) then create_token_file token
   else Lwt.return_unit) >>= fun _ ->
  Lwt_io.(with_file ~mode: Output (token_file_path token)
            (fun chan -> write chan contents))

let rec gimme ?(teacher=false) () =
  let token =
    if teacher then Token.random_teacher ()
    else Token.random ()
  in
  if token_exists token then
    gimme ~teacher ()
  else
    create_token_file token >|= fun () -> token

let all_students_tokens () =
  Array.fold_left (fun acc x1 ->
      let d = Filename.concat !sync_dir x1 in
      if Sys.is_directory d then
        Array.fold_left (fun acc x2 ->
            let d = Filename.concat d x2 in
            if Sys.is_directory d then
              Array.fold_left (fun acc x3 ->
                  let d = Filename.concat d x3 in
                  if Sys.is_directory d then
                    Array.fold_left (fun acc x4 ->
                        try
                          Token.parse
                            (Printf.sprintf "%s-%s-%s-%s" x1 x2 x3 x4)
                          :: acc
                        with Failure _ -> acc)
                      acc
                      (Sys.readdir d)
                  else acc)
                acc
                (Sys.readdir d)
            else acc)
          acc
          (Sys.readdir d)
      else acc)
    []
    (Sys.readdir !sync_dir)

(* let auth_encoding =
 *   let open Json_encoding in
 *   let teacher_encoding =
 *     obj2
 *       (req "login" string)
 *       (req "password" string)
 *     |> conv
 *       (fun {teacher_login; teacher_password} ->
 *          teacher_login, teacher_password)
 *       (fun (teacher_login, teacher_password) ->
 *          {teacher_login; teacher_password})
 *   in
 *   list teacher_encoding |> conv
 *     (fun { one_time_token = _; teachers } -> teachers)
 *     (fun teachers -> { one_time_token = None; teachers })
 * 
 * let read_auth file =
 *   let open Lwt_io in
 *   with_file ~mode:Input file @@ fun ic ->
 *   read ic >|= fun str ->
 *   (Ezjsonm.from_string str |>
 *    Json_encoding.destruct auth_encoding)
 * 
 * let write_auth file auth =
 *   let open Lwt_io in
 *   with_file ~mode:Output file @@ fun oc ->
 *   Json_encoding.construct auth_encoding auth |> function
 *   | `O _ | `A _ as json ->
 *       write oc (Ezjsonm.to_string json)
 *   | _ -> assert false
 * 
 * let get_auth ?(url="URL") () =
 *   let f = auth_file () in
 *   Lwt_unix.file_exists f >>= function
 *   | true -> read_auth f
 *   | false ->
 *       let token = Token.random ~admin:true () in
 *       let auth = {
 *         one_time_token = Some token;
 *         teachers = [];
 *       } in
 *       write_auth f auth >|= fun () ->
 *       Printf.printf
 *         "Use %s/first-login/%s to initialise a teacher account.\n%!"
 *         url (Token.to_string token);
 *       auth *)

exception Too_long_body

let string_of_stream ?(max_size = 64 * 1024) s =
  let b = Bytes.create max_size in
  let pos = ref 0 in
  let add_string s =
    let len = String.length s in
    pos := !pos + len ;
    if !pos > max_size then
      Lwt.fail Too_long_body
    else begin
      String.blit s 0 b (!pos - len) len ;
      Lwt.return_unit
    end
  in
  Lwt.catch begin function () ->
    Lwt_stream.iter_s add_string s >>= fun () ->
    Lwt.return (Some (Bytes.sub_string b 0 !pos))
  end begin function
    | Too_long_body -> Lwt.return None
    | e -> Lwt.fail e
  end

module Api = Learnocaml_api

open Cohttp_lwt_unix

let respond_static path =
  Lwt.catch
    (fun () ->
       read_static_file path >|= fun body ->
       Ok (body, Magic_mime.lookup (List.fold_left (fun _ r -> r) "" path)))
    (fun e ->
       Lwt.return (Error (`Not_found, Printexc.to_string e)))

let respond_json = fun x ->
  Lwt.return (Ok (x, "application/json"))

module Request_handler = struct

  type 'a ret = ('a * string, Cohttp.Code.status_code * string) result Lwt.t

  let map_ret f r =
    r >|= function
    | Ok (x, content_type) -> Ok (f x, content_type)
    | Error (code, msg) -> Error (code, msg)

  let token_save_mutexes = Hashtbl.create 223

  let callback
    : type resp. resp Api.request -> resp ret
    = function
      | Api.Version () ->
          respond_json "LEARNOCAML_VERSION_FILLME"
      | Api.Static path ->
          respond_static path
      | Api.Static_json _ -> assert false
      | Api.Create_token None ->
          gimme () >>= respond_json
      | Api.Create_token (Some token) ->
          if token_exists token then
            Lwt.return (Error (`Bad_request, "token already exists"))
          else
            create_token_file token >>= fun () -> respond_json token
      | Api.Create_teacher_token token ->
          if token_exists token then
            gimme ~teacher:true () >>= respond_json
          else
            Lwt.return (Error (`Forbidden, "Unknown teacher token"))
      | Api.Fetch_save token ->
          Lwt.catch
            (fun () -> retrieve token >>= function
               | Some save -> respond_json save
               | None -> Lwt.return (Error (`Not_found, "token not found")))
          @@ fun exn ->
          Lwt.return
            (Error (`Internal_server_error, Printexc.to_string exn))
      | Api.Update_save (token, save) ->
          let save = Save.fix_mtimes save in
          let key = (token :> Token.t) in
          let mutex =
            try Hashtbl.find token_save_mutexes key with Not_found ->
              let mut = Lwt_mutex.create () in
              Hashtbl.add token_save_mutexes key mut;
              mut
          in
          Lwt_mutex.with_lock mutex @@ fun () ->
          Lwt.finalize (fun () ->
              retrieve token >>= function
              | None ->
                  Lwt.return
                    (Error (`Not_found, Token.to_string token))
              | Some prev_save ->
                let save = Save.sync prev_save save in
                store token save >>= fun () -> respond_json save)
            (fun () ->
               if Lwt_mutex.is_empty mutex
               then Hashtbl.remove token_save_mutexes key;
               Lwt.return_unit)
      | Api.Exercise_index _token ->
          (* TODO: check token; retrieve dedicated exercise assignment *)
          read_static_file [Learnocaml_index.exercise_index_path] >|=
          Ezjsonm.from_string >|=
          Json_encoding.destruct Learnocaml_index.exercise_index_enc >>=
          respond_json
      | Api.Students_list _token ->
          Lwt_list.map_p (fun token ->
              retrieve token >>= function
              | None -> failwith "TODO"
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
                  let tags = [] in
                  Lwt.return Learnocaml_api.Student.{token; nickname; results; tags})
            (all_students_tokens ()) >>=
          respond_json
      | Api.Invalid_request s ->
          Lwt.return (Error (`Bad_request, s))

end

module Api_server = Api.Server (Json_codec) (Request_handler)

let init_teacher_token () =
  let path =
    Filename.concat !sync_dir Token.teacher_tokens_path
  in
  let rec empty dir =
    match Sys.readdir dir with
    | files ->
        Array.for_all
          (fun f ->
             let f = Filename.concat dir f in
             match Sys.is_directory f with
             | true -> empty f
             | false -> false
             | exception (Sys_error _) -> true)
          files
    | exception (Sys_error _) -> true
  in
  if empty path then
    let token = Token.random_teacher () in
    create_token_file token >|= fun () ->
    Printf.printf "Initial teacher token created: %s\n%!"
      (Token.to_string token)
  else
    Lwt.return_unit

let launch () =
  (* Learnocaml_store.init ~exercise_index:
   *   (String.concat Filename.dir_sep
   *      (!static_dir :: Learnocaml_index.exercise_index_path)); *)
  let callback _ req body =
    let path = Uri.path (Request.uri req) in
    let path = Stringext.split ~on:'/' path in
    let path = List.filter ((<>) "") path in
    (* let cookies = Cohttp.Cookie.Cookie_hdr.extract (Cohttp.Request.headers req) in *)
    let respond = function
      | Ok (str, content_type) ->
          let headers = Cohttp.Header.init_with "Content-Type" content_type in
          Server.respond_string ~headers ~status:`OK ~body:str ()
      | Error (status, body) ->
          Server.respond_error ~status ~body ()
    in
    match req.Request.meth with
    | `GET ->
        Api_server.handler {Api.meth = `GET; path} >>= respond
    | `POST ->
        (string_of_stream (Cohttp_lwt.Body.to_stream body) >>= function
          | Some s ->
              Api_server.handler {Api.meth = `POST s; path} >>= respond
          | None ->
              respond (Error (`Bad_request, "Missing POST body")))
    | _ ->
        respond (Error (`Bad_request, "Unsupported method"))
  in
  Random.self_init () ;
  init_teacher_token () >>= fun () ->
  Lwt.catch (fun () ->
      Server.create
        ~on_exn: (function
            | Unix.Unix_error(Unix.EPIPE, "write", "") -> ()
            | exn -> raise exn)
        ~mode:(`TCP (`Port !port)) (Server.make ~callback ()) >>= fun () ->
      Lwt.return true)
  @@ function
  | Sys.Break ->
      Lwt.return true
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Printf.eprintf
        "Could not bind port %d, another instance may still be running?\n%!"
        !port;
      Lwt.return false
  | e ->
      Printf.eprintf "Server error: %s\n%!" (Printexc.to_string e);
      Lwt.return false
