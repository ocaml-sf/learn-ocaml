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

let static_dir = ref (Filename.concat (Sys.getcwd ()) "www")

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")
(*
let allow_teacher_account_creation = ref true

let auth_file () = Filename.concat !sync_dir "auth.json"
*)
let port = ref 8080

let args = Arg.align @@
  [ "-static-dir", Arg.Set_string static_dir,
    "PATH where static files should be found (./www)" ;
    "-sync-dir", Arg.Set_string sync_dir,
    "PATH where sync tokens are stored (./sync)" ;
    "-port", Arg.Set_int port,
    "PORT the TCP port (8080)" ]

open Lwt.Infix

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
          Learnocaml_sync.Token.(to_path token) in
      Lwt_io.(with_file ~mode:Input path (fun chan ->
          read chan >|= fun str ->
          let json = Ezjsonm.from_string str in
          Json_encoding.destruct Learnocaml_sync.save_file_enc json)))
  @@ function
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise Not_found
  | e -> raise e

let create_token_file token =
  let path = Filename.concat !sync_dir (Learnocaml_sync.Token.to_path token) in
  Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname path) >>= fun () ->
  Lwt_io.(with_file ~mode: Output ~perm:0o700 path (fun chan -> write chan ""))

let store token contents =
  let path =
    Filename.concat !sync_dir
      (Learnocaml_sync.Token.to_path token) in
  (if not (Sys.file_exists path) then create_token_file token
   else Lwt.return_unit) >>= fun _ ->
  Lwt_io.(with_file ~mode: Output path (fun chan -> write chan contents))

let rec gimme () =
  let token = Learnocaml_sync.Token.random () in
  if Sys.file_exists (Learnocaml_sync.Token.to_path token) then
    gimme ()
  else
    create_token_file token >|= fun () -> token

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
 *       let token = Learnocaml_sync.Token.random ~admin:true () in
 *       let auth = {
 *         one_time_token = Some token;
 *         teachers = [];
 *       } in
 *       write_auth f auth >|= fun () ->
 *       Printf.printf
 *         "Use %s/first-login/%s to initialise a teacher account.\n%!"
 *         url (Learnocaml_sync.Token.to_string token);
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

module Json_codec = struct
  let decode enc s =
      Ezjsonm.from_string s |>
      Json_encoding.destruct enc

  let encode enc x =
    match Json_encoding.construct enc x with
    | `A _ | `O _ as json -> Ezjsonm.to_string json
    | _ -> assert false
end

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

  let callback
    : type resp. resp Api.request -> resp ret
    = function
      | Api.Version () ->
          respond_json "LEARNOCAML_VERSION_FILLME"
      | Api.Static path ->
          respond_static path
      | Api.Static_json _ -> assert false
      | Api.Create_token () ->
          gimme () >>= respond_json
      | Api.Create_teacher_token _key ->
          gimme () >>= respond_json (* FIXME *)
      | Api.Fetch_save token ->
          Lwt.catch
            (fun () -> retrieve token >>= Lwt.return_some >>= respond_json)
            (function
              | Not_found -> respond_json None
              | e ->
                  Lwt.return
                    (Error (`Internal_server_error, Printexc.to_string e)))
      | Api.Update_save (token, save) ->
          (* TODO: do the merge here, server-side, with a mutex *)
          let s = Json_codec.encode Learnocaml_sync.save_file_enc save in
          store token s >>= fun () -> respond_json save
      | Api.Exercise_index _token ->
          (* TODO: check token; retrieve dedicated exercise assignment *)
          read_static_file [Learnocaml_index.exercise_index_path] >|=
          Ezjsonm.from_string >|=
          Json_encoding.destruct Learnocaml_index.exercise_index_enc >>=
          respond_json
      | Api.Invalid_request s ->
          Lwt.return (Error (`Bad_request, s))

end

module Api_server = Api.Server (Json_codec) (Request_handler)

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
  (* get_auth () >>= fun auth ->
   * let callback = callback auth in *)
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
