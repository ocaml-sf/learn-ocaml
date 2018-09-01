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
open Learnocaml_store

let static_dir = ref (Filename.concat (Sys.getcwd ()) "www")

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

let port = ref 8080

let log_channel = ref (Some stdout)

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

exception Too_long_body

let string_of_stream ?(max_size = 1024 * 1024) s =
  let b = Buffer.create (64 * 1024) in
  let pos = ref 0 in
  let add_string s =
    pos := !pos + String.length s;
    if !pos > max_size then
      Lwt.fail Too_long_body
    else begin
      Buffer.add_string b s;
      Lwt.return_unit
    end
  in
  Lwt.catch begin function () ->
    Lwt_stream.iter_s add_string s >>= fun () ->
    Lwt.return (Some (Buffer.contents b))
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

let with_verified_teacher_token token cont =
  Token.check_teacher token >>= function
  | true -> cont ()
  | false -> Lwt.return (Error (`Forbidden, "Access restricted"))

let string_of_date ts =
  let open Unix in
  let tm = gmtime ts in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let log conn api_req =
  match !log_channel with
  | None -> ()
  | Some oc ->
      let src_addr = match conn with
        | Conduit_lwt_unix.TCP tcp, _ ->
            Ipaddr.to_string tcp.Conduit_lwt_unix.ip
        | _ -> ""
      in
      output_string oc (string_of_date (Unix.gettimeofday ()));
      output_char oc '\t';
      output_string oc src_addr;
      output_char oc '\t';
      output_string oc
        (match api_req.Api.meth with
         | `GET -> "GET "
         | `POST _ -> "POST");
      output_char oc '\t';
      output_char oc '/';
      output_string oc (String.concat "/" api_req.Api.path);
      (match api_req.Api.args with | [] -> () | l ->
          output_char oc '?';
          output_string oc
            (String.concat "&" (List.map (fun (a, b) -> a ^"="^ b) l)));
      output_char oc '\n';
      flush oc

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
          respond_json Api.version
      | Api.Static path ->
          respond_static path
      | Api.Create_token (None, nick) ->
          Token.create_student () >>= fun tok ->
          (match nick with None -> Lwt.return_unit | Some nickname ->
              Save.set tok Save.{empty with nickname}) >>= fun () ->
          respond_json tok
      | Api.Create_token (Some token, _nick) ->
          Lwt.catch
            (fun () -> Token.register token >>= fun () -> respond_json token)
            (function
              | Failure m -> Lwt.return (Error (`Bad_request, m))
              | exn ->
                  Lwt.return
                    (Error (`Internal_server_error, Printexc.to_string exn)))
      | Api.Create_teacher_token token ->
          with_verified_teacher_token token @@ fun () ->
          Token.create_teacher () >>= respond_json

      | Api.Fetch_save token ->
          Lwt.catch
            (fun () -> Save.get token >>= function
               | Some save -> respond_json save
               | None -> Lwt.return (Error (`Not_found, "token not found")))
          @@ fun exn ->
          Lwt.return
            (Error (`Internal_server_error, Printexc.to_string exn))
      | Api.Update_save (token, save) ->
          let save = Save.fix_mtimes save in
          let exercise_states = SMap.bindings save.Save.all_exercise_states in
          (Token.check_teacher token >>= function
            | true -> Lwt.return exercise_states
            | false ->
                Lwt_list.filter_s (fun (id, _) ->
                    Exercise.Status.is_open id token >|= function
                    | `Open -> true
                    | `Closed -> false
                    | `Deadline t -> t >= -300. (* Grace period! *))
                  exercise_states)
          >>= fun valid_exercise_states ->
          let save =
            { save with
              Save.all_exercise_states =
                List.fold_left (fun m (id,save) -> SMap.add id save m)
                  SMap.empty valid_exercise_states }
          in
          let key = (token :> Token.t) in
          let mutex =
            try Hashtbl.find token_save_mutexes key with Not_found ->
              let mut = Lwt_mutex.create () in
              Hashtbl.add token_save_mutexes key mut;
              mut
          in
          Lwt_mutex.with_lock mutex @@ fun () ->
          Lwt.finalize (fun () ->
              Save.get token >>= function
              | None ->
                  Lwt.return
                    (Error (`Not_found, Token.to_string token))
              | Some prev_save ->
                let save = Save.sync prev_save save in
                Save.set token save >>= fun () -> respond_json save)
            (fun () ->
               if Lwt_mutex.is_empty mutex
               then Hashtbl.remove token_save_mutexes key;
               Lwt.return_unit)

      | Api.Students_list token ->
          with_verified_teacher_token token @@ fun () ->
          Token.Index.get ()
          >|= List.filter Token.is_student
          >>= Lwt_list.map_p (fun token ->
              Lwt.catch (fun () -> Save.get token)
                (fun e ->
                   Format.eprintf "[ERROR] Corrupt save, cannot load %s: %s@."
                     (Token.to_string token)
                     (Printexc.to_string e);
                   Lwt.return_none)
              >>= function
              | None ->
                  Lwt.return Student.{
                      token;
                      nickname = None;
                      results = SMap.empty;
                      tags = [];
                    }
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
                  Lwt.return Student.{token; nickname; results; tags})
          >>= respond_json
      | Api.Students_csv token ->
          with_verified_teacher_token token @@ fun () ->
          Token.Index.get ()
          >|= List.filter Token.is_student
          >>= Lwt_list.map_p (fun token ->
              Save.get token >|= fun save -> token, save)
          >>= fun tok_saves ->
          let all_exercises =
            List.fold_left (fun acc (_tok, save) ->
                match save with
                | None -> acc
                | Some save ->
                    SMap.fold (fun ex_id _ans acc -> SSet.add ex_id acc)
                      save.Save.all_exercise_states
                      acc)
              SSet.empty tok_saves
          in
          let columns =
            "token" :: "nickname" ::
            (List.rev @@
             SSet.fold (fun ex_id acc ->
                 (ex_id ^ " date") ::
                 (ex_id ^ " grade") ::
                 acc)
               all_exercises [])
          in
          let buf = Buffer.create 3497 in
          let sep () = Buffer.add_char buf ',' in
          let line () = Buffer.add_char buf '\n' in
          Buffer.add_string buf (String.concat "," columns);
          line ();
          List.iter (fun (tok, save) ->
              match save with None -> () | Some save ->
                Buffer.add_string buf (Token.to_string tok);
                sep ();
                Buffer.add_string buf save.Save.nickname;
                SSet.iter (fun ex_id ->
                    sep ();
                    try
                      let st = SMap.find ex_id save.Save.all_exercise_states in
                      (match st.Answer.grade with
                       | Some n -> Buffer.add_string buf (string_of_int n)
                       | None -> ());
                      sep ();
                      Buffer.add_string buf (string_of_date st.Answer.mtime)
                    with Not_found -> sep ())
                  all_exercises;
                line ())
            tok_saves;
          Lwt.return (Ok (Buffer.contents buf, "text/csv"))

      | Api.Exercise_index token ->
          Exercise.Index.get () >>= fun index ->
          Token.check_teacher token >>= (function
              | true -> Lwt.return (index, [])
              | false ->
                  let deadlines = ref [] in
                  Exercise.Index.filterk
                    (fun id _ k ->
                       Exercise.Status.is_open id token >>= function
                       | `Open -> k true
                       | `Closed -> k false
                       | `Deadline t ->
                           deadlines := (id, max t 0.) :: !deadlines;
                           k true)
                    index (fun index -> Lwt.return (index, !deadlines)))
          >>= respond_json
      | Api.Exercise (token, id) ->
          (Exercise.Status.is_open id token >>= function
          | `Open | `Deadline _ as o ->
              Exercise.Meta.get id >>= fun meta ->
              Exercise.get id >>= fun ex ->
              respond_json
                (meta, ex,
                 match o with `Deadline t -> Some (max t 0.) | `Open -> None)
          | `Closed ->
              Lwt.return (Error (`Forbidden, "Exercise closed")))

      | Api.Lesson_index () ->
          Lesson.Index.get () >>= respond_json
      | Api.Lesson id ->
          Lesson.get id >>= respond_json

      | Api.Tutorial_index () ->
          Tutorial.Index.get () >>= respond_json
      | Api.Tutorial id ->
          Tutorial.get id >>= respond_json

      | Api.Exercise_status_index token ->
          with_verified_teacher_token token @@ fun () ->
          Exercise.Status.all () >>= respond_json
      | Api.Exercise_status (token, id) ->
          with_verified_teacher_token token @@ fun () ->
          Exercise.Status.get id >>= respond_json
      | Api.Set_exercise_status (token, status) ->
          with_verified_teacher_token token @@ fun () ->
          Lwt_list.iter_s Exercise.Status.set status >>= respond_json

      | Api.Invalid_request s ->
          Lwt.return (Error (`Bad_request, s))

end

module Api_server = Api.Server (Json_codec) (Request_handler)

let init_teacher_token () =
  Token.Index.get () >>= function tokens ->
    match List.filter Token.is_teacher tokens with
    | [] ->
        Token.create_teacher () >|= fun token ->
        Printf.printf "Initial teacher token created: %s\n%!"
          (Token.to_string token)
    | teachers ->
        Printf.printf "Found the following teacher tokens:\n  - %s\n%!"
          (String.concat "\n  - " (List.map Token.to_string teachers));
        Lwt.return_unit

let launch () =
  (* Learnocaml_store.init ~exercise_index:
   *   (String.concat Filename.dir_sep
   *      (!static_dir :: Learnocaml_index.exercise_index_path)); *)
  let callback conn req body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    let path = Stringext.split ~on:'/' path in
    let path = List.filter ((<>) "") path in
    let query = Uri.query uri in
    let args = List.map (fun (s, l) -> s, String.concat "," l) query in
    (* let cookies = Cohttp.Cookie.Cookie_hdr.extract (Cohttp.Request.headers req) in *)
    let respond = function
      | Ok (str, content_type) ->
          let headers = Cohttp.Header.init_with "Content-Type" content_type in
          Server.respond_string ~headers ~status:`OK ~body:str ()
      | Error (status, body) ->
          Server.respond_error ~status ~body ()
    in
    (match req.Request.meth with
     | `GET -> Lwt.return (Ok {Api.meth = `GET; path; args})
     | `POST ->
         (string_of_stream (Cohttp_lwt.Body.to_stream body) >|= function
           | Some s -> Ok {Api.meth = `POST s; path; args}
           | None -> Error (`Bad_request, "Missing POST body"))
     | _ -> Lwt.return (Error (`Bad_request, "Unsupported method")))
    >>= (function
        | Ok req ->
            log conn req;
            Api_server.handler req
        | Error e -> Lwt.return (Error e))
    >>=
    respond
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
