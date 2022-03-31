(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Learnocaml_store

let port = ref 8080

let cert_key_files = ref None

let log_channel = ref (Some stdout)

let base_url = ref ""

let args = Arg.align @@
  [ "-static-dir", Arg.Set_string static_dir,
    "PATH where static files should be found (./www)" ;
    "-sync-dir", Arg.Set_string sync_dir,
    "PATH where sync tokens are stored (./sync)" ;
    "-base-url", Arg.Set_string base_url,
    "BASE_URL of the website. \
     Should not end with a trailing slash. \
     Currently, this has no effect on the native backend. \
     Mandatory for 'learn-ocaml build' if the site is not hosted in path '/', \
     which typically occurs for static deployment." ;
    "-port", Arg.Set_int port,
    "PORT the TCP port (8080)" ]

open Lwt.Infix

let read_static_file path =
  Lwt_io.(with_file ~mode: Input (sanitise_path !static_dir path) read)

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

type cache_request_hash = string list

(** nocache, shortcache, longcache indicates the client-side caching. The
    associated key is used for server-side caching *)
type caching =
  | Nocache (* dynamic resources *)
  | Shortcache of cache_request_hash option (* valid for the server lifetime *)
  | Longcache of cache_request_hash (* static resources *)

type cached_response = {
  body: string;
  deflated_body: string option;
  content_type: string;
  caching: caching;
}

type 'a response =
  | Response of { contents: 'a;
                  content_type: string;
                  caching: caching }
  | Cached of cached_response

type error = (Cohttp.Code.status_code * string)

let disable_cache =
  match Sys.getenv_opt "LEARNOCAML_SERVER_NOCACHE" with
  | None | Some ("" | "0" | "false") -> false
  | Some _ -> true

let caching: type resp. resp Api.request -> caching = fun resp ->
  if disable_cache then Nocache else
  match resp with
  | Api.Version () -> Shortcache (Some ["version"; "server_id"])
  | Api.Static ("fonts"::_ | "icons"::_ | "js"::_::_::_ as p) -> Longcache p
  | Api.Static ("css"::_ | "js"::_ | _ as p) -> Shortcache (Some p)

  | Api.Exercise _ -> Nocache

  | Api.Lesson_index () -> Shortcache (Some ["lessons"])
  | Api.Lesson id -> Shortcache (Some ["lesson";id])
  | Api.Tutorial_index () -> Shortcache (Some ["tutorials"])
  | Api.Tutorial id -> Shortcache (Some ["tutorial";id])

  | _ -> Nocache

let lwt_ok r = Lwt.return (Ok r)
let lwt_fail e = Lwt.return (Error e)

let ( >?= ) x f =
  x >>= function
  | Ok x -> f x
  | Error x -> lwt_fail x

let lwt_catch_fail f e =
  Lwt.catch f (fun exn -> lwt_fail @@ e exn)

let lwt_option_fail x e f =
  match x with
  | Some x -> f x
  | None -> lwt_fail e

let respond_static caching path =
  lwt_catch_fail
    (fun () ->
       read_static_file path >>= fun contents ->
       let content_type =
         Magic_mime.lookup (List.fold_left (fun _ r -> r) "" path)
       in
       lwt_ok @@ Response { contents; content_type; caching })
    (fun e -> (`Not_found, Printexc.to_string e))

let respond_json caching contents =
  lwt_ok @@
    Response { contents;
               content_type = "application/json";
               caching }

let verify_teacher_token token =
  Token.check_teacher token >>= function
  | true -> lwt_ok ()
  | false -> lwt_fail (`Forbidden,"Access restricted")

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

let check_report exo report grade =
  let max_grade = Learnocaml_exercise.(access File.max_score) exo in
  let score, _ = Learnocaml_report.result report in
  score * 100 / max_grade = grade

module Memory_cache = struct

  let (tbl: (cache_request_hash, cached_response) Hashtbl.t) =
    Hashtbl.create 533

  let get key =
    try Some (Hashtbl.find tbl key) with Not_found -> None

  let add key entry =
    Hashtbl.replace tbl key entry

end

module Request_handler = struct

  type 'a ret = ('a response, error) result Lwt.t

  let map_ret f r =
    r >?= function
    | Response ({contents; _} as r) -> lwt_ok @@ Response {r with contents = f contents}
    | (Cached _) as r -> lwt_ok r

  let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let alphanum_len = String.length alphanum

  let nonce_req : (string, string) Hashtbl.t = Hashtbl.create 533

  let token_save_mutex = Lwt_utils.gen_mutex_table ()

  let rec string_of_endp =
    function
    | `TCP (i,_) -> Some (Ipaddr.to_string i)
    | `Unix_domain_socket s -> Some s
    | `Vchan_direct (i,_) -> Some (string_of_int i)
    | `Vchan_domain_socket (u,v) -> Some (u ^ v)
    | `TLS (s,e) ->
       begin
         match string_of_endp e with
         | Some s' -> Some (s ^ s')
         | None    -> None
       end
    | `Unknown _ -> None

  let valid_string_of_endp e =
    lwt_option_fail
      (string_of_endp e)
      (`Forbidden, "No address information avaible")
      lwt_ok

  let callback_raw: type resp. Conduit.endp -> Learnocaml_data.Server.config ->
                    caching -> resp Api.request ->
                    (resp response, error) result Lwt.t
    = let module ServerData = Learnocaml_data.Server in
      fun conn config cache -> function
      | Api.Version () ->
         respond_json cache (Api.version, config.ServerData.server_id)
      | Api.Static path ->
         respond_static cache path
      | Api.Nonce () ->
         valid_string_of_endp conn
         >?= fun conn ->
         let nonce =
           match Hashtbl.find_opt nonce_req conn with
           | Some old -> old
           | None ->
              let nonce = String.init 20 (fun _ -> alphanum.[Random.int alphanum_len]) in
              Hashtbl.add nonce_req conn nonce;
              nonce
         in respond_json cache nonce
      | Api.Create_token (secret_candidate, None, nick) ->
         valid_string_of_endp conn
         >?= fun conn ->
         lwt_option_fail
           (Hashtbl.find_opt nonce_req conn)
           (`Forbidden, "No registered token for your address")
         @@ fun nonce ->
         Hashtbl.remove nonce_req conn;
         let know_secret =
           match config.ServerData.secret with
           | None -> true
           | Some x -> Sha.sha512 (nonce ^ x) = secret_candidate in
         if not know_secret
         then lwt_fail (`Forbidden, "Bad secret")
         else
           Token.create_student ()
           >>= fun tok ->
           (match nick with | None -> Lwt.return_unit
                            | Some nickname ->
                               Save.set tok Save.{empty with nickname})
           >>= fun () -> respond_json cache tok
      | Api.Create_token (_secret_candidate, Some token, _nick) ->
         lwt_catch_fail
            (fun () -> Token.register token >>= fun () -> respond_json cache token)
            (function
             | Failure body -> (`Bad_request, body)
             | exn -> (`Internal_server_error, Printexc.to_string exn))
      | Api.Create_teacher_token (token, nick) ->
         verify_teacher_token token
         >?= fun () ->
         Token.create_teacher ()
         >>= fun tok ->
         (match nick with | None -> Lwt.return_unit
                          | Some nickname ->
                              Save.set tok Save.{empty with nickname})
         >>= fun () -> respond_json cache tok
      | Api.Fetch_save token ->
         lwt_catch_fail
           (fun () ->
             Save.get token >>= fun tokopt ->
             lwt_option_fail
               tokopt
               (`Not_found, "token not found")
               (respond_json cache))
         (fun exn -> (`Internal_server_error, Printexc.to_string exn))
      | Api.Archive_zip token ->
          let open Lwt_process in
          let path = Filename.concat !sync_dir (Token.to_path token) in 
          let cmd = shell ("git archive master --format=zip -0 --remote="^path)
          and stdout = `FD_copy Unix.stdout in
          Lwt_process.pread ~stdin:stdout cmd >>= fun contents ->
          lwt_ok @@ Response { contents = contents;
                               content_type = "application/zip";
                               caching = Nocache }
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
          token_save_mutex.Lwt_utils.with_lock (token :> Token.t) (fun () ->
              Save.get token >>= fun x ->
              lwt_option_fail x
                (`Not_found, Token.to_string token)
              @@ fun prev_save ->
                  let save = Save.sync prev_save save in
                  Save.set token save >>= fun () -> respond_json cache save)
      | Api.Git (token, path) ->
          let prefix =
            let ( / ) = Filename.concat in
            !sync_dir / Token.to_path token / ".git"
          in
          let path = sanitise_path prefix path in
          lwt_catch_fail
            (fun () ->
              Lwt_io.(with_file ~mode:Input path read) >>= fun contents ->
              lwt_ok @@
                Response { contents;
                           content_type = "application/octet-stream";
                           caching = Nocache })
            (fun e -> (`Not_found, Printexc.to_string e))

      | Api.Students_list token ->
          verify_teacher_token token >?= fun () ->
          Student.Index.get ()
          >>= respond_json cache
      | Api.Set_students_list (token, students) ->
          verify_teacher_token token >?= fun () ->
          Lwt_list.map_s
            (fun (ancestor, ours) ->
               let token = ancestor.Student.token in
               Student.get token >|= fun theirs ->
               let theirs = match theirs with
                 | None -> Student.default token
                 | Some std -> std
               in
               Student.three_way_merge ~ancestor ~theirs ~ours)
            students >>=
          Student.Index.set
          >>= respond_json cache
      | Api.Students_csv (token, exercises, students) ->
          verify_teacher_token token >?= fun () ->
          (match students with
           | [] -> Token.Index.get () >|= List.filter Token.is_student
           | l -> Lwt.return l)
          >>= Lwt_list.map_p (fun token ->
              Save.get token >|= fun save -> token, save)
          >>= fun tok_saves ->
          let all_exercises =
            match exercises with
            | [] ->
                List.fold_left (fun acc (_tok, save) ->
                    match save with
                    | None -> acc
                    | Some save ->
                        SMap.fold (fun ex_id _ans acc -> SSet.add ex_id acc)
                          save.Save.all_exercise_states
                          acc)
                  SSet.empty tok_saves
                |> SSet.elements
            | exercises -> exercises
          in
          let columns =
            "token" :: "nickname" ::
            (List.fold_left (fun acc ex_id ->
                 (ex_id ^ " grade") ::
                 (ex_id ^ " date") ::
                 acc)
                [] (List.rev all_exercises))
          in
          let buf = Buffer.create 3497 in
          let sep () = Buffer.add_char buf ',' in
          let line () = Buffer.add_char buf '\n' in
          Buffer.add_string buf (String.concat "," columns);
          line ();
          Lwt_list.iter_s (fun (tok, save) ->
              match save with None -> Lwt.return_unit | Some save ->
                Buffer.add_string buf (Token.to_string tok);
                sep ();
                Buffer.add_string buf save.Save.nickname;
                Lwt_list.iter_s (fun ex_id ->
                    Lwt.catch (fun () ->
                        sep ();
                        Exercise.get ex_id >>= fun exo ->
                        Lwt.wrap2 SMap.find ex_id save.Save.all_exercise_states
                        >|= fun st ->
                        (match st.Answer.grade with
                         | None -> ()
                         | Some grade ->
                             if match st.Answer.report with
                               | None -> false
                               | Some rep -> check_report exo rep grade
                             then Buffer.add_string buf (string_of_int grade)
                             else Printf.bprintf buf "CHEAT(%d)" grade);
                        sep ();
                        Buffer.add_string buf (string_of_date st.Answer.mtime))
                      (function
                        | Not_found -> sep (); Lwt.return_unit
                        | e -> raise e))
                  all_exercises
                >|= line)
            tok_saves
          >>= fun () ->
          lwt_ok @@
            Response {contents = Buffer.contents buf;
                      content_type = "text/csv";
                      caching = Nocache}

      | Api.Exercise_index (Some token) ->
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
          >>= respond_json cache
      | Api.Exercise_index None ->
         lwt_fail (`Forbidden, "Forbidden")

      | Api.Exercise (Some token, id, js) ->
          (Exercise.Status.is_open id token >>= function
          | `Open | `Deadline _ as o ->
              Exercise.Meta.get id >>= fun meta ->
              Exercise.get id >>= fun ex ->
              let ex = Learnocaml_exercise.strip js ex in
              respond_json cache
                (meta, ex,
                 match o with `Deadline t -> Some (max t 0.) | `Open -> None)
          | `Closed ->
             lwt_fail (`Forbidden, "Exercise closed"))
      | Api.Exercise (None, _, _) ->
         lwt_fail (`Forbidden, "Forbidden")

      | Api.Lesson_index () ->
          Lesson.Index.get () >>= respond_json cache
      | Api.Lesson id ->
         Lesson.get id >>= respond_json cache

      | Api.Tutorial_index () ->
          Tutorial.Index.get () >>= respond_json cache
      | Api.Tutorial id ->
         Tutorial.get id >>= respond_json cache

      | Api.Playground_index () ->
          Playground.Index.get () >>= respond_json cache
      | Api.Playground id ->
          Playground.get id >>= respond_json cache

      | Api.Exercise_status_index token ->
          verify_teacher_token token >?= fun () ->
          Exercise.Status.all () >>= respond_json cache
      | Api.Exercise_status (token, id) ->
          verify_teacher_token token >?= fun () ->
          Exercise.Status.get id >>= respond_json cache
      | Api.Set_exercise_status (token, status) ->
          verify_teacher_token token >?= fun () ->
          Lwt_list.iter_s
            Exercise.Status.(fun (ancestor, ours) ->
                get ancestor.id >>= fun theirs ->
                set (three_way_merge ~ancestor ~theirs ~ours))
            status
          >>= respond_json cache

      | Api.Partition (token, eid, fid, prof) ->
         lwt_catch_fail (fun () ->
           verify_teacher_token token
           >?= fun () ->
           Learnocaml_partition_create.partition eid fid prof
           >>= respond_json cache
           )
           (fun exn -> (`Not_found, Printexc.to_string exn))

      | Api.Invalid_request body ->
          lwt_fail (`Bad_request, body)

  let callback: type resp. Conduit.endp ->
                           Learnocaml_data.Server.config ->
                           resp Api.request -> resp ret
  = fun conn config req ->
    let cache = caching req in
    let respond () =
      Lwt.catch
        (fun () -> callback_raw conn config cache req)
        (function
          | Not_found ->
              lwt_fail (`Not_found, "Component not found")
          | e -> raise e)
    in
    match cache with
    | Nocache | Shortcache None -> respond ()
    | Longcache key | Shortcache (Some key) ->
        match Memory_cache.get key with
        | Some c -> lwt_ok (Cached c)
        | None -> respond ()

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

let last_modified = (* server startup time *)
  let open Unix in
  let tm = gmtime (gettimeofday ()) in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    (match tm.tm_wday with
     | 0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed"
     | 4 -> "Thu" | 5 -> "Fri" | 6 -> "Sat"
     | _ -> assert false)
    tm.tm_mday
    (match tm.tm_mon with
     | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May"
     | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct"
     | 10 -> "Nov" | 11 -> "Dec" | _ -> assert false)
    (tm.tm_year + 1900)
    tm.tm_hour tm.tm_min tm.tm_sec

(* Taken from the source of "decompress", from bin/easy.ml *)
let compress ?(level = 4) data =
  let input_buffer = Bytes.create 0xFFFF in
  let output_buffer = Bytes.create 0xFFFF in

  let pos = ref 0 in
  let res = Buffer.create (String.length data) in

  Lwt_preemptive.detach
    (Decompress.Zlib_deflate.bytes
       input_buffer
       output_buffer
       (fun input_buffer -> function
          | Some max ->
              let n = min max (min 0xFFFF (String.length data - !pos)) in
              Bytes.blit_string data !pos input_buffer 0 n;
              pos := !pos + n;
              n
          | None ->
              let n = min 0xFFFF (String.length data - !pos) in
              Bytes.blit_string data !pos input_buffer 0 n;
              pos := !pos + n;
              n)
       (fun output_buffer len ->
          Buffer.add_subbytes res output_buffer 0 len;
          0xFFFF))
    (Decompress.Zlib_deflate.default ~witness:Decompress.B.bytes level)
  >>= function
  | Ok _ -> Lwt.return (Buffer.contents res)
  | Error _ -> Lwt.fail_with "Could not compress"

let launch () =
  Random.self_init () ;
  Learnocaml_store.Server.get () >>= fun config ->
  let callback conn req body =
    let uri = Request.uri req in
    let path =
        Uri.path uri |>
        Uri.pct_decode |> (* %-decoding must happen before `/`-splitting *)
        Stringext.split ~on:'/'
    in
    let path =
      let rec clean = function
        | [] | [_] as l -> l
        | ""::l -> clean l
        | s::l -> s::clean l
      in
      clean path
    in
    let query = Uri.query uri in
    let args = List.map (fun (s, l) -> s, String.concat "," l) query in
    let use_compression =
      List.exists (function _, Cohttp.Accept.Deflate -> true | _ -> false)
        (Cohttp.Header.get_acceptable_encodings req.Request.headers)
    in
    let respond = function
      | Response {contents=body; content_type; caching; _}
      | Cached {body; content_type; caching; _} as resp ->
          let headers = Cohttp.Header.init_with "Content-Type" content_type in
          let headers = match caching with
            | Longcache _ ->
                Cohttp.Header.add headers
                  "Cache-Control" "public, immutable, max-age=2592000"
                  (* 1 month *)
            | Shortcache _ ->
                Cohttp.Header.add_list headers [
                  "Last-Modified", last_modified;
                  "Cache-Control", "private, must-revalidate";
                ]
            | Nocache ->
                Cohttp.Header.add headers "Cache-Control" "no-cache"
          in
          let resp = match resp, caching with
            | Response _, (Longcache key | Shortcache (Some key)) ->
                let cached =
                  {body; deflated_body = None; content_type; caching}
                in
                Memory_cache.add key cached;
                Cached cached
            | _ -> resp
          in
          Lwt.try_bind (fun () ->
              if use_compression && String.length body >= 1024 &&
                 match String.split_on_char '/' content_type with
                 | "text"::_
                 | "application" :: ("javascript" | "json") :: _
                 | "image" :: ("gif" | "svg+xml") :: _ -> true
                 | _ -> false
              then
                (match resp with
                 | Cached {deflated_body = Some s; _} -> Lwt.return s
                 | Cached
                     ({deflated_body = None;
                       caching = Longcache key | Shortcache Some key;
                       _ } as c) ->
                     compress body >|= fun s ->
                     Memory_cache.add key {c with deflated_body = Some s};
                     s
                 | _ -> compress body) >|= fun s ->
                Cohttp.Header.add headers "Content-Encoding" "deflate", s
              else
                Lwt.return (headers, body))
            (fun (headers, str) ->
               Server.respond_string ~headers ~status:`OK ~body:str ())
            (fun e ->
              Server.respond_error ~status:`Internal_server_error
                ~body:(Printexc.to_string e) ())
    in
    if Cohttp.Header.get req.Request.headers "If-Modified-Since" =
       Some last_modified
    then Server.respond ~status:`Not_modified ~body:Cohttp_lwt.Body.empty ()
    else
    (match req.Request.meth with
     | `GET -> lwt_ok {Api.meth = `GET; path; args}
     | `POST ->
        begin
          string_of_stream (Cohttp_lwt.Body.to_stream body)
          >>= function
          | Some s -> lwt_ok {Api.meth = `POST s; path; args}
          | None -> lwt_fail (`Bad_request, "Missing POST body")
        end
     | _ -> lwt_fail (`Bad_request, "Unsupported method"))
    >?= (fun req ->
      log conn req;
      Api_server.handler (Conduit_lwt_unix.endp_of_flow (fst conn)) config req)
    >>= function
    | Error (code,body) ->
       Server.respond_error ~status:code ~body ()
    | Ok response -> respond response
  in
  let mode =
    match !cert_key_files with
    | None -> (`TCP (`Port !port))
    | Some (crt, key) ->
        `TLS (`Crt_file_path crt, `Key_file_path key, `No_password, `Port !port)
  in
  init_teacher_token () >>= fun () ->
  Lwt.catch (fun () ->
      Server.create
        ~on_exn: (function
            | Unix.Unix_error(Unix.EPIPE, "write", "") -> ()
            | exn -> raise exn)
        ~mode (Server.make ~callback ()) >>= fun () ->
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
