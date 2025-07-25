(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Learnocaml_store
open Learnocaml_auth

let port = ref 8080

let cert_key_files = ref None

let log_channel = ref (Some stdout)

let base_url = ref ""

let args = Arg.align @@
  [ "-static-dir", Arg.Set_string static_dir,
    "PATH where static files should be found (./www)" ;
    "-sync-dir", Arg.Set_string sync_dir,
    "PATH where sync tokens are stored (./sync)" ;
    "-base-url", Arg.Set_string base_url,"BASE_URL of the website. \
     Should not end with a trailing slash. \
     Currently, this has no effect on the native backend. \
     Mandatory for 'learn-ocaml build' if the site is not hosted in path '/', \
     which typically occurs for static deployment." ;
    "-port", Arg.Set_int port,
    "PORT the TCP port (8080)" ]

open Lwt.Infix

type kind = Exercise of string*bool | Lesson of string*bool | Playground of string*bool | Toplevel

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
  cookies: Cohttp.Cookie.Set_cookie_hdr.t list
}

type 'a response =
  | Response of { contents: 'a;
                  content_type: string;
                  caching: caching;
                  cookies: Cohttp.Cookie.Set_cookie_hdr.t list }
  | Redirect of { code: Cohttp.Code.status_code;
                  url: string;
                  cookies: Cohttp.Cookie.Set_cookie_hdr.t list }
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

let respond_static ?(cookies=[]) caching path =
  lwt_catch_fail
    (fun () ->
       read_static_file path >>= fun contents ->
       let content_type =
         Magic_mime.lookup (List.fold_left (fun _ r -> r) "" path)
       in
       lwt_ok @@ Response { contents; content_type; caching; cookies })
    (fun e -> (`Not_found, Printexc.to_string e))

let respond_json ?(cookies=[]) caching contents =
  lwt_ok @@
    Response { contents;
               content_type = "application/json";
               caching;
               cookies}

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

let generate_csrf_token length =
  let random_bytes = Bytes.make length '\000' in
  Cryptokit.Random.secure_rng#random_bytes random_bytes 0 length;
  Base64.encode (Bytes.to_string random_bytes)


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
    | (Redirect _) as r -> lwt_ok r
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

  let wrap_user_session session f =
    Session.get_user_token session >>= function
    | Some token -> f token
    | None ->  Lwt.fail_with "Invalid session"

  let callback_raw: type resp. Conduit.endp -> Learnocaml_data.Server.config ->
                    caching -> Api.http_request -> resp Api.request ->
                    (resp response, error) result Lwt.t
    = let module ServerData = Learnocaml_data.Server in
      fun conn config cache req -> function
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
      | Api.Create_teacher_token_s (session, nick) ->
         wrap_user_session session @@ fun token ->
         verify_teacher_token token
         >?= fun () ->
         Token.create_teacher ()
         >>= fun tok ->
         (match nick with | None -> Lwt.return_unit
                          | Some nickname ->
                              Save.set tok Save.{empty with nickname})
         >>= fun () -> respond_json cache tok
      | Api.Launch body when config.ServerData.use_lti ->
         (* 32 bytes of entropy, same as RoR as of 2020. *)
         let csrf_token =  match generate_csrf_token 32 with
           | Ok tok -> tok
           | Error (`Msg msg) -> failwith msg in
         let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                          ~expiration:(`Max_age (Int64.of_int 3600))
                          ~path:"/" ~http_only:true
                          ("csrf", csrf_token)] in
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) in
         let launch_url =
          if req.Api.host = "" then
           "http://localhost:8080/launch"
          else
           req.Api.host ^ "/launch"
         in
         NonceIndex.get_current_secret () >>= fun secret ->
         LtiAuth.check_oauth launch_url params secret >>=
           (function
            | Ok id ->
               LtiAuth.get_token id >>= (function
               |Ok token ->
                 let session = Session.gen_session () in
                 Session.set_session session token >>= fun () ->
                 let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                                  ~expiration:(`Max_age (Int64.of_int 60))
                                  ~path:"/"
                                  ("session", Session.to_string session)] in
                 let rank = function Exercise (_,_) -> 0 | Lesson (_,_) -> 1 | Playground (_,_) -> 2 | Toplevel -> 3 in
                 let sort_from_rank l = List.sort (fun (k1) (k2) -> rank k1 - rank k2) l in
                 (*sort_from_rank [(Lesson, "first"); (Exercise, "demo"); (Playground, "editor")]*)
                 let ex_exist exo = Exercise.Index.get () >>= fun exercises ->
                                    let find_exercises_names contents = match contents with
                                      | Learnocaml_data.Exercise.Index.Groups _ -> failwith "erreur find_exercises_names"
                                      | Learnocaml_data.Exercise.Index.Exercises exos -> List.map fst exos in
                                    let find_names exs = List.map
                                                           (fun group -> find_exercises_names (snd group).Learnocaml_data.Exercise.Index.contents)
                                                           exs in
                                    let names = match exercises with
                                      | Learnocaml_data.Exercise.Index.Groups exs -> List.concat (find_names exs)
                                      | Learnocaml_data.Exercise.Index.Exercises _ -> [] in
                                    Lwt.return (List.exists (fun name -> name = exo) names) in
                 let play_exist play = Playground.Index.get () >>= fun playgrounds ->
                                       let find_names exs = List.map
                                                              (fun group -> (fst group))
                                                              exs in
                                       let names = find_names playgrounds in
                                       Lwt.return (List.exists (fun name -> name = play) names) in
                 let less_exist less = Lesson.Index.get () >>= fun lessons ->
                                       let find_names exs = List.map
                                                              (fun group -> (fst group))
                                                              exs in
                                       let names = find_names lessons in
                                       Lwt.return (List.exists (fun name -> name = less) names) in
                 let list_redirections l =
                   Lwt_list.fold_left_s (fun r (kind, id) ->
                                       match kind with
                                       | "custom_exercise" -> ex_exist id >|= fun ok ->
                                                              Exercise (id,ok) :: r
                                       | "custom_playground" -> play_exist id >|= fun ok ->
                                                                Playground (id, ok) :: r
                                       | "custom_lesson" -> less_exist id >|= fun ok ->
                                                            Lesson (id, ok) :: r
                                       | "custom_toplevel" -> Lwt.return (Toplevel :: r)
                                       | _ -> Lwt.return r
                     ) [] l
                 in
                 let return_url kind_url = match kind_url with
                   | Exercise (id,ok) -> if ok
                                         then "/exercises/"^id^"/#tab%3Dtext"
                                         else "/redirection?kind=exercise&id="^id
                   | Playground (id,ok) -> if ok
                                           then "/playground/"^id^"/#tab%3Dtoplevel"
                                           else "/redirection?kind=playground&id="^id
                   | Lesson (id,ok) -> if ok
                                       then "/#activity%3Dlessons%26lesson%3D"^id
                                       else "/redirection?kind=lesson&id="^id
                   | Toplevel -> "/#activity%3Dtoplevel"
                 in
                 let return_url_many kind_url = match kind_url with
                   | Exercise (id,ok) -> if ok
                                         then "/redirection?kind=exercise&id="^id^"&many=true"
                                         else "/redirection?kind=exercise&id="^id
                   | Playground (id,ok) -> if ok
                                           then "/redirection?kind=playground&id="^id^"&many=true"
                                           else "/redirection?kind=playground&id="^id
                   | Lesson (id,ok) -> if ok
                                       then "/redirection?kind=lesson&id="^id^"&many=true"
                                       else "/redirection?kind=lesson&id="^id
                   | _ -> "/" in
                 let redirection l = match sort_from_rank l with
                     [] -> "/"
                   | [url] -> return_url url
                   | url :: _ -> return_url_many url in
                 list_redirections params >>= fun list ->
                 lwt_ok @@ Redirect { code=`See_other; url= !base_url^(redirection list); cookies }
               |Error _ ->
                 NonceIndex.get_current_secret () >>= fun secret ->
                 let hmac = generate_hmac secret csrf_token id in
                 read_static_file ["lti.html"] >>= fun s ->
                 let contents =
                   Markup.string s
                   |> Markup.parse_html
                   |> Markup.signals
                   |> Markup.map (function
                          | `Start_element ((e, "input"), attrs) as elt ->
                             (match List.assoc_opt ("", "type") attrs,
                                    List.assoc_opt ("", "name") attrs with
                              | Some "hidden", Some "csrf" ->
                                 `Start_element ((e, "input"), (("", "value"), csrf_token) :: attrs)
                              | Some "hidden", Some "user-id" ->
                                 `Start_element ((e, "input"), (("", "value"), id) :: attrs)
                              | Some "hidden", Some "hmac" ->
                                 `Start_element ((e, "input"), (("", "value"), hmac) :: attrs)
                              | _ -> elt)
                          | t -> t)
                   |> Markup.pretty_print
                   |> Markup.write_html
                   |> Markup.to_string in
                 lwt_ok @@ Response { contents; content_type="text/html"; caching=Nocache; cookies })
            | Error e -> lwt_fail (`Forbidden, e))
      | Api.Launch _ ->
         lwt_fail (`Forbidden, "LTI is disabled on this instance.")
      | Api.Associate body ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (k, vs) -> (k, String.concat "," vs)) in
         let target_method = List.assoc "target_method" params in
         let source_method = List.assoc "source_method" params in

         if target_method = source_method then
           lwt_fail (`Bad_request, "Cannot associate a method to itself")
         else (match source_method with
               |"token"->
                 let token = List.assoc "token" params in
                 Lwt.return (Ok (Token.parse token))
               |"email" ->
                 Lwt.return (Error "Not implemented yet")
               | _ -> Lwt.return (Error "Unknow method"))
              >>= (function
                   | Error msg -> lwt_fail (`Forbidden, msg)
                   | Ok token ->
                      let lwt_result =
                    (match target_method with
                     | "lti" when config.ServerData.use_lti ->
                        let csrf = List.assoc "csrf" params and
                            hmac = List.assoc "hmac" params and
                            user_id = List.assoc "user-id" params in
                          let creds = { LtiAuth.user_id; LtiAuth.token; LtiAuth.csrf; LtiAuth.hmac } in
                          LtiAuth.associate creds >>= (function
                            | Ok t -> Lwt.return (Ok (t, "lti", user_id))
                            | Error e -> Lwt.return (Error e))
                     | "lti" ->
                        Lwt.return (Error "LTI is disabled on this instance")
                     |"email" ->
                       Lwt.return (Error "Not implemented yet")
                     | _ ->
                        Lwt.return (Error "Unknown target_method"))
                     in
                     lwt_result >>= (function
                     | Error msg ->
                        lwt_fail (`Forbidden, msg)
                     | Ok (token, method_, value) ->
                        TokenIndex.add_association ~token ~method_ ~value >>= fun () ->
                        let session = Session.gen_session () in
                        Session.set_session session token >>= fun () ->
                        let make_cookie = Cohttp.Cookie.Set_cookie_hdr.make
                                            ~expiration:(`Max_age (Int64.of_int 60)) ~path:"/" in
                        let cookies = [make_cookie ("session", Session.to_string session);
                                       make_cookie ~http_only:true ("csrf", "expired")] in
                        lwt_ok @@ Redirect { code=`See_other; url="/"; cookies }))
      | Api.Login body ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (k, vs) -> (k, String.concat "," vs)) in
         begin match List.assoc_opt "method" params with
         | Some "token" ->
            begin match List.assoc_opt "token" params with
            | Some t ->
               let token = Token.parse t in
               Token.exists token >>= fun exists ->
               if exists then (
                 let session = Session.gen_session () in
                 Session.set_session session token >>= fun () ->
                 respond_json cache session
               ) else
                 lwt_fail (`Forbidden, "Invalid token")
            | None ->
               lwt_fail (`Bad_request, "Missing 'token' parameter")
            end
         | Some "email" ->
            lwt_fail (`Forbidden, "Not implemented yet")
         | Some m -> lwt_fail (`Bad_request, "Unknown login method: " ^ m)
         | None -> lwt_fail (`Bad_request, "Missing 'method' parameter")
         end
      | Api.Register body ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (k, vs) -> (k, String.concat "," vs)) in
         let token =
           (match List.assoc_opt "method" params with
            | Some "lti" ->
               let user_id = List.assoc "user-id" params and
                   csrf = List.assoc "csrf" params and
                   hmac = List.assoc "hmac" params and
                   nickname = List.assoc "nick" params in
               let creds = { LtiAuth.user_id; LtiAuth.nickname; LtiAuth.csrf; LtiAuth.hmac } in
               LtiAuth.register creds >>=
                 (function
                  | Ok t -> Lwt.return (Ok (t,"lti",user_id))
                  | Error e -> Lwt.return (Error e))
            | Some "email" ->
               Lwt.return (Error "Not implemented yet")
            | Some m ->
               Lwt.return (Error ("Unknown login method: " ^ m))
            |None -> Lwt.return (Error "Missing 'method' parameter"))
         in
         token >>= (function
         | Ok  (token, method_, value) ->
            TokenIndex.add_association ~token ~method_ ~value >>= fun () ->
            let session = Session.gen_session () in
            Session.set_session session token >>= fun () ->
            let make_cookie = Cohttp.Cookie.Set_cookie_hdr.make
                                ~expiration:(`Max_age (Int64.of_int 60)) ~path:"/" in
            let cookies = [make_cookie ("session", Session.to_string session);
                           make_cookie ~http_only:true ("csrf", "expired")] in
            lwt_ok @@ Redirect { code=`See_other; url="/"; cookies }
         | Error msg ->
            lwt_fail (`Forbidden, msg))
      | Api.Fetch_save token ->
         lwt_catch_fail
           (fun () ->
             Save.get token >>= fun tokopt ->
             lwt_option_fail
               tokopt
               (`Not_found, "token not found")
               (respond_json cache))
         (fun exn -> (`Internal_server_error, Printexc.to_string exn))
      | Api.Fetch_save_s session ->
         wrap_user_session session @@ fun token ->
         lwt_catch_fail
           (fun () ->
             Save.get token >>= fun tokopt ->
             lwt_option_fail
               tokopt
               (`Not_found, "token not found")
               (respond_json cache))
           (fun exn -> (`Internal_server_error, Printexc.to_string exn))
      | Api.Get_token session ->
         wrap_user_session session @@ fun token ->
          respond_json cache token
      | Api.Archive_zip token ->
          let open Lwt_process in
          let path = Filename.concat !sync_dir (Token.to_path token) in 
          let cmd = shell ("git archive master --format=zip -0 --remote="^path)
          and stdout = `FD_copy Unix.stdout in
          Lwt_process.pread ~stdin:stdout cmd >>= fun contents ->
          lwt_ok @@ Response { contents = contents;
                               content_type = "application/zip";
                               caching = Nocache;
                               cookies = [] }
      | Api.Archive_zip_s session ->
          let open Lwt_process in
          wrap_user_session session @@ fun token ->
          let path = Filename.concat !sync_dir (Token.to_path token) in 
          let cmd = shell ("git archive master --format=zip -0 --remote="^path)
          and stdout = `FD_copy Unix.stdout in
          Lwt_process.pread ~stdin:stdout cmd >>= fun contents ->
          lwt_ok @@ Response { contents = contents;
                               content_type = "application/zip";
                               caching = Nocache;
                               cookies = [] }
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
      | Api.Update_save_s (session, save) ->
          wrap_user_session session @@ fun token ->
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
                           caching = Nocache;
                               cookies = [] })
            (fun e -> (`Not_found, Printexc.to_string e))

      | Api.Students_list token ->
          verify_teacher_token token >?= fun () ->
          Student.Index.get ()
          >>= respond_json cache
      | Api.Students_list_s session ->
          wrap_user_session session @@ fun token ->
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
      | Api.Set_students_list_s (session, students) ->
          wrap_user_session session @@ fun token ->
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
                      caching = Nocache;
                      cookies = [] }
      | Api.Students_csv_s (session, exercises, students) ->
          wrap_user_session session @@ fun token ->
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
                      caching = Nocache;
                      cookies = [] }

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
      | Api.Exercise_index_s (Some session) ->
          wrap_user_session session @@ fun token ->
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
      | Api.Exercise_index_s None ->
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
      | Api.Exercise_s (Some session, id, js) ->
          wrap_user_session session @@ fun token ->
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
      | Api.Exercise_s (None, _, _) ->
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
      | Api.Exercise_status_index_s session ->
          wrap_user_session session @@ fun token ->
          verify_teacher_token token >?= fun () ->
          Exercise.Status.all () >>= respond_json cache
      | Api.Exercise_status (token, id) ->
          verify_teacher_token token >?= fun () ->
          Exercise.Status.get id >>= respond_json cache
      | Api.Exercise_status_s (session, id) ->
          wrap_user_session session @@ fun token ->
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
      | Api.Set_exercise_status_s (session, status) ->
          wrap_user_session session @@ fun token ->
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
      | Api.Partition_s (session, eid, fid, prof) ->
          wrap_user_session session @@ fun token ->
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
                           Api.http_request ->
                           resp Api.request -> resp ret
  = fun conn config http_req req ->
    let cache = caching req in
    let respond () =
      Lwt.catch
        (fun () -> callback_raw conn config cache http_req req)
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

module Api_server = Api.Server (Api.Json_codec) (Request_handler)

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

(* Adapted from the source of "decompress.1.5.3", from bin/decompress.ml *)
let compress ?(level = 4) data =
  let bigstring_output o off len buf =
    let res = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set res i o.{off + i}
    done
    ; Buffer.add_bytes buf res in
  let src_len   = String.length data in
  let dst_bound = max (De.Def.Ns.compress_bound src_len) De.io_buffer_size in
  let o = De.bigstring_create dst_bound in
  (* buffer.mli: nothing bad will happen if the buffer grows beyond that limit: *)
  let buf = Buffer.create dst_bound in
  (* de.mli: we recommend a queue as large as output buffer: *)
  let q = De.Queue.create De.io_buffer_size in
  (* LZ77 with a 32.kB sliding-window compression: *)
  let w = De.Lz77.make_window ~bits:15 in
  let open Zl in
  let encoder = Def.encoder (`String data) `Manual ~q ~w ~level in
  let rec go encoder =
    match Def.encode encoder with
    | `Await _encoder ->
       Error "Zl.Def.encode: could not compress"
    | `Flush encoder ->
       let len = De.io_buffer_size - Def.dst_rem encoder in
       bigstring_output o 0 len buf
       ; Def.dst encoder o 0 De.io_buffer_size |> go
    | `End encoder ->
       let len = De.io_buffer_size - Def.dst_rem encoder in
       if len > 0 then bigstring_output o 0 len buf
       ; Ok (Buffer.contents buf) in
  Lwt_preemptive.detach
    (fun () ->
      Def.dst encoder o 0 De.io_buffer_size |> go)
    ()
  >>= function
  | Ok str -> Lwt.return str
  | Error e -> Lwt.fail_with e

let launch () =
  Random.self_init () ;
  NonceIndex.get_first_oauth () >>= fun (secret, _) ->
  Lwt_io.printf "LTI shared secret: %s\n" secret
  >>= fun () ->
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
      | Response {contents=body; content_type; caching; cookies; _}
      | Cached {body; content_type; caching; cookies; _} as resp ->
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
          let cookies_hdr = List.rev_map Cohttp.Cookie.Set_cookie_hdr.serialize cookies in
          let headers = Cohttp.Header.add_list headers cookies_hdr in
          let resp = match resp, caching with
            | Response _, (Longcache key | Shortcache (Some key)) ->
                let cached =
                  {body; deflated_body = None; content_type; caching; cookies = []}
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
      | Redirect { code; url; cookies } ->
         let headers = Cohttp.Header.init_with "Location" url in
         let cookies_hdr = List.rev_map Cohttp.Cookie.Set_cookie_hdr.serialize cookies in
         let headers = Cohttp.Header.add_list headers cookies_hdr in
         Server.respond_string ~headers ~status:code ~body:"" ()
    in
    if Cohttp.Header.get req.Request.headers "If-Modified-Since" =
       Some last_modified
    then Server.respond ~status:`Not_modified ~body:Cohttp_lwt.Body.empty ()
    else
    (match req.Request.meth with
     | `GET -> lwt_ok {Api.meth = `GET; host = !base_url; path; args}
     | `POST ->
        begin
          Cohttp_lwt.Body.to_string body
          >>= fun params ->
          let param_list = Uri.query_of_encoded params in
          if param_list = [] then
            lwt_fail (`Bad_request, "Missing POST body")
          else
            let cookies = Cohttp.Cookie.Cookie_hdr.extract req.Request.headers in
            match List.assoc_opt "csrf" param_list,
                  List.assoc_opt "csrf" cookies with
            | Some (param_csrf :: _), Some cookie_csrf ->
               if Eqaf.equal param_csrf cookie_csrf then
                 lwt_ok {Api.meth = `POST params; host = !base_url; path; args}
               else
                 lwt_fail (`Forbidden, "CSRF token mismatch")
            | None, None | None, Some _ ->
               lwt_ok {Api.meth = `POST params; host = !base_url; path; args}
            | _, _ ->
               lwt_fail (`Forbidden, "Bad CSRF token")
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

let check_running () =
  try
    let ic = Printf.ksprintf Unix.open_process_in "lsof -ti tcp:%d -s tcp:LISTEN" !port in
    let pid = match input_line ic with
      | "" -> None
      | s -> int_of_string_opt s
      | exception End_of_file -> None
    in
    close_in ic;
    pid
  with Unix.Unix_error _ ->
    Printf.eprintf "Warning: could not check for previously running instance";
    None

let kill_running pid =
  let timeout = 15 in
  Unix.kill pid Sys.sigint;
  Printf.eprintf "Waiting for process %d to terminate... %2d %!" pid timeout;
  let rec aux tout =
    Printf.eprintf "\027[3D%2d %!" tout;
    if Printf.ksprintf Sys.command "lsof -ti tcp:%d -p %d >/dev/null" !port pid
       = 0
    then
      if tout <= 0 then
        (prerr_endline "Error: process didn't terminate in time"; exit 10)
      else
        (Unix.sleep 1;
         aux (tout - 1))
  in
  aux timeout;
  prerr_endline "\027[3Dok "
