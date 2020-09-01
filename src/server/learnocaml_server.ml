(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Learnocaml_store

let check_email_ml email =
  let regexp = Str.regexp Learnocaml_data.email_regexp_ml in
  Learnocaml_data.email_check_length email
  && Str.string_match regexp email 0

let port = ref 8080

let cert_key_files = ref None

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

let caching: type resp. resp Api.request -> caching = function
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
               cookies }

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
  B64.encode (Bytes.to_string random_bytes)

let generate_hmac secret csrf user_id =
  let decoder = Cryptokit.Hexa.decode () in
  let secret = Cryptokit.transform_string decoder secret in
  let hmac = Cryptokit.MAC.hmac_sha256 secret and
      encoder = Cryptokit.Hexa.encode () in
  Cryptokit.hash_string hmac (csrf ^ user_id)
  |> Cryptokit.transform_string encoder

let create_student conn (config: Learnocaml_data.Server.config) cache req
      nonce_req secret_candidate nick base_auth =
  let module ServerData = Learnocaml_data.Server in
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
       (match nick with
        | None -> Lwt.return_unit
        | Some nickname -> Save.set tok Save.{empty with nickname})
       >>= fun () ->
       (match base_auth with
        | `Token use_moodle ->
           Lwt.return (Token_index.Token (tok, use_moodle))
        | `Password (email, password) ->
           Token_index.UpgradeIndex.change_email !sync_dir tok >|= (fun handle ->
            Learnocaml_sendmail.confirm_email
              ~nick
              ~url:(req.Api.host ^ "/confirm/" ^ handle)
              email;
            Token_index.Password (tok, email, password, Some(email)))) >>= fun auth ->
       Token_index.UserIndex.add !sync_dir auth >>= fun () ->
       respond_json cache tok

(** [get_nickname] is used to show the user name in emails openings.
    (Cost some filesystem read; we might want to always return None) *)
let get_nickname token =
  Save.get token >>= function
    | None -> Lwt.return_none
    | Some save -> Lwt.return_some save.Save.nickname

let initiate_password_change token address cache req =
  Token_index.UpgradeIndex.reset_password !sync_dir token >>= fun handle ->
  get_nickname token >>= fun nick ->
  Learnocaml_sendmail.reset_password
    ~nick
    ~url:(req.Api.host ^ "/reset_password/" ^ handle)
    address;
  respond_json cache address

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

  let callback_raw: type resp. Conduit.endp -> Learnocaml_data.Server.config ->
                    caching -> Api.http_request -> resp Api.request ->
                    (resp response, error) result Lwt.t
    = let module ServerData = Learnocaml_data.Server in
      fun conn config cache req -> function
      | Api.Version () ->
         respond_json cache (Api.version, config.ServerData.server_id)
      | Api.Launch body when config.ServerData.use_moodle ->
         (* 32 bytes of entropy, same as RoR as of 2020. *)
         let csrf_token = generate_csrf_token 32 in
         let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                          ~expiration:(`Max_age (Int64.of_int 3600))
                          ~path:"/" ~http_only:true
                          ("csrf", csrf_token)] in
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) in
         Token_index.check_oauth !sync_dir (req.Api.host ^ "/launch") params >>=
           (function
            | Ok id ->
               Token_index.MoodleIndex.user_exists !sync_dir id >>= fun exists ->
               if exists then
                 Token_index.MoodleIndex.get_user_token !sync_dir id >>= fun token ->
                 let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                                  ~expiration:(`Max_age (Int64.of_int 60))
                                  ~path:"/"
                                  ("token", Token.to_string token)] in
                 lwt_ok @@ Redirect { code=`See_other; url="/"; cookies }
               else
                 Token_index.OauthIndex.get_current_secret !sync_dir >>= fun secret ->
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
                 lwt_ok @@ Response { contents; content_type="text/html"; caching=Nocache; cookies }
            | Error e -> lwt_fail (`Forbidden, e))
      | Api.Launch_login body when config.ServerData.use_moodle ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) in
         let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                          ~expiration:(`Max_age (Int64.of_int 60))
                          ~path:"/" ~http_only:true
                          ("csrf", "expired")] in
         let email = List.assoc "email" params and
             passwd = List.assoc "passwd" params and
             user_id = List.assoc "user-id" params and
             csrf = List.assoc "csrf" params and
             hmac = List.assoc "hmac" params in
         Token_index.UserIndex.authenticate !sync_dir (Token_index.Passwd (email, passwd)) >>=
           (function
            | None -> lwt_fail (`Forbidden, "incorrect password")
            | Some token ->
               Token_index.OauthIndex.get_current_secret !sync_dir >>= fun secret ->
               let new_hmac = generate_hmac secret csrf user_id in
               if not (Eqaf.equal hmac new_hmac) then
                 lwt_fail (`Forbidden, "bad hmac")
               else
                 Token_index.MoodleIndex.user_exists !sync_dir user_id >>= fun exists ->
                 if exists then
                   (* This can only happen if the user launched twice
                      at the same time and completed the form twice,
                      but as the CSRF in the cookies has changed twice
                      (once for the second form, once for the
                      invalidation), this should not happen at all. *)
                   lwt_fail (`Forbidden, "user exists")
                 else
                   Token_index.MoodleIndex.add_user !sync_dir user_id token >>= fun () ->
                   let cookies = (Cohttp.Cookie.Set_cookie_hdr.make
                                    ~expiration:(`Max_age (Int64.of_int 60))
                                    ~path:"/"
                                    ("token", Token.to_string token)) :: cookies in
                   lwt_ok @@ Redirect { code=`See_other; url="/"; cookies })
      | Api.Launch_direct body when config.ServerData.use_moodle ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) and
             make_cookie = Cohttp.Cookie.Set_cookie_hdr.make
                             ~expiration:(`Max_age (Int64.of_int 60)) ~path:"/" in
         let user_id = List.assoc "user-id" params and
             csrf = List.assoc "csrf" params and
             hmac = List.assoc "hmac" params in
         Token_index.OauthIndex.get_current_secret !sync_dir >>= fun secret ->
         let new_hmac = generate_hmac secret csrf user_id in
         if not (Eqaf.equal hmac new_hmac) then
           lwt_fail (`Forbidden, "bad hmac")
         else
           Token.create_student () >>= fun token ->
           let auth = Token_index.Token (token, true) in
           Token_index.(
             TokenIndex.add_token !sync_dir token >>= fun () ->
             MoodleIndex.add_user !sync_dir user_id token >>= fun () ->
             UserIndex.add !sync_dir auth) >>= fun () ->
           let cookies = [make_cookie ("token", Token.to_string token);
                          make_cookie ~http_only:true ("csrf", "expired")] in
           lwt_ok @@ Redirect { code=`See_other; url="/"; cookies }
      | Api.Launch _ ->
         lwt_fail (`Forbidden, "LTI is disabled on this instance.")
      | Api.Launch_login _ ->
         lwt_fail (`Forbidden, "LTI is disabled on this instance.")
      | Api.Launch_direct _ ->
         lwt_fail (`Forbidden, "LTI is disabled on this instance.")
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
      | Api.Create_token _ when config.ServerData.use_passwd ->
         lwt_fail (`Forbidden, "Creating a raw token is forbidden on this instance.")
      | Api.Create_token (secret_candidate, None, nick) ->
         valid_string_of_endp conn
         >?= fun conn ->
         create_student conn config cache req nonce_req secret_candidate nick (`Token false)
      | Api.Create_token (_secret_candidate, Some token, _nick) ->
         lwt_catch_fail
            (fun () -> Token.register token >>= fun () ->
                       let auth = Token_index.Token (token, false) in
                       Token_index.UserIndex.add !sync_dir auth >>= fun () ->
                       respond_json cache token)
            (function
             | Failure body -> (`Bad_request, body)
             | exn -> (`Internal_server_error, Printexc.to_string exn))
      | Api.Create_teacher_token token ->
         verify_teacher_token token
         >?= fun () ->
         Token.create_teacher ()
         >>= respond_json cache
      | Api.Create_user (email, nick, password, secret) when config.ServerData.use_passwd ->
         valid_string_of_endp conn
         >?= fun conn ->
         Token_index.UserIndex.exists !sync_dir email >>= fun exists ->
         if exists then
           lwt_fail (`Forbidden, "User already exists")
         else if not (check_email_ml email) then
           lwt_fail (`Bad_request, "Invalid e-mail address")
         else if String.length password < 8 then
           lwt_fail (`Bad_request, "Password must be at least 8 characters long")
         else
           create_student conn config cache req nonce_req secret (Some nick) (`Password (email, password))
      | Api.Login (nick, password) when config.ServerData.use_passwd ->
         Token_index.UserIndex.authenticate !sync_dir (Token_index.Passwd (nick, password)) >>=
           (function
            | Some token -> respond_json cache token
            | _ ->
               Lwt.return (Printf.printf "[WARNING] Bad login or password for: %s\n%!" nick)
               >>= fun () ->
               lwt_fail (`Forbidden, "Bad login or password"))
      | Api.Create_user _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Login _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Can_login token ->
         Token_index.UserIndex.can_login !sync_dir token >>=
         respond_json cache

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
                      caching = Nocache;
                      cookies = []}

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

      | Api.Exercise (Some token, id) ->
          (Exercise.Status.is_open id token >>= function
          | `Open | `Deadline _ as o ->
              Exercise.Meta.get id >>= fun meta ->
              Exercise.get id >>= fun ex ->
              respond_json cache
                (meta, ex,
                 match o with `Deadline t -> Some (max t 0.) | `Open -> None)
          | `Closed ->
             lwt_fail (`Forbidden, "Exercise closed"))
      | Api.Exercise (None, _) ->
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

      | Api.Change_email (token, address) when config.ServerData.use_passwd ->
         Token_index.UserIndex.email_of_token !sync_dir token >>=
           (function
            | Some old_address ->
               Token_index.UserIndex.exists !sync_dir address >>= fun exists ->
               if exists then
                 lwt_fail (`Forbidden, "Address already in use.")
               else
                 Token_index.UserIndex.change_email !sync_dir token address >>= fun () ->
                 Token_index.UpgradeIndex.change_email !sync_dir token >>= fun handle ->
                 get_nickname token >>= fun nick ->
                 Learnocaml_sendmail.change_email
                   ~nick
                   ~url:(req.Api.host ^ "/confirm/" ^ handle)
                   old_address address;
                 respond_json cache ()
            | None -> lwt_fail (`Not_found, "Unknown user."))
      | Api.Confirm_email handle when config.ServerData.use_passwd ->
         Token_index.UpgradeIndex.can_change_email !sync_dir handle >>=
           (function
            | Some token ->
               Token_index.UserIndex.confirm_email !sync_dir token >>= fun () ->
               Token_index.UpgradeIndex.revoke_operation !sync_dir handle >>= fun () ->
               respond_static cache ["validate.html"]
            | None ->
               lwt_fail (`Forbidden, "Nothing to do."))
      | Api.Send_reset_password address when config.ServerData.use_passwd ->
         Token_index.UserIndex.token_of_email !sync_dir address >>=
           (function
            | Some token ->
               initiate_password_change token address cache req
            | None ->
               Lwt.return
                 (Printf.printf "[INFO] attempt to reset password for unknown email: %s\n%!"
                    address)
               >>= fun () ->
               respond_json cache address)
      | Api.Change_password token when config.ServerData.use_passwd ->
         Token_index.UserIndex.email_of_token !sync_dir token >>=
           (function
            | Some address ->
               initiate_password_change token address cache req
            | None -> lwt_fail (`Not_found, "Unknown user."))
      | Api.Reset_password handle when config.ServerData.use_passwd ->
         Token_index.UpgradeIndex.can_reset_password !sync_dir handle >>=
           (function
            | Some _token ->
               let csrf_token = generate_csrf_token 32 in
               let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                                ~expiration:(`Max_age (Int64.of_int 3600))
                                ~path:"/" ~http_only:true
                                ("csrf", csrf_token)] in
               read_static_file ["reset.html"] >>= fun s ->
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
                            | Some "hidden", Some "handle" ->
                               `Start_element ((e, "input"), (("", "value"), handle) :: attrs)
                            | _ -> elt)
                        | t -> t)
                 |> Markup.pretty_print
                 |> Markup.write_html
                 |> Markup.to_string in
               lwt_ok @@ Response { contents; content_type="text/html"; caching=Nocache; cookies }
            | None ->
               lwt_fail (`Forbidden, "Nothing to do."))
      | Api.Do_reset_password body when config.ServerData.use_passwd ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) in
         let handle = List.assoc "handle" params in
         Token_index.UpgradeIndex.can_reset_password !sync_dir handle >>=
           (function
            | Some token ->
               let passwd = List.assoc "passwd" params and
                   cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                                ~expiration:(`Max_age (Int64.of_int 60)) ~path:"/"
                                ~http_only:true ("csrf", "expired")] in
               if String.length passwd < 8 then
                 lwt_ok @@ Redirect { code=`See_other; url="/reset_password/" ^ handle; cookies }
               else
                 Token_index.UserIndex.update !sync_dir token passwd >>= fun () ->
                 Token_index.UpgradeIndex.revoke_operation !sync_dir handle >>= fun () ->
                 lwt_ok @@ Redirect { code=`See_other; url="/"; cookies }
            | None ->
               lwt_fail (`Forbidden, "Nothing to do."))

      | Api.Change_email _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Confirm_email _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Send_reset_password _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Change_password _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Reset_password _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Do_reset_password _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")

      | Api.Is_account token when config.ServerData.use_passwd ->
         Token_index.UserIndex.email_of_token !sync_dir token >>= fun email ->
         respond_json cache (email <> None)
      | Api.Is_account _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")

      | Api.Upgrade_form body when config.ServerData.use_passwd ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) in
         let token = Token.parse @@ List.assoc "token" params in
         Token_index.UserIndex.email_of_token !sync_dir token >>=
           (function
            | None ->
               let csrf_token = generate_csrf_token 32 in
               let cookies = [Cohttp.Cookie.Set_cookie_hdr.make
                                ~expiration:(`Max_age (Int64.of_int 3600))
                                ~path:"/" ~http_only:true ("csrf", csrf_token)] in
               read_static_file ["upgrade.html"] >>= fun s ->
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
                            | Some "hidden", Some "token" ->
                               `Start_element ((e, "input"), (("", "value"), Token.to_string token) :: attrs)
                            | _ -> elt)
                        | t -> t)
                 |> Markup.pretty_print
                 |> Markup.write_html
                 |> Markup.to_string in
               lwt_ok @@ Response { contents; content_type="text/html"; caching=Nocache; cookies }
            | Some _ -> lwt_fail (`Forbidden, "Already an account."))
      | Api.Upgrade body when config.ServerData.use_passwd ->
         let params = Uri.query_of_encoded body
                      |> List.map (fun (a, b) -> a, String.concat "," b) in
         let token = Token.parse @@ List.assoc "token" params in
         Token_index.UserIndex.email_of_token !sync_dir token >>=
           (function
            | None ->
               let make_cookie = Cohttp.Cookie.Set_cookie_hdr.make
                                   ~expiration:(`Max_age (Int64.of_int 60)) ~path:"/" in
               let cookies = [make_cookie ~http_only:true ("csrf", "expired")] and
                   email = List.assoc "email" params and
                   passwd = List.assoc "passwd" params in
               Token_index.UserIndex.exists !sync_dir email >>= fun exists ->
               if exists then lwt_fail (`Forbidden, "E-mail already used")
               else if String.length passwd < 8 || not (check_email_ml email) then
                 lwt_ok @@ Redirect { code=`See_other; url="/upgrade"; cookies }
               else
                 let cookies = make_cookie ("token", Token.to_string token) :: cookies in
                 Token_index.UserIndex.upgrade !sync_dir token email passwd >>= fun () ->
                 Token_index.UpgradeIndex.change_email !sync_dir token >>= fun handle ->
                 get_nickname token >>= fun nick ->
                 Learnocaml_sendmail.confirm_email
                   ~nick
                   ~url:(req.Api.host ^ "/confirm/" ^ handle)
                   email;
                 lwt_ok @@ Redirect { code=`See_other; url="/"; cookies }
            | Some _ -> lwt_fail (`Forbidden, "Already an account."))

      | Api.Upgrade_form _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")
      | Api.Upgrade _ ->
         lwt_fail (`Forbidden, "Users with passwords are disabled on this instance.")

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

module Api_server = Api.Server (Token_index.Json_codec) (Request_handler)

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

let get_base_url req =
  let uri = Request.uri req in
  match Uri.(scheme uri, host uri, port uri) with
  | Some ("http" as scheme), Some host, Some 80
  | Some ("https" as scheme), Some host, Some 443 ->
     Uri.to_string @@ Uri.make ~scheme ~host ()
  | Some scheme, Some host, Some port -> Uri.to_string @@ Uri.make ~scheme ~host ~port ()
  | _, Some host, Some 80 -> Uri.to_string @@ Uri.make ~scheme:("http") ~host ()
  | _, Some host, Some 443 -> Uri.to_string @@ Uri.make ~scheme:("https") ~host ()
  | _, Some host, Some port -> Uri.to_string @@ Uri.make ~scheme:("http") ~host ~port ()
  | _ -> failwith "Bad request"

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
  if config.Learnocaml_data.Server.use_moodle
     && not config.Learnocaml_data.Server.use_passwd then
    failwith "Cannot enable Moodle/LTI without enabling passwords."
  else if not config.Learnocaml_data.Server.use_passwd then
    print_endline "[INFO] You may want to enable passwords and LTI \
                   with the config options `use_passwd' and `use_moodle'."
  else if not config.Learnocaml_data.Server.use_moodle then
    print_endline "[INFO] You may want to enable LTI with the config \
                   option `use_moodle'.";
  let callback conn req body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    let path = Stringext.split ~on:'/' path in
    let path =
      let rec clean = function
        | [] | [_] as l -> l
        | ""::l -> clean l
        | s::l -> s::clean l
      in
      clean path
    in
    let path = List.map Uri.pct_decode path in
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
     | `GET -> lwt_ok {Api.meth = `GET; host = get_base_url req; path; args}
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
                 lwt_ok {Api.meth = `POST params; host = get_base_url req; path; args}
               else
                 lwt_fail (`Forbidden, "CSRF token mismatch")
            | None, None | None, Some _ ->
               lwt_ok {Api.meth = `POST params; host = get_base_url req; path; args}
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
