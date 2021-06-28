(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt
open Learnocaml_data

let ( / ) dir f = if dir = "" then f else Filename.concat dir f
let indexes_subdir = "data"

let logfailwith str arg =
  Printf.printf "[ERROR] %s (%s)\n%!" str arg;
  failwith str

let generate_random_hex len =
  Cryptokit.Random.string Cryptokit.Random.secure_rng len
  |> Cryptokit.transform_string @@ Cryptokit.Hexa.encode ()

module J = Json_encoding

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
    | `Bool v -> string_of_bool v
    | _ -> assert false
end

module type IndexRW = sig
  type t

  val init : unit -> t
  val read : t -> string -> (string -> 'a) -> 'a Lwt.t
  val write : t -> string -> ('a -> string) -> 'a -> unit Lwt.t
end

module IndexFile: IndexRW = struct
  type t = Lwt_mutex.t

  (* Unlocked by default *)
  let init = Lwt_mutex.create

  let read mutex filename parse =
    Lwt_mutex.with_lock mutex @@
      fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input filename @@
        fun channel ->
        Lwt_io.read channel >>= fun data ->
        Lwt.return @@ parse data

  let write mutex filename serialise data =
    Lwt_mutex.with_lock mutex @@
      fun () ->
      Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname filename) >>= fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Output filename @@
        fun channel ->
        Lwt_io.write channel (serialise data)
end

(* inspired from learnocaml_data.ml *)
let enc_check_version_1 file enc =
  J.conv
    (fun data -> ("1", data))
    (fun (version, data) ->
       begin
         match version with
         | "1" -> ()
         | _ ->
             let msg = Format.asprintf "%s: unknown version %s" file version in
             raise (J.Cannot_destruct ([], Failure msg))
       end ;
       data)
    (J.merge_objs (J.obj1 (J.req "learnocaml_version" J.string))
       (J.obj1 (J.req file enc)))

module BaseTokenIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "token.json"

  let enc = enc_check_version_1 file @@ J.list Token.enc

  let parse = Json_codec.decode enc
  let serialise_str = Json_codec.encode ~minify:false
                        (enc_check_version_1 file J.(list string))
  let serialise = Json_codec.encode ~minify:false enc

  let create_index sync_dir =
    let found_indexes =
      let rec scan f d acc =
        let rec aux s acc =
          Lwt.catch (fun () ->
              Lwt_stream.get s >>= function
              | Some ("." | ".." | "data") -> aux s acc
              | Some x -> scan f (d / x) acc >>= aux s
              | None -> Lwt.return acc)
          @@ function
          | Unix.Unix_error (Unix.ENOTDIR, _, _) -> f d acc
          | Unix.Unix_error _ -> Lwt.return acc
          | e -> Lwt.fail e
        in
        aux (Lwt_unix.files_of_directory (sync_dir / d)) acc
      in
      scan (fun d acc ->
          let d =
            if Filename.basename d = "save.json" then Filename.dirname d
            else d
          in
          let stok = String.map (function '/' | '\\' -> '-' | c -> c) d in
          if Token.check stok then
            Lwt.return (stok :: acc)
          else
            Lwt.return acc
        ) "" [] in
    Lwt_io.printl "[INFO] Regenerating the token index..." >>= fun () ->
    found_indexes >>= RW.write rw (sync_dir / indexes_subdir / file) serialise_str

  let get_file sync_dir name =
    let filename = (sync_dir / indexes_subdir / name) in
    let create () =
          create_index sync_dir >>= fun () ->
          RW.read rw filename parse in
    if Sys.file_exists filename then
      Lwt.catch
        (fun () -> RW.read rw filename parse)
        (fun _exn ->
           (* Note: this error handler may be adapted later to be more conservative?
              it does not matter now as sync/data/token.json is not a critical file, and
              can be regenerated. *)
           create ())
    else
      create ()

  let get_tokens sync_dir =
    get_file sync_dir file

  let add_token sync_dir token =
    get_tokens sync_dir >>= fun tokens ->
    if not (List.exists (fun found_token -> found_token = token) tokens) then
      RW.write rw (sync_dir / indexes_subdir / file) serialise (token :: tokens)
    else
      Lwt.return_unit
end

module TokenIndex = BaseTokenIndex (IndexFile)

module BaseMoodleIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "moodle_user.json"

  let enc = enc_check_version_1 file @@ J.assoc Token.enc

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let create_index sync_dir =
    RW.write rw (sync_dir / indexes_subdir / file) serialise []

  let get_users sync_dir =
    Lwt.catch
      (fun () -> RW.read rw (sync_dir / indexes_subdir / file) parse)
      (fun _exn -> Lwt.return [])

  let user_exists sync_dir id =
    get_users sync_dir >|=
    List.exists (fun (rid, _token) -> rid = id)

  let token_exists sync_dir token =
    get_users sync_dir >|=
    List.exists (fun (_id, rtoken) -> rtoken = token)

  let add_user sync_dir id token =
    get_users sync_dir >>= fun users ->
    if List.exists (fun (rid, _token) -> rid = id) users then
      Lwt.return ()
    else
      let users = (id, token) :: users in
      RW.write rw (sync_dir / indexes_subdir / file) serialise users

  let get_user_token sync_dir id =
    get_users sync_dir >|= fun users ->
    List.find (fun (rid, _token) -> rid = id) users
    |> snd
end

module MoodleIndex = BaseMoodleIndex (IndexFile)

module BaseOauthIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "oauth.json"

  let enc = enc_check_version_1 file @@ J.(assoc (list string))

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let create_index sync_dir =
    let secret = generate_random_hex 32 in
    RW.write rw (sync_dir / indexes_subdir / file) serialise [(secret, [])] >|= fun () ->
    secret

  let get_first_oauth sync_dir =
    let create () =
      create_index sync_dir >|= fun secret ->
      (secret, []) in
    Lwt.catch
      (fun () ->
         RW.read rw (sync_dir / indexes_subdir / file) parse >>= function
         | oauth :: _ -> Lwt.return oauth
         | [] -> create ())
      (fun _exn -> create ())

  let get_current_secret sync_dir =
    get_first_oauth sync_dir >|= fun (secret, _nonces) ->
    secret

  let purge sync_dir =
    get_first_oauth sync_dir >>= fun oauth ->
    RW.write rw (sync_dir / indexes_subdir / file) serialise [oauth]

  let add_nonce sync_dir nonce =
    RW.read rw (sync_dir / indexes_subdir / file) parse >>= fun oauth ->
    let oauth =
      match oauth with
      | (secret, nonces) :: r -> (secret, nonce :: nonces) :: r
      | [] -> [(generate_random_hex 32, [nonce])] in
    RW.write rw (sync_dir / indexes_subdir / file) serialise oauth

  let check_nonce sync_dir nonce =
    get_first_oauth sync_dir >|= fun (_secret, nonces) ->
    List.exists ((=) nonce) nonces
end

module OauthIndex = BaseOauthIndex (IndexFile)

type oauth_args = {
    signature: string;
    timestamp: string;
    nonce: string;
    version: string;
    consumer_key: string;
    signature_method: string;
  }

let get_oauth_args args =
  (* POST request handling *)
  List.(
    let signature = assoc "oauth_signature" args and
        timestamp = assoc "oauth_timestamp" args and
        nonce = assoc "oauth_nonce" args and
        version = assoc "oauth_version" args and
        consumer_key = assoc "oauth_consumer_key" args and
        signature_method = assoc "oauth_signature_method" args in
    {signature; timestamp; nonce; version; consumer_key; signature_method}
  )

(* Based on gapi-ocaml
  This function will build a signature by using hmac_sha1 algorithm.*)
let signature_oauth list_args http_method basic_uri secret =
  let pair_encode = (* 1 : encode keys/values *)
    List.filter (fun (k, _) -> k <> "oauth_signature") list_args
    |> List.map (fun (k, v) ->
         Netencoding.Url.(encode ~plus:false k, encode ~plus:false v)) in
  let pair_sorted = List.sort compare pair_encode in
  let list_concat =  (* 3 : Form key=value&key2=value2*)
    List.map (fun (k, v) -> k ^ "=" ^ v) pair_sorted
    |> String.concat "&" in
  let signature_base_string =     (* 4 : Add HTTP method and URI *)
    Printf.sprintf "%s&%s&%s" (String.uppercase_ascii http_method)
      (Netencoding.Url.encode ~plus:false basic_uri)
      (Netencoding.Url.encode ~plus:false list_concat) in
  let signing_key = (Netencoding.Url.encode ~plus:false secret) ^ "&" in  (* 5 : Build signing_key *)
  let encoding =
    let hash = Cryptokit.MAC.hmac_sha1 signing_key in
    let result = Cryptokit.hash_string hash signature_base_string in
    B64.encode result
  in encoding

let oauth_signature_method = "HMAC-SHA1"

(** Don't give the same oauth_consumer_key to differents LTI consumer **)
(* Deal with the request to check OAuth autenticity and return Moodle user's token*)
let check_oauth sync_dir url args =
  try
    let oauth_args = get_oauth_args args in
    if oauth_args.signature_method <> oauth_signature_method then
      Lwt.return (Error "Not implemented")
    else
      OauthIndex.check_nonce sync_dir oauth_args.nonce >>= fun exists ->
      if exists then
        Lwt.return (Error "Nonce already used")
      else
        OauthIndex.add_nonce sync_dir oauth_args.nonce >>= fun () ->
        OauthIndex.get_current_secret sync_dir >|=
          signature_oauth args "post" url >>= fun s ->
        if Eqaf.equal s oauth_args.signature then
          Lwt.return (Ok (oauth_args.consumer_key ^ ":" ^ (List.assoc "user_id" args)))
        else
          Lwt.return (Error "Wrong signature")
  with Not_found ->
    Lwt.return (Error "Missing args")

(** Invariants:
  * [Password (_, email, _, Some email)]: init state, unverified email
  * [Password (_, email, _, None)]: verified email
  * [Password (_, email, _, Some other_email)]: pending email change
  *)
type user =
  | Token of (Token.t * bool)
  | Password of (Token.t * string * string * string option)

type authentication =
  | AuthToken of Token.t
  | Passwd of (string * string)

module BaseUserIndex (RW: IndexRW) = struct
  let rw = RW.init ()

  (** Invariant: all emails are pairwise different (except possibly in
      the initial account state: [Password (_, email, _, Some email)]).

      Also, users can login directly with their (legacy) token only if
      a password is not yet defined, and the token has not yet been
      associated with some Moodle credential: [Token (_, false)]. *)
  let file = "user.json"

  let enc =
    enc_check_version_1 file
    @@ J.(
      list (union [case (tup2 Token.enc bool)
                     (function
                      | Token (token, using_moodle) -> Some (token, using_moodle)
                      | _ -> None)
                     (fun (token, using_moodle) -> Token (token, using_moodle));
                   case (tup4 Token.enc string string (option string))
                     (function
                      | Password (token, email, passwd, verify_email) ->
                         Some (token, email, passwd, verify_email)
                      | _ -> None)
                     (fun (token, email, passwd, verify_email) ->
                       Password (token, email, passwd, verify_email))]))

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let token_list_to_users =
    List.map (fun token -> Token (token, false))

  let create_index ?(tokens) sync_dir =
    match tokens with
    | Some tokens ->
       let users = token_list_to_users tokens in
       RW.write rw (sync_dir / indexes_subdir / file) serialise users >|= fun () ->
       users
    | None ->
       TokenIndex.get_tokens sync_dir >>= fun tokens ->
       Lwt_io.printl "[INFO] Generating the user index from token index..." >>= fun () ->
       let users = token_list_to_users tokens in
       RW.write rw (sync_dir / indexes_subdir / file) serialise users >|= fun () ->
       users

  let get_data sync_dir =
    Lwt.catch
      (fun () -> RW.read rw (sync_dir / indexes_subdir / file) parse)
      (fun _exn -> create_index sync_dir)

  let authenticate sync_dir auth =
    get_data sync_dir >|=
    List.fold_left (fun res elt ->
        if res = None then
          match auth, elt with
          | AuthToken token, Token (found_tok, use_moodle)
               when not use_moodle && found_tok = token ->
             Some (token)
          | Passwd (email, _), Password (_, found_email, _, Some new_email)
               when found_email = email && found_email = new_email ->
             None
          | Passwd (email, passwd), Password (token, found_email, found_passwd, _)
               when found_email = email && Bcrypt.verify passwd (Bcrypt.hash_of_string found_passwd) ->
             Some (token)
          | _ ->
             None
        else res) None

  let exists sync_dir email =
    get_data sync_dir >|=
      List.exists (function
          | Password (_token, found_email, _passwd, None) -> found_email = email
          | Password (_token, found_email, _passwd, Some verify_email) ->
             found_email = email || verify_email = email
          | _ -> false)

  (* private function; might be exposed in the .mli if need be *)
  let exists_token token user_list =
    List.exists (function
        | Token (found_token, _moodle) -> found_token = token
        | Password (found_token, _email, _passwd, _pending) -> found_token = token)
    user_list

  let add sync_dir auth =
    get_data sync_dir >>= fun users ->
    let token, new_user = match auth with
      | Token (token, _) -> (token, auth)
      | Password (token, email, passwd, verify_email) ->
         let hash = Bcrypt.string_of_hash @@ Bcrypt.hash passwd in
         (token, Password (token, email, hash, verify_email)) in
    if exists_token token users then
      logfailwith "BaseUserIndex.add: duplicate token" (Token.to_string token)
    else
      RW.write rw (sync_dir / indexes_subdir / file) serialise (new_user :: users)

  let update sync_dir token passwd =
    get_data sync_dir >|=
      List.map (function
          | Token (found_token, _use_moodle) when found_token = token ->
             logfailwith "BaseUserIndex.update: invalid action" (Token.to_string token)
          | Password (found_token, email, _passwd, verify) when found_token = token ->
             let hash = Bcrypt.string_of_hash @@ Bcrypt.hash passwd in
             Password (token, email, hash, verify)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let upgrade_moodle sync_dir token =
    get_data sync_dir >|=
      List.map (function
          | Token (found_token, _use_moodle) when found_token = token ->
             Token (token, true)
          | Password (found_token, _email, _passwd, _verify)
               when found_token = token ->
             logfailwith "BaseUserIndex.upgrade_moodle: invalid action" (Token.to_string token)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let upgrade sync_dir token email passwd =
    (exists sync_dir email >|= fun exists ->
     if exists then
        logfailwith "BaseUserIndex.upgrade: duplicate email" email)
    >>= fun () ->
    get_data sync_dir >|=
      List.map (function
          | Token (found_token, _use_moodle) when found_token = token ->
             let hash = Bcrypt.string_of_hash @@ Bcrypt.hash passwd in
             Password (token, email, hash, Some(email))
          | Password (found_token, _email, _passwd, _verify)
               when found_token = token ->
             logfailwith "BaseUserIndex.upgrade: invalid action" (Token.to_string token)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let confirm_email sync_dir token =
    get_data sync_dir >|=
      List.map (function
          | Password (found_token, _email, passwd, Some verify)
               when found_token = token ->
             Password (found_token, verify, passwd, None)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let can_login ?(use_passwd = true) ?(use_moodle = true) sync_dir token =
    get_data sync_dir >|= fun users ->
      List.find_opt (function
          | Token (found_token, moodle_account)
            -> found_token = token && not (use_moodle && moodle_account)
          | Password (found_token, _email, _passwd, _verify) ->
             found_token = token && not use_passwd) users <> None

  let token_of_email sync_dir email =
    RW.read rw (sync_dir / indexes_subdir / file) parse >|=
    List.fold_left (fun res elt ->
        match res, elt with
        | None, Password (token, found_email, _, _) when found_email = email -> Some token
        | _ -> res) None

  let emails_of_token sync_dir token =
    RW.read rw (sync_dir / indexes_subdir / file) parse >|=
    List.fold_left (fun res elt ->
        match res, elt with
        | None, Password (found_token, email, _, pending) when found_token = token ->
           Some (email, pending)
        | _ -> res) None

  let change_email sync_dir token new_email =
    (exists sync_dir new_email >|= fun exists ->
     if exists then
       logfailwith "BaseUserIndex.change_email: duplicate email" new_email)
    >>= fun () ->
    RW.read rw (sync_dir / indexes_subdir / file) parse >|=
      List.map (function
          | Password (found_token, email, passwd, _) when found_token = token ->
             Password (found_token, email, passwd, Some new_email)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let abort_email_change sync_dir token =
    RW.read rw (sync_dir / indexes_subdir / file) parse >|=
      List.map (function
          | Password (found_token, email, passwd, Some pending)
               when found_token = token && email <> pending ->
             Password (found_token, email, passwd, None)
          | Token (found_token, _moodle) when found_token = token ->
             logfailwith "BaseUserIndex.abort_email_change: invalid action" (Token.to_string token)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise
end

module UserIndex = BaseUserIndex (IndexFile)

module BaseUpgradeIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "upgrade.json"

  type t =
    | ChangeEmail
    | ResetPassword

  let enc =
    enc_check_version_1 file
    @@ J.(
      assoc (tup3 Token.enc float
               (string_enum ["change_email", ChangeEmail;
                             "reset_password", ResetPassword])))

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let create_index sync_dir =
    RW.write rw (sync_dir / indexes_subdir / file) serialise [] >|= fun () ->
    []

  let get_data sync_dir =
    Lwt.catch
      (fun () -> RW.read rw (sync_dir / indexes_subdir / file) parse)
      (fun _exn -> create_index sync_dir)

  let create_upgrade_operation kind sync_dir token =
    get_data sync_dir >>= fun operations ->
    let id = generate_random_hex 32 in
    (id, (token, Unix.time (), kind)) :: operations
    |> RW.write rw (sync_dir / indexes_subdir / file) serialise >|= fun () ->
    id

  let change_email = create_upgrade_operation ChangeEmail
  let reset_password = create_upgrade_operation ResetPassword

  let check_upgrade_operation kind sync_dir handle =
    get_data sync_dir >|= fun operations ->
    (* expires after 4 hours *)
    let expiration_threshold = floor (Unix.time ()) +. 4. *. 3600. in
    match List.assoc_opt handle operations with
    | Some (token, date, ResetPassword)
         when kind = ResetPassword && date <= expiration_threshold -> Some token
    | Some (token, _date, ChangeEmail) when kind = ChangeEmail -> Some token
    | _ -> None

  let can_change_email = check_upgrade_operation ChangeEmail
  let can_reset_password = check_upgrade_operation ResetPassword

  let ongoing_change_email sync_dir token =
    get_data sync_dir >>= fun operations ->
    List.map fst @@
      List.filter (fun (_handle, (found_token, _date, operation)) ->
          operation = ChangeEmail && token = found_token) operations
    |> function
      | [] -> Lwt.return_none
      | handle :: [] -> Lwt.return_some handle
      | handle :: _ ->
         Lwt_io.printlf "[WARNING] several ChangeEmail handles for %s"
           (Token.to_string token) >>= fun () ->
         Lwt.return_some handle

  let abort_email_change sync_dir token =
    get_data sync_dir >>= fun operations ->
    List.filter (fun (_handle, (found_token, _date, operation)) ->
        operation = ResetPassword || token <> found_token) operations
    |> RW.write rw (sync_dir / indexes_subdir / file) serialise

  let revoke_operation sync_dir handle =
    get_data sync_dir >|=
    List.filter (fun (found_handle, _operation) -> found_handle <> handle) >>=
    RW.write rw (sync_dir / indexes_subdir / file) serialise

  let filter_old_operations sync_dir =
    get_data sync_dir >>= fun operations ->
    (* expires after 4 weeks *)
    let expiration_threshold = floor (Unix.time ()) +. 4. *. 604800. in
    List.filter (fun (_id, (_token, date, operation)) ->
        operation = ChangeEmail || date <= expiration_threshold) operations
    |> RW.write rw (sync_dir / indexes_subdir / file) serialise
end

module UpgradeIndex = BaseUpgradeIndex (IndexFile)
