open Lwt
open Learnocaml_data

let ( / ) dir f = if dir = "" then f else Filename.concat dir f
let indexes_subdir = "data"

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
  val read : string -> (string -> 'a) -> 'a Lwt.t
  val write : t -> string -> ('a -> string) -> 'a -> unit Lwt.t
end

module IndexFile: IndexRW = struct
  type t = Lwt_mutex.t

  (* Unlocked by default *)
  let init = Lwt_mutex.create

  let read filename parse =
    Lwt_io.open_file ~mode:Lwt_io.Input filename >>= fun channel ->
    Lwt_io.read channel >>= fun data ->
    Lwt_io.close channel >>= fun () ->
    Lwt.return @@ parse data

  let write mutex filename serialise data =
    Lwt_mutex.lock mutex >>= fun () ->
    let path = Filename.dirname filename in
    Lwt_utils.is_directory path >>= fun is_directory ->
    (if is_directory then
       Lwt.return_unit
     else
       Lwt_unix.mkdir path 0o755) >>= fun () ->
    Lwt_io.open_file ~mode:Lwt_io.Output filename >>= fun channel ->
    Lwt_io.write channel (serialise data) >>= fun () ->
    Lwt_io.close channel >>= fun () ->
    Lwt.return @@ Lwt_mutex.unlock mutex
end

module BaseTokenIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "token.json"

  let enc = J.list Token.enc

  let parse = Json_codec.decode enc
  let serialise_str = Json_codec.encode ~minify:false J.(list string)
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
    Lwt_io.printl "Regenerating the token index..." >>= fun () ->
    found_indexes >>= RW.write rw (sync_dir / indexes_subdir / file) serialise_str

  let get_file sync_dir name =
    let filename = (sync_dir / indexes_subdir / name) in
    let create () =
          create_index sync_dir >>= fun () ->
          RW.read filename parse in
    if Sys.file_exists filename then
      Lwt.catch
        (fun () -> RW.read filename parse)
        (fun _exn ->
           (* Note: this error handler may be adapted later to be more conservative?
              it does not matter now as sync/token.json is not a critical file, and
              can be regenerated. *)
           create ())
    else
      create ()

  let get_tokens sync_dir =
    get_file sync_dir file

  let add_token sync_dir token =
    get_tokens sync_dir >>= fun tokens ->
    RW.write rw (sync_dir / indexes_subdir / file) serialise (token :: tokens)
end

module TokenIndex = BaseTokenIndex (IndexFile)

module BaseMoodleIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "moodle_user.json"

  let enc = J.assoc Token.enc

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let create_index sync_dir =
    RW.write rw (sync_dir / indexes_subdir / file) serialise []

  let get_users sync_dir =
    Lwt.catch
      (fun () -> RW.read (sync_dir / indexes_subdir / file) parse)
      (fun _exn -> Lwt.return [])

  let user_exists sync_dir id =
    get_users sync_dir >|=
    List.exists (fun (rid, _token) -> rid = id)

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

  let enc = J.(assoc (list string))

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
         RW.read (sync_dir / indexes_subdir / file) parse >>= function
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
    RW.read (sync_dir / indexes_subdir / file) parse >>= fun oauth ->
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
          Lwt.return (Ok (oauth_args.consumer_key ^ (List.assoc "user_id" args)))
        else
          Lwt.return (Error "Wrong signature")
  with Not_found ->
    Lwt.return (Error "Missing args")

type user =
  | Token of (Token.t * bool)
  | Password of (Token.t * string * string * string option)

type authentication =
  | AuthToken of Token.t
  | Passwd of (string * string)

module BaseUserIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "user.json"

  let enc = J.(
      list (union [case (tup2 Token.enc bool)
                     (function
                      | Token (token, using_moodle) -> Some (token, using_moodle)
                      | _ -> None)
                     (fun (token, using_moodle) -> Token (token, using_moodle));
                   case (tup4 Token.enc string string (option string))
                     (function
                      | Password (token, username, passwd, verify_email) ->
                         Some (token, username, passwd, verify_email)
                      | _ -> None)
                     (fun (token, username, passwd, verify_email) ->
                       Password (token, username, passwd, verify_email))]))

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let token_list_to_users =
    List.map (fun token -> Token (token, false))

  let create_index sync_dir tokens =
    token_list_to_users tokens
    |> RW.write rw (sync_dir / indexes_subdir / file) serialise

  let get_data sync_dir =
    Lwt.catch
      (fun () -> RW.read (sync_dir / indexes_subdir / file) parse)
      (fun _exn ->
        TokenIndex.get_tokens sync_dir >>= fun tokens ->
        let users = token_list_to_users tokens in
        RW.write rw (sync_dir / indexes_subdir / file) serialise users >|= fun () ->
        users)

  let authenticate sync_dir auth =
    get_data sync_dir >|=
    List.fold_left (fun res elt ->
        if res = None then
          match auth, elt with
          | AuthToken token, Token (found_tok, use_moodle)
               when not use_moodle && found_tok = token ->
             Some (token)
          | Passwd (name, passwd), Password (token, found_name, found_passwd, _)
               when found_name = name && Bcrypt.verify passwd (Bcrypt.hash_of_string found_passwd) ->
             Some (token)
          | _ ->
             None
        else res) None

  let exists sync_dir name =
    get_data sync_dir >|=
      List.exists (function
          | Password (_token, found_name, _passwd, None) -> found_name = name
          | Password (_token, found_name, _passwd, Some verify_email) ->
             found_name = name || verify_email = name
          | _ -> false)

  let add sync_dir auth =
    get_data sync_dir >>= fun users ->
    let new_user = match auth with
      | Token _ -> auth
      | Password (token, name, passwd, verify_email) ->
         let hash = Bcrypt.string_of_hash @@ Bcrypt.hash passwd in
         Password (token, name, hash, verify_email) in
    RW.write rw (sync_dir / indexes_subdir / file) serialise (new_user :: users)

  let upgrade sync_dir token name passwd =
    get_data sync_dir >|=
      List.map (function
          | Token (found_token, _use_moodle) when found_token = token ->
             let hash = Bcrypt.string_of_hash @@ Bcrypt.hash passwd in
             Password (token, name, hash, Some(name))
          | Password (found_token, name, _passwd, verify) when found_token = token ->
             let hash = Bcrypt.string_of_hash @@ Bcrypt.hash passwd in
             Password (token, name, hash, verify)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let confirm_email sync_dir token =
    get_data sync_dir >|=
      List.map (function
          | Password (found_token, _name, passwd, Some verify) when found_token = token ->
             Password (found_token, verify, passwd, None)
          | elt -> elt) >>=
      RW.write rw (sync_dir / indexes_subdir / file) serialise

  let can_login sync_dir token =
    RW.read (sync_dir / indexes_subdir / file) parse >|= fun users ->
      List.find_opt (function
          | Token (found_token, use_moodle) -> found_token = token && not use_moodle
          | _ -> false) users <> None

  let token_of_email sync_dir email =
    RW.read (sync_dir / indexes_subdir / file) parse >|=
    List.fold_left (fun res elt ->
        match res, elt with
        | None, Password (token, found_email, _, _) when found_email = email -> Some token
        | _ -> res) None
end

module UserIndex = BaseUserIndex (IndexFile)

module BaseUpgradeIndex (RW: IndexRW) = struct
  let rw = RW.init ()
  let file = "upgrade.json"

  type t =
    | ChangeEmail
    | ResetPassword

  let enc = J.(
      assoc (tup2 Token.enc (string_enum ["change_email", ChangeEmail;
                                          "reset_password", ResetPassword])))

  let parse = Json_codec.decode enc
  let serialise = Json_codec.encode ~minify:false enc

  let create_index sync_dir =
    RW.write rw (sync_dir / indexes_subdir / file) serialise [] >|= fun () ->
    []

  let get_data sync_dir =
    Lwt.catch
      (fun () -> RW.read (sync_dir / indexes_subdir / file) parse)
      (fun _exn -> create_index sync_dir)

  let create_upgrade_operation kind sync_dir token =
    get_data sync_dir >>= fun operations ->
    let id = generate_random_hex 32 in
    (id, (token, kind)) :: operations
    |> RW.write rw (sync_dir / indexes_subdir / file) serialise >|= fun () ->
    id

  let change_email = create_upgrade_operation ChangeEmail
  let reset_password = create_upgrade_operation ResetPassword

  let check_upgrade_operation kind sync_dir handle =
    get_data sync_dir >|= fun operations ->
    match List.assoc_opt handle operations with
    | Some (token, found_kind) when found_kind = kind -> Some token
    | _ -> None

  let can_change_email = check_upgrade_operation ChangeEmail
  let can_reset_password = check_upgrade_operation ResetPassword

  let revoke_operation sync_dir handle =
    get_data sync_dir >|=
    List.filter (fun (found_handle, _operation) -> found_handle <> handle) >>=
    RW.write rw (sync_dir / indexes_subdir / file) serialise
end

module UpgradeIndex = BaseUpgradeIndex (IndexFile)
