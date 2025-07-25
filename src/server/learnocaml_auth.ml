(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Learnocaml_store
open Lwt.Infix


let generate_random_hex len =
  Cryptokit.Random.string Cryptokit.Random.secure_rng len
  |> Cryptokit.transform_string @@ Cryptokit.Hexa.encode ()

let safe_encode s =
  Uri.pct_encode ~component:`Userinfo s


let generate_hmac secret csrf user_id =
  let decoder = Cryptokit.Hexa.decode () in
  let secret = Cryptokit.transform_string decoder secret in
  let hmac = Cryptokit.MAC.hmac_sha256 secret and
      encoder = Cryptokit.Hexa.encode () in
  Cryptokit.hash_string hmac (csrf ^ user_id)
  |> Cryptokit.transform_string encoder

module type AUTH_METHOD = sig
  type login_credentials
  type register_credentials
  type associate_credentials

  val login : login_credentials -> (Token.t, string) result Lwt.t
  val register : register_credentials -> (Token.t, string) result Lwt.t
  val associate : associate_credentials -> (Token.t, string) result Lwt.t
  val get_token : params:(string * string) list -> (Token.t, string) result Lwt.t
end

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
           (safe_encode k,safe_encode v)) in
  let pair_sorted = List.sort compare pair_encode in
  let list_concat =  (* 3 : Form key=value&key2=value2*)
    List.map (fun (k, v) -> k ^ "=" ^ v) pair_sorted
    |> String.concat "&" in
  let signature_base_string =     (* 4 : Add HTTP method and URI *)
    Printf.sprintf "%s&%s&%s" (String.uppercase_ascii http_method)
      (safe_encode basic_uri)
      (safe_encode list_concat) in
  let signing_key = (safe_encode secret) ^ "&" in  (* 5 : Build signing_key *)
  let encoding =
    let hash = Cryptokit.MAC.hmac_sha1 signing_key in
    let result = Cryptokit.hash_string hash signature_base_string in
    Base64.encode result
  in match encoding with
     | Ok string -> string
     | Error (`Msg msg) -> failwith msg

let oauth_signature_method = "HMAC-SHA1"


(** LTI (Moodle) authentication implementation *)
module LtiAuth = struct
  type login_credentials = { user_id : string }
  type register_credentials = { user_id : string; nickname : string; csrf : string; hmac : string }
  type associate_credentials = { user_id : string; token : Token.t; csrf : string; hmac : string }

  let user_exists _ = Lwt.return true

 let get_token id =
   LtiIndex.get_user_token id >>= function
   | Some token -> Lwt.return (Ok token)
   | None -> Lwt.return (Error "Token not found")

  let can_login id token =
    LtiIndex.exists id >>= fun already_linked ->
    if already_linked then
      Lwt.return false
    else
      TokenIndex.has token "lti" >|= not


  let login (_:login_credentials) = Lwt.return (Error "TODO: implement LtiAuth.login")

  let register (params:register_credentials) =
    NonceIndex.get_current_secret () >>= fun secret ->
    let new_hmac = generate_hmac secret params.csrf params.user_id in
    if not (Eqaf.equal params.hmac new_hmac) then
      Lwt.return (Error "bad hmac")
    else
      let nickname = params.nickname in
      Token.create_student () >>= fun token ->
      (if nickname = "" then Lwt.return_unit
       else Save.set token Save.{empty with nickname})
      >>= fun () ->
      LtiIndex.add params.user_id token >>= fun () ->
      Lwt.return (Ok token)

  let associate (params:associate_credentials) =
    NonceIndex.get_current_secret () >>= fun secret ->
    let new_hmac = generate_hmac secret params.csrf params.user_id in
    if not (Eqaf.equal params.hmac new_hmac) then
      Lwt.return (Error "bad hmac")
    else
      can_login params.user_id params.token >>= fun canlogin ->
      if not canlogin then
        Lwt.return (Error "Bad token (or token already used by an upgraded account")
      else
        LtiIndex.add params.user_id params.token >>= fun () ->
        Lwt.return (Ok params.token)

  (** Don't give the same oauth_consumer_key to differents LTI consumer **)
  (* Deal with the request to check OAuth autenticity and return Moodle user's token*)
  let check_oauth url args =
    try
      let oauth_args = get_oauth_args args in
      if oauth_args.signature_method <> oauth_signature_method then
        Lwt.return (Error "Not implemented")
      else
        NonceIndex.check_nonce oauth_args.nonce >>= fun exists ->
        if exists then
          Lwt.return (Error "Nonce already used")
        else
          NonceIndex.add_nonce oauth_args.nonce >>= fun () ->
          NonceIndex.get_current_secret () >|=
            signature_oauth args "post" url >>= fun s ->
          if Eqaf.equal s oauth_args.signature then
            Lwt.return (Ok ((safe_encode oauth_args.consumer_key) ^ "/" ^ (List.assoc "user_id" args)))
          else
            Lwt.return (Error "Wrong signature")
    with Not_found ->
      Lwt.return (Error "Missing args")

end
