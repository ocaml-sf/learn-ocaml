(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Cmdliner
open Arg

let info = info ~docs:"SERVER OPTIONS"

let sync_dir =
  value & opt string "./sync" & info ["sync-dir"] ~docv:"DIR" ~doc:
    "Directory where to store user sync tokens"

let default_http_port = 8080
let default_https_port = 8443

let cert =
  value & opt (some string) None &
  info ["cert"] ~docv:"BASENAME" ~env:(Arg.env_var "LEARNOCAML_CERT") ~doc:
    "HTTPS certificate: this option turns on HTTPS, and requires files \
     $(i,BASENAME.pem) and $(i,BASENAME.key) to be present. They will be \
     used as the server certificate and key, respectively. A passphrase \
     may be asked on the terminal if the key file is encrypted."

let port =
  value & opt (some int) None &
  info ["port";"p"] ~docv:"PORT" ~env:(Arg.env_var "LEARNOCAML_PORT") ~doc:
    (Printf.sprintf
       "The TCP port on which to run the server. Defaults to %d, or %d if \
        HTTPS is enabled."
       default_http_port default_https_port)

type t = {
  sync_dir: string;
  base_url: string;
  port: int;
  cert: string option;
}

let term app_dir base_url =
  let apply app_dir sync_dir base_url port cert =
    Learnocaml_store.static_dir := app_dir;
    Learnocaml_store.sync_dir := sync_dir;
    let port = match port, cert with
      | Some p, _ -> p
      | None, Some _ -> default_https_port
      | None, None -> default_http_port
    in
    Learnocaml_server.cert_key_files :=
      (match cert with
       | Some base -> Some (base ^ ".pem", base ^ ".key");
       | None -> None);
    Learnocaml_server.port := port;
    Learnocaml_server.base_url := base_url;
    { sync_dir; base_url; port; cert }
  in
  (* warning: if you add any options here, remember to pass them through when
     calling the native server from learn-ocaml main *)
  Term.(const apply $ app_dir $ sync_dir $ base_url $ port $ cert)
