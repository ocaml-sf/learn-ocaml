(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Lwt.Infix
module Api = Learnocaml_api

let version = Api.version

module Args = struct
  open Cmdliner
  open Arg

  type global_infos = {
      server_url: Uri.t option;
      token: Learnocaml_data.student Learnocaml_data.token option;
      local: bool;
    }

  type exercises_infos = {
    solution_file: string option;
    exercise_id: string option;
    output_format: [`Console|`Json|`Html|`Raw];
    submit: bool;
    color: bool;
    verbosity: int;
    }

  type all_infos = (global_infos * exercises_infos)

  let url_conv =
    conv ~docv:"URL" (
      (fun s ->
         try Ok (Uri.of_string s)
         with e -> Error (`Msg (Printexc.to_string e))),
      Uri.pp_hum
    )

  let token_conv =
    conv ~docv:"TOKEN" (
      (fun s ->
         try Ok (Token.parse s)
         with Failure msg ->
           Error (`Msg (Printf.sprintf "Invalid token %s: %s" s msg))),
      (fun fmt t -> Format.pp_print_string fmt (Token.to_string t))
    )

  let server_url =
    value & opt (some url_conv) None &
    info ["s";"server"] ~docv:"URL" ~doc:
      "The URL of the learn-ocaml server"
      ~env:(Cmdliner.Term.env_info "LEARNOCAML_SERVER" ~doc:
            "Sets the learn-ocaml server URL. Overridden by $(b,--server).")

  let solution_file =
    value & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:
      "The file containing the user's solution to the exercise"

  let exercise_id =
    value & opt (some string) None & info ["id"] ~docv:"ID" ~doc:
      "The exercise identifier. If unspecified, this is inferred from the file \
       name"

  let output_format =
    value & vflag `Console & [
      `Console, info ["console"] ~doc:
        "output the results to the console. This is the default";
      `Json, info ["json"] ~doc:
        "output the results as JSON, to stdout";
      `Html, info ["html"] ~doc:
        "output the results as HTML, to the console";
      `Raw, info ["raw"] ~doc:
        "output the results as raw text";
    ]

  let dont_submit =
    value & flag & info ["n";"dry-run"] ~doc:
      "Perform the grading locally, but don't submit back to the server"

  let color_when =
    let when_enum = ["always", Some true; "never", Some false; "auto", None] in
    value & opt (enum when_enum) None & info ["color"] ~docv:"WHEN" ~doc:
      ("Colorise the output, and also allows use of UTF-8 characters. $(docv) \
        must be "^doc_alts_enum when_enum)

  let verbose =
    value & flag_all & info ["v";"verbose"] ~doc:
      "Be more verbose. Can be repeated"

  let token =
    value & opt (some token_conv) None & info ["token";"t"] ~docv:"TOKEN" ~doc:
      "Your token on the learn-ocaml server. This is required to submit \
       solutions"
      ~env:(Cmdliner.Term.env_info "LEARNOCAML_TOKEN" ~doc:
              "Sets the learn-ocaml user token on the sever. Overridden by \
               $(b,--token).")

  let local =
    value & flag & info ["local"] ~doc:
      "Generate a configuration file local to the current directory, rather \
       than user-wide"

  let global server_url token local =
    {server_url; token; local}

  let exercises solution_file exercise_id output_format dont_submit
        color_when verbose  =
    let color = match color_when with
      | Some o -> o
      | None -> Unix.(isatty stdout) && Sys.getenv_opt "TERM" <> Some "dumb"
    in
    {
      solution_file;
      exercise_id;
      output_format;
      submit = not dont_submit;
      color;
      verbosity = List.length verbose;
    }

  let term_global =
    Term.(const global
          $server_url $token $local)

  let term_exercises =
    Term.(const exercises
          $solution_file $exercise_id $output_format $dont_submit
          $color_when $verbose )
end

module ConfigFile = struct

  type t = {
    server: Uri.t;
    token: Token.t;
  }

  let local_path, user_path =
    let ( / ) = Filename.concat in
    Sys.getcwd () / ".learnocaml-client",
    (try Sys.getenv "HOME" with Not_found -> ".")
    / ".config" / "learnocaml" / "client.json"

  let path ?(local=false) () =
    if local then
      if Sys.file_exists local_path then Some local_path else None
    else
      List.find_opt Sys.file_exists [local_path; user_path]

  let enc =
    let open Json_encoding in
    conv
      (fun {server; token} -> server, token)
      (fun (server, token) -> {server; token}) @@
    obj2
      (req "server" (conv Uri.to_string Uri.of_string string))
      (req "token" Token.(conv to_string parse string))

  let read file =
    Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read >|=
    Ezjsonm.from_string >|=
    Json_encoding.destruct enc

  let write path t =
    Lwt_utils.mkdir_p (Filename.dirname path) >>= fun () ->
    Lwt_io.(with_file ~mode:Output ~perm:0o600 path) @@ fun oc ->
    Json_encoding.construct enc t |> function
    | `O _ | `A _ as json -> Lwt_io.write oc (Ezjsonm.to_string json)
    | _ -> assert false

end

module Console = struct

  let enable_colors = ref false
  let enable_utf8 = ref false

  let color cols =
    let code = function
      | `Bold -> "1"
      | `Underline -> "4"
      | `Crossed -> "9"
      | `Black -> "30"
      | `Red -> "31"
      | `Green -> "32"
      | `Yellow -> "33"
      | `Blue -> "34"
      | `Magenta -> "35"
      | `Cyan -> "36"
      | `White -> "37"
      | `Bg `Black -> "40;37"
      | `Bg `Red -> "41;37"
      | `Bg `Green -> "42;30"
      | `Bg `Yellow -> "43;30"
      | `Bg `Blue -> "44;37"
      | `Bg `Magenta -> "45;30"
      | `Bg `Cyan -> "46;30"
      | `Bg `White -> "47;30"
    in
    if !enable_colors then
      Printf.sprintf "\027[%sm%s\027[m"
        (String.concat ";" @@ (List.map code cols))
    else
      fun s -> s

  let status_line s =
    if !enable_colors then
      (flush stdout; Printf.eprintf "%s..%!\r\027[K" s)
    else
      Printf.eprintf "%s..\n" s

  let utf default c = if !enable_utf8 then c else default

  let block ?title ?border_color ?text_color ?(no_open=false) s =
    let top = utf "+" "\xe2\x94\x8c\xe2\x94\x80" (*U+250C U+2500*) in
    let left = utf "|" "\xe2\x94\x82" (*U+2502*) in
    let bottom = utf "`" "\xe2\x94\x94\xe2\x94\x80" (*U+2514 U+2500*) in
    let buf = Buffer.create (String.length s + String.length s / 10) in
    let oc = function None -> (fun s -> s) | Some col -> color col in
    if not no_open then
      Buffer.add_string buf (oc border_color top);
    (match title with
     | None -> ()
     | Some s ->
         match String.split_on_char '\n' s with
         | s1::r ->
             if not no_open then
               Buffer.add_string buf " ";
             Buffer.add_string buf s1;
             Buffer.add_char buf '\n';
             List.iter
               (fun si ->
                  if not no_open then
                    (Buffer.add_string buf left;
                     Buffer.add_string buf "  ");
                  Buffer.add_string buf si;
                  Buffer.add_char buf '\n')
               r;
             if not no_open then
               Buffer.add_string buf left;
         | [] -> ());
    if not no_open then
      Buffer.add_char buf '\n';
    List.iter
      (fun s ->
         Buffer.add_string buf (oc border_color left);
         Buffer.add_string buf "  ";
         Buffer.add_string buf (oc text_color s);
         Buffer.add_char buf '\n')
      (String.split_on_char '\n' s);
    Buffer.add_string buf (oc border_color bottom);
    Buffer.add_char buf '\n';
    Buffer.contents buf

  let button col s =
    let left = utf "<" "\xe2\x9d\xb0" (*U+2770*) in
    let right = utf ">" "\xe2\x9d\xb1" (*U+2771*) in
    color [`Bg col] left ^ color [`Bg col] s ^ color [`Bg col] right

  let hline ?(width=80) () =
    let c = utf "-" "\xe2\x94\x80" (*U+2500*) in
    let ln = String.length c in
    let b = Bytes.create (width * ln + 1) in
    for i = 0 to width - 1 do String.blit c 0 b (i * ln) ln done;
    Bytes.set b (width * ln) '\n';
    Bytes.to_string b

  let rec input ?default parse =
    flush stderr;
    let on_empty () =
      match default with
      | Some d -> d
      | None ->
          Printf.eprintf "I beg you pardon? %!";
          input ?default parse
    in
    try match read_line () with
      | "" -> on_empty ()
      | s ->
          try parse s with Failure msg ->
            Printf.eprintf "Invalid input: %s\nPlease try again: %!" msg;
            input ?default parse
    with
    | End_of_file -> prerr_newline (); on_empty ()
    | Sys.Break as e -> prerr_newline (); raise e

  let yesno ?(default=false) () =
    input ~default (fun s -> match String.lowercase_ascii s with
        | "y" | "yes" -> true
        | "n" | "no" -> false
        | _ -> failwith "please answer 'y' or 'n'.")

end

let get_score =
  let open Learnocaml_report in
  let rec get_score report =
    List.fold_left (fun acc -> function
        | Section (_text, report) -> get_score acc report
        | Message (_text, status) -> match status with
          | Success i -> acc + i
          | _ -> acc)
      report
  in
  get_score 0

let max_score exo = Learnocaml_exercise.(access File.max_score exo)

let print_score ?(max=1) ?color i =
  let color = match color with
    | None -> if i <= 0 then `Red else if i >= max then `Green else `Yellow
    | Some c -> c
  in
  if i <= 1 then
    Console.button color (Printf.sprintf " %3d pt  " i)
  else
    Console.button color (Printf.sprintf " %3d pts " i)

let console_report ?(verbose=false) ex report =
  let open Console in
  let open Learnocaml_report in
  let score = get_score report in
  let max_score = max_score ex in
  print_string (hline ());
  Printf.printf
    "## %-*s %s\n"
    (65 + String.length (color [`Bold] ""))
    (color [`Bold]
       (if score <= 0 then "Exercise failed"
        else if score >= max_score then "Exercise complete"
        else Printf.sprintf "Exercise incomplete (%02d%%)" (100 * score / max_score)))
    (print_score ~max:max_score score);
  print_string (hline ());
  print_newline ();
  let format_text t =
    String.concat " " @@ List.map (function
        | Text w -> w
        | Break -> "\n"
        | Code s when String.contains s '\n' -> "\n"^block ~border_color:[`Cyan] s
        | Code s -> color [`Cyan] s
        | Output s -> block ~border_color:[`Yellow] s)
      t
  in
  let rec all_good report =
    (List.for_all @@ function
      | Section (_, report) -> all_good report
      | Message (_, (Success _ | Informative | Warning | Important)) -> true
      | Message (_, Failure) -> false)
      report
  in
  let rec format_item = function
    | Section (text, report) ->
        let good = all_good report in
        let score = get_score report in
        let title =
          let color =
            if good then `Green else if score = 0 then `Red else `Yellow
          in
          print_score ~color score ^ " " ^ format_text text
        in
        if not verbose && all_good report then
          title
        else
          "\n" ^ block ~title ~no_open:true
            (String.concat "\n" @@ List.map format_item report)
    | Message (text, Success i) ->
        print_score i ^ "   " ^ format_text text
    | Message (text, Failure) ->
        print_score 0 ^ "   " ^ format_text text
    | Message (text, Warning) ->
        color [`Bg `Yellow] "[ warning ]" ^ "   " ^ format_text text
    | Message (text, Informative) ->
        format_text text
    | Message (text, Important) ->
        color [`Bg `Cyan]   "[important]" ^ "   " ^ format_text text
  in
  List.iter (fun i -> print_endline (format_item i)) report;
  print_newline ()

(* fixme: copied from Learnocaml_store *)
module Json_codec = struct
  let decode enc s =
    (match s with
     | "" -> `O []
     | s -> Ezjsonm.from_string s)
    |> Json_encoding.destruct enc

  let encode ?minify:_ enc x =
    match Json_encoding.construct enc x with
    | `A _ | `O _ as json -> Ezjsonm.to_string json
    | `Null -> ""
    | _ -> assert false
end

module Api_client = Learnocaml_api.Client (Json_codec)

let fetch server_url req =
  let url path args =
    let path = String.concat "/" (Uri.path server_url :: path) in
    let uri = Uri.with_path server_url path in
    Uri.with_query' uri args
  in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let do_req = function
    | { Learnocaml_api.meth = `GET; path; args } ->
        Client.get (url path args)
    | { Learnocaml_api.meth = `POST body; path; args } ->
        Client.post ~body:(Cohttp_lwt.Body.of_string body) (url path args)
  in
  Api_client.make_request
    (fun http_request ->
       do_req http_request >>= function
       | {Response.status = `OK; _}, body ->
           Cohttp_lwt.Body.to_string body >|= fun s -> Ok s
       | {Response.status = `Not_found; _}, _ ->
           Lwt.return (Error `Not_found)
       | {Response.status; _}, _ ->
           Lwt.return (Error (`Failure (Code.string_of_status status))))
    req
  >>= function
  | Ok x -> Lwt.return x
  | Error `Not_found -> raise Not_found
  | Error (`Failure s) -> Lwt.fail_with ("Server request failed: "^ s)

let fetch_exercise server_url token id =
  Lwt.catch (fun () -> fetch server_url (Api.Exercise (token, id)))
  @@ function
  | Not_found ->
      Printf.ksprintf Lwt.fail_with
        "Exercise %S was not found on the server."
        id
  | e -> Lwt.fail e

let upload_save server_url token save =
  Lwt.catch (fun () -> fetch server_url (Api.Update_save (token, save)))
  @@ function
  | e ->
      Printf.ksprintf Lwt.fail_with
        "Could not upload the results to the server: %s"
        (match e with Failure s -> s | e -> Printexc.to_string e)

let write_save_files save =
  Lwt_list.iter_s (fun (id, st) ->
      let f = Filename.concat (Sys.getcwd ()) (id ^ ".ml") in
      if Sys.file_exists f then
        (Printf.eprintf "File %s already exists, not overwriting.\n" f;
         Lwt.return_unit)
      else
        Lwt_io.(with_file ~mode:Output ~perm:0o600 f) @@ fun oc ->
        Lwt_io.write oc st.Answer.solution >|= fun () ->
        Printf.eprintf "Wrote file %s\n%!" f)
    (SMap.bindings (save.Save.all_exercise_states))

let upload_report server token ex solution report =
  let score = get_score report in
  let max_score = max_score ex in
  let id = Learnocaml_exercise.(access File.id ex) in
  let mtime = Unix.gettimeofday () in
  let exercise_state =
    { Answer.
      solution;
      grade = if max_score = 0 then None else Some (score * 100 / max_score);
      report = Some report;
      mtime;
    }
  in
  let new_save =
    { Save.
      nickname = "";
      all_exercise_editors = SMap.empty;
      all_exercise_states = SMap.singleton id exercise_state;
      all_toplevel_histories = SMap.empty;
      all_exercise_toplevel_histories = SMap.empty;
    }
  in
  Lwt.catch (fun () -> upload_save server token new_save)
  @@ function
  | Not_found ->
      Printf.ksprintf Lwt.fail_with
        "Token %S not found on the server before upload."
        (Token.to_string token)
  | e -> Lwt.fail e

let check_server_version server =
  Lwt.catch (fun () ->
      fetch server (Api.Version ()) >|= fun server_version ->
      if server_version <> Api.version then
        (Printf.eprintf "API version mismatch: client v.%s and server v.%s\n"
           Api.version server_version;
         exit 1))
  @@ fun e ->
  Printf.eprintf "[ERROR] Could not reach server: %s\n"
    (match e with
     | Unix.Unix_error (err, _, _) -> Unix.error_message err
     | Failure m -> m
     | e -> Printexc.to_string e);
  exit 1

let init ?(local=false) ?server ?token () =
  let path = if local then ConfigFile.local_path else ConfigFile.user_path in
  let default_server = Uri.of_string "http://learn-ocaml.org" in
  let server =
    match server with
    | Some s -> s
    | None ->
        Printf.eprintf
          "Please specify the address of the learn-ocaml server to use \
           [default: %s]: " (Uri.to_string default_server);
        let uri s =
          let u = Uri.of_string s in
          match Uri.scheme u with
          | None -> Uri.with_scheme u (Some "http")
          | Some ("http" (* | "https" *)) -> u
          | Some s ->
              failwith (Printf.sprintf
                          "unsupported scheme %S, please use http://."
                          s)
        in
        Console.input ~default:default_server uri
  in
  let get_new_token nickname =
    Printf.printf "Please provide the secret: ";
    match Console.input ~default:None (fun s -> Some s) with
    | Some secret_candidate ->
       fetch server (Api.Create_token (Sha.sha512 secret_candidate, None, nickname))
    | None -> failwith "Please provide a secret"
  in
  let get_token () =
    match token with
    | Some t -> Lwt.return t
    | None ->
        Printf.eprintf
          "Please provide your user token on %s (leave empty to generate one): %!"
          (Uri.to_string server);
        match
          Console.input ~default:None
            (fun s -> Some (Token.parse s))
        with
        | Some t -> Lwt.return t
        | None ->
            Printf.eprintf "Please enter a nickname: %!";
            get_new_token
              (Console.input
                 (fun s -> if String.length s < 2 then None else Some s))
  in
  check_server_version server >>=
  get_token >>= fun token ->
  let config = { ConfigFile. server; token } in
  ConfigFile.write path config >|= fun () ->
  Printf.eprintf "Configuration written to %s\n%!" path;
  config

let get_config ?local ?(save_back=false) server_opt token_opt =
  match ConfigFile.path ?local () with
  | Some f ->
      ConfigFile.read f >>= fun c ->
      let c = match server_opt with
        | None -> c
        | Some server -> { c with ConfigFile.server }
      in
      let c = match token_opt with
        | None -> c
        | Some token -> { c with ConfigFile.token}
      in
      check_server_version c.ConfigFile.server
      >>= fun () ->
      (
        if save_back
        then
          ConfigFile.write f c >|= fun () ->
          Printf.eprintf "Configuration written to %s\n%!" f
        else
          Lwt.return_unit
      )
      >|= fun () -> c
  | None -> init ?local ?server:server_opt ?token:token_opt ()

let man p = [
    `S "DESCRIPTION";
    `P p;
    `S "OPTIONS";
    `S "AUTHORS";
    `P "Learn OCaml is written by OCamlPro. Its main authors are Benjamin Canou, \
        Çağdaş Bozman, Grégoire Henry and Louis Gesbert. It is licensed under \
        the MIT License.";
    `S "BUGS";
    `P "Bugs should be reported to \
        $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
  ]

let get_config_o ?save_back o =
  let open Args in
  get_config ~local:o.local ?save_back o.server_url o.token

module Grade = struct
  open Args
  let grade go eo =
    Console.enable_colors := eo.Args.color;
    Console.enable_utf8 := eo.Args.color;
    get_config_o go
    >>= fun { ConfigFile.server; token } ->
    let status_line =
      if eo.verbosity >= 2 then Printf.eprintf "%s..\n" else Console.status_line
    in
    let solution, exercise_id =
      match eo.solution_file, eo.exercise_id with
      | None, _ -> Printf.eprintf "You must specify a file to grade.\n%!"; exit 2
      | Some f, Some id -> f, id
      | Some f, None ->
         let id = Filename.remove_extension f in
         f, id
    in
    status_line "Reading solution.";
    Lwt_io.with_file ~mode:Lwt_io.Input solution Lwt_io.read
    >>= fun solution ->
    status_line "Fetching exercise data from server.";
    fetch_exercise server token exercise_id
    >>= fun (_meta, exercise, deadline) ->
    if deadline = Some 0. then
      Printf.eprintf
        "[ERROR] The deadline is expired, you won't be able to submit.\n";
    Grading_cli.get_grade ~callback:status_line ?timeout:None
      exercise solution
    >>= fun (report, ex_stdout, ex_stderr, ex_outcome) ->
    flush stderr;
    let pr col title s =
      if eo.verbosity >= 1 then
        let s = String.trim s in
        if s <> "" then
          prerr_string (Console.block ~title ~border_color:[col] s)
    in
    pr `Green "stdout" ex_stdout;
    pr `Red "stderr" ex_stderr;
    pr `Cyan "outcome" ex_outcome;
    if eo.verbosity >= 1 then prerr_newline ();
    match report with
    | Error e ->
       let str =
         match Grading.string_of_exn e with
         | Some s -> s
         | None   -> Printexc.to_string e
       in
       Printf.eprintf "[ERROR] Could not do the grading:\n%s\n" str;
       Lwt.return 10
    | Ok report ->
       (match eo.output_format with
        | `Console -> console_report ~verbose:(eo.verbosity > 0) exercise report
        | `Raw ->
           Report.print Format.std_formatter report
        | `Html ->
           Report.output_html Format.std_formatter report
        | `Json ->
           match Json_encoding.construct Report.enc report
           with
           | `O _ | `A _ as json -> Ezjsonm.to_channel ~minify:false stdout json
           | _ -> assert false);
       if deadline = Some 0. then
         (Printf.eprintf "Results NOT saved to server (deadline expired)\n";
          Lwt.return 1)
       else
         upload_report server token exercise solution report >>= fun _ ->
         Printf.eprintf "Results saved to server\n";
         Lwt.return 0

  let man =
    man
      "Grades an OCaml exercise using a learn-ocaml server, and submits \
        solutions."

  let cmd =
    Cmdliner.Term.(
      const (fun go eo -> Pervasives.exit (Lwt_main.run (grade go eo)))
      $ Args.term_global $ Args.term_exercises),
    Cmdliner.Term.info ~version ~man
      ~doc:"Learn-ocaml grading client"
      "grade"
end

module Print_token = struct
  let print_tok o =
    get_config_o o
    >>= fun config ->
    Lwt_io.print (Token.to_string config.ConfigFile.token ^ "\n")
    >|= fun () -> 0

  let man = man "Just print the configured user token and exit"

  let cmd =
    Cmdliner.Term.(
      const (fun o -> Pervasives.exit (Lwt_main.run (print_tok o)))
      $ Args.term_global),
    Cmdliner.Term.info ~version ~man
      ~doc:"Just print the configured user token and exit"
      "print-token"
end

module Set_options = struct
  let set_opts o =
    get_config_o ~save_back:true o
    >|= fun _ -> 0

  let man =
    man
      "Overwrite the configuration file with the command-line options \
       ($(b,--server), $(b,--token)), and exit"

  let cmd =
    Cmdliner.Term.(
      const (fun o -> Pervasives.exit (Lwt_main.run (set_opts o)))
      $ Args.term_global),
    Cmdliner.Term.info ~version ~man
      ~doc:"Set local configuration and exit"
      "set-options"
end

module Fetch = struct
  let fetch_save server_url token =
    Lwt.catch (fun () -> fetch server_url (Api.Fetch_save token))
    @@ function
      | Not_found ->
         Printf.ksprintf Lwt.fail_with
           "Token %S not found on the server."
           (Token.to_string token)
      | e -> Lwt.fail e

  let fetch o =
    get_config_o o
    >>= fun { ConfigFile.server; token } ->
    fetch_save server token
    >>= write_save_files
    >|= fun () -> 0

  let man =
    man
      "Fetch the user's solutions on the server to the current directory and exit"

  let cmd =
    Cmdliner.Term.(
      const (fun o -> Pervasives.exit (Lwt_main.run (fetch o)))
      $ Args.term_global),
    Cmdliner.Term.info ~version ~man
      ~doc:"Fetch the user's solutions"
      "fetch"
end

module Main = struct
  let man =
    man
      "Learn-ocaml-client, default action is grading"

  let cmd =
    Cmdliner.Term.(
      const (fun go eo -> Pervasives.exit (Lwt_main.run (Grade.grade go eo)))
      $ Args.term_global $ Args.term_exercises),
    Cmdliner.Term.info ~version ~man
      ~doc:"Learn-ocaml grading client"
      "learn-ocaml-client"
end

let () =
  match Cmdliner.Term.eval_choice ~catch:false Main.cmd
          [Grade.cmd; Print_token.cmd; Set_options.cmd; Fetch.cmd]
  with
  | exception Failure msg ->
      Printf.eprintf "[ERROR] %s\n" msg;
      exit 1
  | `Error _ -> exit 2
  | _ -> exit 0
