(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
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

open Lwt.Infix

let version = "0.1"

module Args = struct
  open Cmdliner
  open Arg

  type t = {
    server_url: Uri.t option;
    solution_file: string option;
    exercise_id: string option;
    output_format: [`Console|`Json|`Html|`Raw];
    submit: bool;
    color: bool;
    verbosity: int;
    token: Learnocaml_sync.Token.t option;
    local: bool;
    set_options: bool;
    print_token: bool;
    fetch: bool;
    version: bool;
  }

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
         try Ok (Learnocaml_sync.Token.parse s)
         with Failure msg ->
           Error (`Msg (Printf.sprintf "Invalid token %s: %s" s msg))),
      (fun fmt t -> Format.pp_print_string fmt (Learnocaml_sync.Token.to_string t))
    )

  let server_url =
    value & opt (some url_conv) None &
    info ["s";"server"] ~docv:"URL" ~doc:
      "The URL of the learn-ocaml server"
      ~env:(Cmdliner.Term.env_info "LEARNOCAML_SERVER" ~doc:
            "Sets the learn-ocaml server URL. Overriden by $(b,--server).")

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
              "Sets the learn-ocaml user token on the sever. Overriden by \
               $(b,--token).")

  let local =
    value & flag & info ["local"] ~doc:
      "Generate a configuration file local to the current directory, rather \
       than user-wide"

  let set_options =
    value & flag & info ["set-options"] ~doc:
      "Overwrite the configuration file with the command-line options \
       ($(b,--server), $(b,--token)), and exit"

  let print_token =
    value & flag & info ["print-token"] ~doc:
      "Just print the configured user token and exit"

  let fetch =
    value & flag & info ["fetch"] ~doc:
      "Fetch the user's solutions on the server to the current directory and exit"

  let version =
    value & flag & info ["version"] ~doc:
      "Print the version of this program and exit"

  let term =
    let apply
        server_url solution_file exercise_id output_format dont_submit
        color_when verbose token local set_options print_token fetch version =
      let color = match color_when with
        | Some o -> o
        | None -> Unix.(isatty stdout) && Sys.getenv_opt "TERM" <> Some "dumb"
      in
      {
        server_url;
        solution_file;
        exercise_id;
        output_format;
        submit = not dont_submit;
        color;
        verbosity = List.length verbose;
        token;
        local;
        set_options;
        print_token;
        fetch;
        version;
      }
    in
    Term.(const apply
          $server_url $solution_file $exercise_id $output_format $dont_submit
          $color_when $verbose $token $local $set_options $print_token $fetch
          $version)
end

module ConfigFile = struct

  type t = {
    server: Uri.t;
    token: Learnocaml_sync.Token.t;
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
      (req "token" Learnocaml_sync.Token.(conv to_string parse string))

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

let max_score exercise =
  Learnocaml_exercise.(access File.max_score exercise)

let print_score ?(max=1) ?color i =
  let color = match color with
    | None -> if i <= 0 then `Red else if i >= max then `Green else `Yellow
    | Some c -> c
  in
  if i <= 1 then
    Console.button color (Printf.sprintf " %3d pt  " i)
  else
    Console.button color (Printf.sprintf " %3d pts " i)

let console_report ?(verbose=false) exercise report =
  let open Console in
  let open Learnocaml_report in
  let score = get_score report in
  let max_score = max_score exercise in
  print_string (hline ());
  Printf.printf
    "## %-72s %s\n"
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
        | Code s when String.contains s '\n' -> block ~border_color:[`Cyan] s
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


let fetch url =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  Client.get url >>= fun (resp, body) ->
  match Response.status resp with
  | `OK -> Cohttp_lwt.Body.to_string body
  | status ->
      Printf.ksprintf Lwt.fail_with
        "Error contacting learn-ocaml server: code %d"
        (Code.code_of_status status)

let server_path server_url path =
  Uri.with_path server_url @@
  String.concat "/" (Uri.path server_url :: path)

let fetch_exercise server_url id =
  let exercise_json_url =
    server_path server_url ["exercises"; id ^ ".json"]
  in
  fetch exercise_json_url >|=
  Ezjsonm.from_string >|=
  Json_encoding.destruct Learnocaml_exercise.enc

let fetch_save server_url token =
  let sync_url =
    server_path server_url ["sync"; Learnocaml_sync.Token.to_string token]
  in
  fetch sync_url >|= fun s ->
  let s = if s = "" then "{}" else s in
  Ezjsonm.from_string s |>
  Json_encoding.destruct Learnocaml_sync.save_file_enc

let upload_save server_url token save =
  let sync_url =
    server_path server_url ["sync"; Learnocaml_sync.Token.to_string token]
  in
  let json =
    match Json_encoding.construct Learnocaml_sync.save_file_enc save with
    | `A _ | `O _ as d -> d
    | v -> `A [ v ]
  in
  let body = `String (Ezjsonm.to_string json) in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  Client.post ~body sync_url >>= fun (resp, _body) ->
  match Response.status resp with
  | `OK -> Lwt.return_unit
  | status ->
      Printf.ksprintf Lwt.fail_with
        "Could not upload the results to the server: code %d"
        (Code.code_of_status status)

module StringMap = Map.Make(String)

let write_save_files save =
  Lwt_list.iter_s (fun (id, st) ->
      let f = Filename.concat (Sys.getcwd ()) (id ^ ".ml") in
      if Sys.file_exists f then
        (Printf.eprintf "File %s already exists, not overwriting.\n" f;
         Lwt.return_unit)
      else
        Lwt_io.(with_file ~mode:Output ~perm:0o600 f) @@ fun oc ->
        Lwt_io.write oc st.Learnocaml_exercise_state.solution >|= fun () ->
        Printf.eprintf "Wrote file %s\n%!" f)
    (StringMap.bindings (save.Learnocaml_sync.all_exercise_states))

let upload_report server token exercise solution report =
  let score = get_score report in
  let max_score = max_score exercise in
  let exercise_state =
    { Learnocaml_exercise_state.
      solution;
      grade = if max_score = 0 then None else Some (score * 100 / max_score);
      report = Some report;
      mtime = Unix.gettimeofday ();
    }
  in
  let new_save =
    { Learnocaml_sync.
      all_exercise_states =
        StringMap.singleton (Learnocaml_exercise.(access File.id) exercise)
          exercise_state;
      all_toplevel_histories = StringMap.empty;
      all_exercise_toplevel_histories = StringMap.empty;
    }
  in
  fetch_save server token >>= fun save ->
  upload_save server token (Learnocaml_sync.sync save new_save)

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
  let get_new_token () =
    let url = server_path server ["sync"; "gimme"] in
    fetch url >|=
    Ezjsonm.from_string >|=
    Json_encoding.(
      destruct @@ conv
        Learnocaml_sync.Token.to_string Learnocaml_sync.Token.parse
        (obj1 (req "token" string)))
  in
  let get_token () =
    match token with
    | Some t -> Lwt.return t
    | None ->
        Printf.eprintf
          "Please provide your user token on %s (leave empty to generate one): "
          (Uri.to_string server);
        match
          Console.input ~default:None
            (fun s -> Some (Learnocaml_sync.Token.parse s))
        with
        | Some t -> Lwt.return t
        | None -> get_new_token ()
  in
  get_token () >>= fun token ->
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
      (if save_back then
         ConfigFile.write f c >|= fun () ->
         Printf.eprintf "Configuration written to %s\n%!" f;
         exit 0
       else Lwt.return_unit)
      >|= fun () -> c
  | None -> init ?local ?server:server_opt ?token:token_opt ()

let main o =
  Console.enable_colors := o.Args.color;
  Console.enable_utf8 := o.Args.color;
  if o.Args.version then
    (print_endline version; exit 0);
  let open Args in
  get_config ~local:o.local ~save_back:o.set_options o.server_url o.token
  >>= fun { ConfigFile.server; token } ->
  if o.print_token then
    (print_endline (Learnocaml_sync.Token.to_string token);
     exit 0);
  (if o.fetch then
     (fetch_save server token >>= write_save_files >>= fun () -> exit 0)
   else Lwt.return_unit) >>= fun () ->
  let status_line =
    if o.verbosity >= 2 then Printf.eprintf "%s..\n" else Console.status_line
  in
  let solution, exercise_id =
    match o.solution_file, o.exercise_id with
    | None, _ -> Printf.eprintf "You must specify a file to grade.\n%!"; exit 2
    | Some f, None -> f, Filename.(remove_extension (basename f))
    | Some f, Some id -> f, id
  in
  status_line "Reading solution.";
  Lwt_io.with_file ~mode:Lwt_io.Input solution Lwt_io.read
  >>= fun solution ->
  status_line "Fetching exercise data from server.";
  fetch_exercise server exercise_id
  >>= fun exercise ->
  Grading_cli.get_grade ~callback:status_line ?timeout:None
    exercise solution
  >>= fun (report, ex_stdout, ex_stderr, ex_outcome) ->
  flush stderr;
  let pr col title s =
    if o.verbosity >= 1 then
      let s = String.trim s in
      if s <> "" then
        prerr_string (Console.block ~title ~border_color:[col] s)
  in
  pr `Green "stdout" ex_stdout;
  pr `Red "stderr" ex_stderr;
  pr `Cyan "outcome" ex_outcome;
  if o.verbosity >= 1 then prerr_newline ();
  match report with
  | Error e ->
      Printf.eprintf "[ERROR] Could not do the grading: %s"
        (Printexc.to_string e);
      Lwt.return 10
  | Ok report ->
      (match o.output_format with
       | `Console -> console_report ~verbose:(o.verbosity > 0) exercise report
       | `Raw ->
           Learnocaml_report.print_report Format.std_formatter report
       | `Html ->
           Learnocaml_report.output_html_of_report Format.std_formatter report
       | `Json ->
           match Json_encoding.construct Learnocaml_report.report_enc report
           with
           | `O _ | `A _ as json -> Ezjsonm.to_channel stdout json
           | _ -> assert false);
      upload_report server token exercise solution report >>= fun () ->
      Printf.eprintf "Results saved to server\n";
      Lwt.return 0

let man = [
  `S "DESCRIPTION";
  `P "Grades an OCaml exercise using a learn-ocaml server, and submits \
      solutions.";
  `S "OPTIONS";
  `S "AUTHORS";
  `P "Learn OCaml is written by OCamlPro. Its main authors are Benjamin Canou, \
      Çağdaş Bozman and Grégoire Henry. It is licensed under the GNU Affero \
      General Public License version 3: any instance of the app must provide \
      its source code to its users.";
  `S "BUGS";
  `P "Bugs should be reported to \
      $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
]

let main_cmd =
  Cmdliner.Term.(
    const (fun o -> Pervasives.exit (Lwt_main.run (main o)))
    $ Args.term),
  Cmdliner.Term.info
    ~man
    ~doc:"Learn-ocaml grading client"
    "learn-ocaml-client"

let () =
  match Cmdliner.Term.eval ~catch:false main_cmd
  with
  | exception Failure msg ->
      Printf.eprintf "[ERROR] %s\n" msg;
      exit 1
  | `Error _ -> exit 2
  | _ -> exit 0
