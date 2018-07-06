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

module Args = struct
  open Cmdliner
  open Arg

  type t = {
    server_url: Uri.t;
    solution_file: string;
    exercise_id: string;
    output_format: [`Console|`Json|`Html|`Raw];
    submit: bool;
    color: bool;
    verbosity: int;
    token: string option;
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
         if Learnocaml_sync.Token.check s then Ok s
         else Error (`Msg ("Invalid token "^s))),
      (fun fmt s -> Format.pp_print_string fmt s)
    )

  let server_url =
    value & opt url_conv (Uri.of_string "https://learn-ocaml.org") &
    info ["s";"server"] ~docv:"URL" ~doc:
      "The URL of the learn-ocaml server"
      ~env:(Cmdliner.Term.env_info "LEARNOCAML_SERVER" ~doc:
            "Sets the learn-ocaml server URL. Overriden by $(b,--server).")

  let solution_file =
    required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:
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

  let term =
    let apply
        server_url solution_file exercise_id output_format dont_submit
        color_when verbose token =
      let exercise_id = match exercise_id with
        | Some id -> id
        | None -> Filename.(remove_extension (basename solution_file))
      in
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
        token
      }
    in
    Term.(const apply
          $server_url $solution_file $exercise_id $output_format $dont_submit
          $color_when $verbose $token)
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
  Learnocaml_exercise.(get max_score) exercise

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
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Printf.ksprintf Lwt.fail_with
        "Could not fetch data from server: code %d\n        %s"
        (Code.code_of_status status)
        body_str

let fetch_exercise server_url id =
  let exercise_json_url =
    Uri.with_path server_url
      (Printf.sprintf "%s/exercises/%s.json" (Uri.path server_url) id)
  in
  fetch exercise_json_url >|=
  Ezjsonm.from_string >|=
  Json_encoding.destruct Learnocaml_exercise.enc

let fetch_save server_url token =
  let sync_url =
    Uri.with_path server_url
      (Printf.sprintf "%s/sync/%s" (Uri.path server_url) token)
  in
  fetch sync_url >|= fun s ->
  let s = if s = "" then "{}" else s in
  Ezjsonm.from_string s |>
  Json_encoding.destruct Learnocaml_sync.save_file_enc

let upload_save server_url token save =
  let sync_url =
    Uri.with_path server_url
      (Printf.sprintf "%s/sync/%s" (Uri.path server_url) token)
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


let upload_report server token exercise solution report =
  let module M = Map.Make(String) in
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
        M.singleton (Learnocaml_exercise.(get id) exercise)
          exercise_state;
      all_toplevel_histories = M.empty;
      all_exercise_toplevel_histories = M.empty;
    }
  in
  fetch_save server token >>= fun save ->
  upload_save server token (Learnocaml_sync.sync save new_save)

open Args

let main o =
  Console.enable_colors := o.color;
  Console.enable_utf8 := o.color;
  let status_line =
    if o.verbosity >= 2 then Printf.eprintf "%s..\n" else Console.status_line
  in
  status_line "Reading solution.";
  Lwt_io.with_file ~mode:Lwt_io.Input o.solution_file Lwt_io.read
  >>= fun solution ->
  status_line "Fetching exercise data from server.";
  fetch_exercise o.server_url o.exercise_id
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
      match o.token with
      | None ->
          Printf.eprintf
            "[ERROR] No token specified, can not submit results to server.\n\
            \        Get one from the web service and define %s.\n"
            (Console.color [`Bold] "LEARNOCAML_TOKEN");
          Lwt.return 11
      | Some tok ->
          upload_report o.server_url tok exercise solution report >>= fun () ->
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
    $ term),
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
