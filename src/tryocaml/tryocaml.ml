(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
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

open Js_utils
open Tyxml_js

module H = Tryocaml_history

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let map_option f = function
  | None -> None
  | Some o -> Some (f o)
let iter_option f o = match o with | None -> () | Some o -> f o

type 'a result = Success of 'a | Timeout of float

type t = {
  timeout_delay: float;
  mutable timeout_prompt: t -> unit Lwt.t;
  mutable current_timeout_prompt: unit Lwt.t;
  flood_limit: int;
  mutable flood_prompt: t -> Html5_types.nmtoken -> (unit -> int) -> bool Lwt.t;
  mutable current_flood_prompt: unit Lwt.t;
  flood_reset: t -> unit;
  worker: Tryocaml_worker.t;
  container: [ `Div ] Html5.elt;
  oldify: bool;
  mutable status: [ `Reset of (unit Lwt.t * unit Lwt.u) | `Execute of unit Lwt.t | `Idle ] ;
  mutable on_enable_input: t -> unit;
  mutable on_disable_input: t -> unit;
  mutable disabled : int;
  output: Tryocaml_output.output;
  input: Tryocaml_input.input;
}

let set_timeout_prompt t f = t.timeout_prompt <- f
let set_flood_prompt t f = t.flood_prompt <- f
let set_on_enable_input t f = t.on_enable_input <- f
let set_on_disable_input t f = t.on_disable_input <- f

let trim s =
  let ws c = c = ' ' || c = '\t' || c = '\n' in
  let len = String.length s in
  let start = ref 0 in
  let stop = ref (len - 1) in
  while !start < len && (ws s.[!start])
  do incr start done;
  while !stop > !start && (ws s.[!stop])
  do decr stop done;
  String.sub s !start (!stop - !start + 1)

let disable_input top =
  top.disabled <- top.disabled + 1 ;
  if top.disabled = 1 then begin
    top.on_disable_input top ;
    Tryocaml_input.disable top.input
  end

let enable_input top =
  top.disabled <- top.disabled - 1 ;
  if top.disabled = 0 then begin
    top.on_enable_input top ;
    Tryocaml_input.enable top.input
  end

let scroll { output } =
  Tryocaml_output.scroll output

let clear { output } =
  Tryocaml_output.clear output ;
  Tryocaml_output.output_stdout output
    "The toplevel has been cleared.\n"

let never_ending =
  let t = fst (Lwt.wait ()) in
  fun _ -> t

let wait_for_prompts top =
  Lwt.join
    [ Lwt.catch
        (fun () -> top.current_timeout_prompt)
        Lwt.(function Canceled -> return () | exn -> fail exn) ;
      Lwt.catch
        (fun () -> top.current_flood_prompt)
        Lwt.(function Canceled -> return () | exn -> fail exn) ]

let start_timeout top name timeout =
  Lwt.cancel top.current_timeout_prompt ;
  match timeout with
  | Some timeout -> timeout top
  | None ->
      Lwt_js.sleep top.timeout_delay >>= fun () ->
      wait_for_prompts top >>= fun () ->
      top.current_timeout_prompt <- top.timeout_prompt top ;
      top.current_timeout_prompt

let reset_with_timeout top ?timeout () =
  match top.status with
  | `Reset (t, _) -> t
  | `Idle ->
      let t, u = Lwt.wait () in
      Lwt.cancel top.current_timeout_prompt ;
      Lwt.cancel top.current_flood_prompt ;
      wait_for_prompts top >>= fun () ->
      top.status <- `Reset (t, u) ;
      let timeout () = start_timeout top "reset" timeout in
      disable_input top;
      Tryocaml_worker.reset ~timeout top.worker () >>= fun () ->
      t
  | `Execute task ->
      let t, u = Lwt.wait () in
      Lwt.cancel top.current_timeout_prompt ;
      Lwt.cancel top.current_flood_prompt ;
      wait_for_prompts top >>= fun () ->
      top.status <- `Reset (t, u) ;
      let timeout () = start_timeout top "reset" timeout in
      disable_input top;
      Tryocaml_worker.reset ~timeout top.worker () >>= fun () ->
      t >>= fun () ->
      Lwt.cancel task ;
      Lwt.return ()

let reset top =
  let timeout _ = Lwt_js.sleep 2. in
  reset_with_timeout top ~timeout ()

let protect_execution top exec =
  wait_for_prompts top >>= fun () ->
  match top.status with
  | `Reset _ | `Execute _ ->
      Lwt.fail_invalid_arg "Tryocaml.protect_execution"
  | `Idle ->
      let t, u = Lwt.task () in
      top.status <- `Execute t ;
      disable_input top;
      top.flood_reset top ;
      let thread = t >>= fun () ->
        Lwt.catch
          (fun () ->
             exec () >>= fun res ->
             match top.status with
             | `Execute t' ->
                 assert (t == t') ;
                 top.status <- `Idle;
                 wait_for_prompts top >>= fun () ->
                 enable_input top;
                 Lwt.return res
             | `Idle | `Reset _ ->
                 (* The task successfully ended between a reset order
                    and its ack, we fake its cancelation. *)
                 Lwt.fail Lwt.Canceled)
          (function
            | Lwt.Canceled ->
                enable_input top;
                begin match top.status with
                  | `Reset (t, _) -> t
                  | _ -> Lwt.return ()
                end >>= fun () -> Lwt.fail Lwt.Canceled
            | exn ->
                enable_input top;
                Lwt.fail exn) in
      Lwt.wakeup u () ;
      thread

let execute_phrase top ?timeout content =
  let phrase = Tryocaml_output.phrase () in
  let pp_code = Tryocaml_output.output_code ~phrase top.output in
  let pp_answer = Tryocaml_output.output_answer ~phrase top.output in
  let t =
    Tryocaml_worker.execute
      top.worker ~pp_code ~pp_answer ~print_outcome:true content in
  Lwt.pick [
    (Lwt.protected t >>= fun _ -> Lwt.return_unit) ;
    (start_timeout top "execute" timeout >>= fun () ->
     let timeout _ = Lwt.return () in
     reset_with_timeout top ~timeout ());
  ] >>= fun () ->
  t >>= fun result ->
  let warnings, result = match result with
    | Toploop_results.Ok (result, warnings) -> warnings, result
    | Toploop_results.Error (error, warnings) ->
        Tryocaml_output.output_error ~phrase top.output error ;
        warnings, false in
  List.iter
    (Tryocaml_output.output_warning ~phrase top.output)
    warnings ;
  Lwt.return result

let execute top =
  Tryocaml_input.execute top.input

let go_backward top =
  Tryocaml_input.go_backward top.input

let go_forward top =
  Tryocaml_input.go_forward top.input

let check top code =
  protect_execution top @@ fun () ->
  Tryocaml_worker.check top.worker code

let set_checking_environment top =
  protect_execution top @@ fun () ->
  Tryocaml_worker.set_checking_environment top.worker >>= fun _ ->
  Lwt.return ()

let execute_phrase top ?timeout content =
  protect_execution top @@ fun () ->
  execute_phrase top ?timeout content

let load top ?(print_outcome = true) ?timeout ?message content =
  let phrase = Tryocaml_output.phrase () in
  protect_execution top @@ fun () ->
  begin match message with
    | None -> ()
    | Some message ->
        Tryocaml_output.output_code ~phrase top.output
          ("(* " ^ message ^ "*)")
  end ;
  let pp_answer =
    if print_outcome then
      Tryocaml_output.output_answer ~phrase top.output
    else
      ignore in
  let t =
    Tryocaml_worker.use_string
      top.worker ~pp_answer ~print_outcome content in
  Lwt.pick [
    (Lwt.protected t >>= fun _ -> Lwt.return_unit) ;
    (start_timeout top "load" timeout >>= fun () ->
     reset top);
  ] >>= fun () ->
  t >>= fun result ->
  let warnings, result = match result with
    | Toploop_results.Ok (result, warnings) -> warnings, result
    | Toploop_results.Error (error, warnings) ->
        Tryocaml_output.output_error top.output error ;
        warnings, false in
  List.iter
    (Tryocaml_output.output_warning top.output)
    warnings ;
  Lwt.return result

let make_timeout_popup
    ?(countdown = 10)
    ?(refill_step = 10)
    ?(on_show = (fun () -> ()))
    () { container } =
  let open Tyxml_js.Html5 in
  let t0 = Sys.time () in
  let countdown = ref countdown in
  let btn_continue =
    let label = Format.asprintf "%d seconds!" refill_step in
    button [ pcdata label ] in
  let btn_stop =
    button [ pcdata "Kill it!" ] in
  Manip.Ev.onclick btn_continue
    (fun _ -> countdown := !countdown + refill_step ; true) ;
  Manip.Ev.onclick btn_stop
    (fun _ -> countdown := 0 ; true) ;
  let clock_span = span [] in
  let countdown_span = span [] in
  let dialog =
    div ~a: [ a_class [ "dialog-container" ] ]
      [ div ~a: [ a_class [ "dialog" ] ]
          [ h1 [ pcdata "Infinite loop?" ] ;
            div ~a: [ a_class [ "message" ] ]
              [ pcdata "The toplevel has not been responding for " ;
                clock_span ;
                pcdata " seconds." ;
                br () ;
                pcdata "It will be killed in " ;
                countdown_span ;
                pcdata " seconds." ] ;
            div ~a: [ a_class [ "buttons" ] ]
              [ btn_continue ; btn_stop ] ] ] in
  Lwt.catch
    (fun () ->
       Manip.appendChild container dialog ;
       on_show () ;
       let rec loop () =
         let elapsed = int_of_float (Sys.time () -. t0) in
         Manip.replaceChildren clock_span
           [ pcdata (string_of_int elapsed) ] ;
         Manip.replaceChildren countdown_span
           [ pcdata (string_of_int (!countdown - elapsed)) ] ;
         if elapsed >= !countdown then begin
           Manip.removeChild container dialog ;
           Lwt.return ()
         end else
           Lwt_js.sleep 0.2 >>= loop in
       loop ())
    (fun exn ->
       (try Manip.removeChild container dialog with _ -> ()) ;
       Lwt.fail exn)

let make_flood_popup
    ?(on_show = (fun () -> ()))
    () { container } name amount =
  let open Tyxml_js.Html5 in
  let answer = ref None in
  let btn_continue =
    button [ pcdata "Show anyway!" ] in
  let btn_stop =
    button [ pcdata "Hide output!" ] in
  Manip.Ev.onclick btn_continue
    (fun _ -> answer := Some false ; true) ;
  Manip.Ev.onclick btn_stop
    (fun _ -> answer := Some true ; true) ;
  let qty_span = span [] in
  let dialog =
    div ~a: [ a_class [ "dialog-container" ] ]
      [ div ~a: [ a_class [ "dialog" ] ]
          [ h1 [ pcdata "Flooded output!" ] ;
            div ~a: [ a_class [ "message" ] ]
              [ pcdata "Your code is flooding the " ;
                pcdata name ;
                pcdata " channel. " ;
                br ();
                pcdata "It has already printed " ;
                qty_span ;
                pcdata " bytes." ] ;
            div ~a: [ a_class [ "buttons" ] ]
              [ btn_continue ; btn_stop ] ] ] in
  Manip.appendChild container dialog ;
  on_show () ;
  let rec loop () =
    Manip.replaceChildren qty_span
      [ pcdata (string_of_int (amount ())) ] ;
    match !answer with
    | Some ans ->
        Manip.removeChild container dialog ;
        Lwt.return ans
    | None ->
        Lwt_js.sleep 0.2 >>= loop in
  Lwt.catch
    loop
    (fun exn ->
       Manip.removeChild container dialog ;
       Lwt.fail exn)

let wrap_flusher_to_prevent_flood top name hook real =
  let flooded = ref 0 in
  hook := fun s ->
    real s ;
    let total = !flooded + String.length s in
    if total >= top.flood_limit then begin
      let buf = Buffer.create top.flood_limit in
      hook := (fun s -> try flooded := !flooded + String.length s ; Buffer.add_string buf s with _ -> ()) ;
      flooded := total ;
      Lwt.async @@ fun () ->
      Lwt.catch
        (fun () ->
           wait_for_prompts top >>= fun () ->
           top.current_flood_prompt <-
             (top.flood_prompt top name (fun () -> !flooded) >>= function
               | true ->
                   real ("\nInterrupted output channel " ^ name ^ ".\n") ;
                   hook := ignore ;
                   Lwt.return ()
               | false ->
                   real (Buffer.contents buf) ;
                   hook := real ;
                   Lwt.return ()) ;
           top.current_flood_prompt)
        (fun exn ->
           hook := ignore ;
           Lwt.return ())
    end else begin
      flooded := total
    end

let welcome_phrase =
  "Printf.printf \"Welcome to OCaml %s\\n%!\" (Sys.ocaml_version) ;\
   print_endline \" - type your OCaml phrase in the box below and press [Enter]\" ;\
   print_endline \" - use [Shift-Enter] to break lines without triggering execution\" ;\
   print_endline \" - use [Ctrl-↑] once to reuse the previous entry\" ;\
   print_endline \" - use [Ctrl-↑] / [Ctrl-↓] to navigate through history\" ;;"

let create
    ?worker_js_file
    ?(timeout_delay = 5.)
    ~timeout_prompt
    ?(flood_limit = 8000)
    ~flood_prompt
    ?after_init
    ?(input_sizing =
      { Tryocaml_input.line_height = 18 ;
        min_lines = 1 ; max_lines = 6 })
    ?on_resize
    ?(on_disable_input = fun _ -> ())
    ?(on_enable_input = fun _ -> ())
    ?history
    ?(oldify = true)
    ?(display_welcome = true)
    ~container () =
  let output_div = Html5.div [] in
  let input_div = Html5.div [] in
  Manip.appendChild container output_div;
  Manip.appendChild container input_div;
  let output =
    Tryocaml_output.setup
      ?on_resize
      ~container:output_div
      () in
  let execute_hook = ref (fun _code -> assert false) in
  let input =
    Tryocaml_input.setup
      ~sizing: input_sizing
      ~execute: (fun code -> !execute_hook code)
      ~container:input_div
      ?on_resize
      ?history
      () in
  let pp_stdout_hook = ref ignore in
  let pp_stdout s = !pp_stdout_hook s in
  let pp_stderr_hook = ref ignore in
  let pp_stderr s = !pp_stderr_hook s in
  let flood_reset top =
    let phrase = Tryocaml_output.phrase () in
    Lwt.cancel top.current_flood_prompt ;
    wrap_flusher_to_prevent_flood top
      "stdout" pp_stdout_hook
      (Tryocaml_output.output_stdout ~phrase output) ;
    wrap_flusher_to_prevent_flood top
      "stderr" pp_stderr_hook
      (Tryocaml_output.output_stderr ~phrase output) in
  Tryocaml_worker.create
    ?js_file:worker_js_file
    ~pp_stdout ~pp_stderr () >>= fun worker ->
  let top = {
    timeout_prompt;
    current_timeout_prompt = Lwt.return ();
    timeout_delay;
    flood_prompt;
    current_flood_prompt = Lwt.return ();
    flood_limit;
    flood_reset;
    worker;
    container;
    oldify;
    status = `Reset (Lwt.wait ());
    on_enable_input;
    on_disable_input;
    disabled = 1;
    input;
    output;
  } in
  flood_reset top ;
  execute_hook :=
    (fun code -> Lwt.async @@ fun () ->
      Lwt.catch
        (fun () -> execute_phrase top code)
        (function
          | Lwt.Canceled -> Lwt.return true
          | exn -> Lwt.fail exn )) ;
  let first_time = ref true in
  let after_init top =
    if !first_time || not oldify then
      Tryocaml_output.clear output
    else
      Tryocaml_output.oldify output;
    enable_input top ;
    top.flood_reset top;
    begin match top.status with
      | `Reset (_, u) -> Lwt.wakeup u ()
      | `Idle | `Execute _ -> assert false
    end ;
    top.status <- `Idle;
    begin if display_welcome && (!first_time || not oldify) then
        Tryocaml_worker.execute
          ~pp_answer: (fun _ -> ())
          ~print_outcome: false
          worker welcome_phrase >>= fun _ ->
        Lwt.return ()
      else Lwt.return ()
    end >>= fun _ ->
    if not !first_time then
      let phrase = Tryocaml_output.phrase () in
      Tryocaml_output.output_stdout output ~phrase
        "The toplevel has been reset.\n"
    else
      first_time := false ;
    Tryocaml_worker.register_callback worker "print_html"
      (Tryocaml_output.output_html output) >>= fun _ ->
    match after_init with
    | None -> Lwt.return_unit
    | Some f -> f top in
  after_init top >>= fun () ->
  Tryocaml_worker.set_after_init top.worker (fun _ -> after_init top);
  Lwt.return top

let print_string { output } = Tryocaml_output.output_stdout output

let prerr_string { output } = Tryocaml_output.output_stderr output

let print_html { output } = Tryocaml_output.output_html output
