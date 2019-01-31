(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Lwt.Infix
open Learnocaml_data

module H = Tyxml_js.Html

let find_div_or_append_to_body id =
  match Manip.by_id id with
  | Some div -> div
  | None ->
      let div = H.(div ~a:[ a_id id ]) [] in
      Manip.(appendChild Elt.body) div;
      div

let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)

let fake_download ~name ~contents =
  (* TODO: add some primitives to jsoo and clean this up  *)
  let blob : (Js.js_string Js.t Js.js_array Js.t -> File.blob Js.t) Js.constr =
    Js.Unsafe.global ##. _Blob in
  let blob = new%js blob (Js.array [| contents |]) in
  let url =
    Js.Unsafe.meth_call (Js.Unsafe.global##._URL) "createObjectURL" [| Js.Unsafe.inject blob |] in
  let link = Dom_html.createA Dom_html.document in
  link##.href := url ;
  Js.Unsafe.set link (Js.string "download") (Js.string name) ;
  ignore (Dom_html.document##.body##(appendChild ((link :> Dom.node Js.t)))) ;
  ignore (Js.Unsafe.meth_call link "click" [||]) ;
  ignore (Dom_html.document##.body##(removeChild ((link :> Dom.node Js.t))))

let fake_upload () =
  let input_files_load =
    Dom_html.createInput ~_type: (Js.string "file") Dom_html.document in
  let result_t, result_wakener = Lwt.wait () in
  let fail () =
    Lwt.wakeup_exn result_wakener
      (Failure "file loading not implemented for this browser") ;
    Js._true in
  input_files_load##.onchange := Dom.handler (fun ev ->
      Js.Opt.case (ev##.target) fail @@ fun target ->
      Js.Opt.case (Dom_html.CoerceTo.input target) fail @@ fun input ->
      Js.Optdef.case (input##.files) fail @@ fun files ->
      Js.Opt.case (files##(item (0))) fail @@ fun file ->
      let name = Js.to_string file##.name in
      let fileReader = new%js File.fileReader in
      fileReader##.onload := Dom.handler (fun ev ->
          Js.Opt.case (ev##.target) fail @@ fun target ->
          Js.Opt.case (File.CoerceTo.string (target##.result)) fail @@ fun result ->
          Lwt.wakeup result_wakener (name, result) ;
          Js._true) ;
      fileReader##(readAsText file) ;
      Js._true) ;
  ignore (Js.Unsafe.meth_call input_files_load "click" [||]) ;
  result_t

let fatal ?(title=[%i"INTERNAL ERROR"]) message =
  let titletext = title in
  let id = "ocp-fatal-layer" in
  let div = match Manip.by_id id with
    | Some div -> div
    | None ->
        let div =
          H.div ~a:[ H.a_id id ;
                     H.a_class ["learnocaml-dialog-overlay"]
                   ]
            []
        in
        Manip.(appendChild Elt.body) div;
        div in
  Manip.replaceChildren div [
    H.div [
      H.h3 [ H.pcdata titletext ];
      H.div [ H.p [ H.pcdata (String.trim message) ] ];
    ]
  ]

let dialog_layer_id = "ocp-dialog-layer"

let box_button txt f =
  H.button ~a: [
    H.a_onclick (fun _ ->
        f ();
        match Manip.by_id dialog_layer_id with
        | Some div -> Manip.removeChild Manip.Elt.body div; false
        | None -> (); false)
  ] [ H.pcdata txt ]

let close_button txt =
  box_button txt @@ fun () -> ()

let ext_alert ~title ?(buttons = [close_button [%i"OK"]]) message =
  let div = match Manip.by_id dialog_layer_id with
    | Some div -> div
    | None ->
        let div =
          H.div ~a:[ H.a_id dialog_layer_id ;
                     H.a_class ["learnocaml-dialog-overlay"] ]
            []
        in
        Manip.(appendChild Elt.body) div;
        div in
  Manip.replaceChildren div [
    H.div [
      H.h3 [ H.pcdata title ];
      H.div message;
      H.div ~a:[ H.a_class ["buttons"] ] buttons;
    ]
  ]

let lwt_alert ~title ~buttons message =
  let waiter, wakener = Lwt.task () in
  let buttons =
    List.map (fun (txt, f) ->
        box_button txt (fun () ->
            Lwt.async @@ fun () ->
            f () >|= Lwt.wakeup_later wakener))
      buttons
  in
  ext_alert ~title message ~buttons;
  waiter

let alert ?(title=[%i"ERROR"]) ?buttons message =
  ext_alert ~title ?buttons [ H.p [H.pcdata (String.trim message)] ]

let confirm ~title ?(ok_label=[%i"OK"]) ?(cancel_label=[%i"Cancel"]) contents f =
  ext_alert ~title contents ~buttons:[
    box_button ok_label f;
    close_button cancel_label;
  ]

let default_exn_printer = function
  | Failure msg -> msg
  | e -> Printexc.to_string e

let catch_with_alert ?(printer=default_exn_printer) f =
  Lwt.catch f @@ fun exn -> alert (printer exn); Lwt.return_unit

let hide_loading ?(id = "ocp-loading-layer") () =
  let elt = find_div_or_append_to_body id in
  Manip.(removeClass elt "initial") ;
  Manip.(removeClass elt "loading") ;
  Manip.(addClass elt "loaded")

let show_loading ?(id = "ocp-loading-layer") contents f =
  let show () =
    let elt = find_div_or_append_to_body id in
    Manip.(addClass elt "loading-layer") ;
    Manip.(removeClass elt "loaded") ;
    Manip.(addClass elt "loading") ;
    let chamo_src =
      "/icons/tryocaml_loading_" ^ string_of_int (Random.int 9 + 1) ^ ".gif" in
    Manip.replaceChildren elt
      H.[
        div ~a: [ a_id "chamo" ] [ img ~alt: "loading" ~src: chamo_src () ] ;
        div ~a: [ a_class [ "messages" ] ] contents
      ]
  in
  let hide () =
    let elt = find_div_or_append_to_body id in
    Manip.(removeClass elt "initial") ;
    Manip.(removeClass elt "loading") ;
    Manip.(addClass elt "loaded")
  in
  Lwt.finalize
    (fun () -> show (); f ())
    (fun () -> hide (); Lwt.return_unit)

let set_assoc name value =
  let rec set acc = function
    | [] -> List.rev ((name, value) :: acc)
    | (n, _) :: args when n = name ->
        List.rev_append ((name, value) :: acc) args
    | arg :: args -> set (arg :: acc) args in
  set []

let delete_assoc name =
  List.filter (fun (n, _) -> n <> name)

let arg, set_arg, delete_arg =
  let args = ref (Js_utils.parse_fragment ()) in
  let delete_arg name =
    args := delete_assoc name !args ;
    Js_utils.set_fragment !args in
  let set_arg name value =
    args := set_assoc name value !args ;
    Js_utils.set_fragment !args  in
  let arg name =
    List.assoc name !args in
  arg, set_arg, delete_arg

type button_group =
  (< disabled : bool Js.t Js.prop > Js.t * bool ref) list ref
  * Lwt_mutex.t
  * int ref

let button_group () : button_group =
  (ref [], Lwt_mutex.create (), ref 0)

type button_state =
  bool ref
  * (button_group * < disabled : bool Js.t Js.prop > Js.t) option ref

let button_state () : button_state =
  (ref false, ref None)

let disable_button_group (buttons, _, cpt) =
  incr cpt ;
  if !cpt = 1 then
    List.iter
      (fun (button, _) ->
         button##.disabled := Js.bool true)
      !buttons

let enable_button_group (buttons, _, cpt) =
  decr cpt ;
  if !cpt = 0 then
    List.iter
      (fun (button, state) ->
         if not !state then
           button##.disabled := Js.bool false)
      !buttons

let disable_button (disabled, self) =
  match !self with
  | None ->
      disabled := true
  | Some (_, button) ->
      disabled := true ;
      button##.disabled := Js.bool true

let enable_button (disabled, self) =
  match !self with
  | None ->
      disabled := false
  | Some ((_, _, cpt), button) ->
      disabled := false ;
      if !cpt = 0 then
        button##.disabled := Js.bool false

let button_group_disabled (_, _, cpt) =
  !cpt > 0

let disabling_button_group group cb =
  disable_button_group group ;
  Lwt_js.yield () >>= fun () ->
  Lwt.catch cb
    (function
      | Lwt.Canceled -> Lwt.return ()
      | exn -> Lwt.fail exn) >>= fun res ->
  enable_button_group group ;
  Lwt_js.yield () >>= fun () ->
  Lwt.return res

let disable_with_button_group component (buttons, _, _) =
  buttons :=
    ((component :> < disabled : bool Js.t Js.prop > Js.t), ref false)
    :: !buttons

let button ~container ~theme ?group ?state ~icon lbl cb =
  let (others, mutex, cnt) as group =
    match group with
    | None -> button_group ()
    | Some group -> group in
  let button =
    H.(button [
        img ~alt:"" ~src:("/icons/icon_" ^ icon ^ "_" ^ theme ^ ".svg") () ;
        pcdata " " ;
        span ~a:[ a_class [ "label" ] ] [ pcdata lbl ]
      ]) in
  Manip.Ev.onclick button
    (fun _ ->
       begin Lwt.async @@ fun () ->
         Lwt_mutex.with_lock mutex @@ fun () ->
         disabling_button_group group cb
       end ;
       true) ;
  let dom_button =
    (Tyxml_js.To_dom.of_button button
     :> < disabled : bool Js.t Js.prop > Js.t) in
  let self_disabled =
    match state with
    | None -> ref false
    | Some (disabled, self) ->
        self := Some (group, dom_button) ;
        disabled in
  others := (dom_button, self_disabled) :: !others ;
  if !self_disabled || !cnt > 0 then
    dom_button##.disabled := Js.bool true ;
  Manip.appendChild container button

let dropdown ~id ~title items =
    let toggle _ =
      let menu = find_component id in
      let disp =
        match Manip.Css.display menu with
        | "block" -> "none"
        | _ ->
            Lwt_js_events.async (fun () ->
                Lwt_js_events.click window >|= fun _ ->
                Manip.SetCss.display menu "none"
              );
            "block"
      in
      Manip.SetCss.display menu disp;
      false
    in
    H.div ~a: [H.a_class ["dropdown_btn"]] [
      H.button ~a: [H.a_onclick toggle]
        (title @ [H.pcdata " \xe2\x96\xb4" (* U+25B4 *)]);
      H.div ~a: [H.a_id id; H.a_class ["dropdown_content"]] items
    ]

let gettimeofday () =
  (new%js Js.date_now)##getTime /. 1000.

let render_rich_text ?on_runnable_clicked text =
  let open Learnocaml_data.Tutorial in
  let rec render acc text =
    match text with
    | [] -> List.rev acc
    | Text text :: rest ->
        render
          (H.pcdata text :: acc)
          rest
    | Code { code ; runnable } :: rest ->
        let elt = H.code [ H.pcdata code ] in
        (match runnable, on_runnable_clicked with
         | true, Some cb ->
             Manip.addClass elt "runnable" ;
             Manip.Ev.onclick elt (fun _ -> cb code ; true)
         | _ -> ()) ;
        render (elt :: acc) rest ;
    | Emph text :: rest ->
        render
          (H.em (render [] text) :: acc)
          rest
    | Image _ :: _ -> assert false
    | Math code :: rest ->
        render
          (H.pcdata ("`" ^ code ^ "`") :: acc)
          rest in
  (render [] text
   :> [< Html_types.phrasing > `Code `Em `PCDATA ] H.elt list)

let extract_text_from_rich_text text =
  let open Learnocaml_data.Tutorial in
  let rec render acc text =
    match text with
    | [] -> String.concat " " (List.rev acc)
    | Text text :: rest ->
        render (text :: acc) rest
    | Code { code ; _ } :: rest ->
        render (("[" ^ code ^ "]") :: acc) rest
    | Emph text :: rest ->
        render (("*" ^ render [] text ^ "*") :: acc) rest
    | Image { alt ; _ } :: rest ->
        render (("(" ^ alt ^ ")") :: acc) rest
    | Math code :: rest ->
        render (("$" ^ code ^ "$") :: acc) rest in
  render [] text

let set_state_from_save_file ?token save =
  let open Learnocaml_data.Save in
  let open Learnocaml_local_storage in
  match token with None -> () | Some t -> store sync_token t;
  store nickname save.nickname;
  store all_exercise_states
    (SMap.merge (fun _ ans edi ->
         match ans, edi with
         | Some ans, Some (mtime, solution) ->
             Some {ans with Answer.solution; mtime}
         | None, Some (mtime, solution) ->
             Some Answer.{grade = None; report = None; solution; mtime}
         | ans, _ -> ans)
        save.all_exercise_states save.all_exercise_editors);
  store all_toplevel_histories save.all_toplevel_histories;
  store all_exercise_toplevel_histories save.all_exercise_toplevel_histories

let rec retrieve ?ignore req =
  Server_caller.request req >>= function
  | Ok x -> Lwt.return x
  | Error e ->
      lwt_alert ~title:[%i"REQUEST ERROR"] [
        H.p [H.pcdata [%i"Could not retrieve data from server"]];
        H.code [H.pcdata (Server_caller.string_of_error e)];
      ] ~buttons:(
        ([%i"Retry"], (fun () -> retrieve req)) ::
        (match ignore with
         | None -> []
         | Some v -> [[%i"Ignore"], fun () -> Lwt.return v]) @
        [[%i"Cancel"], (fun () -> Lwt.fail Lwt.Canceled)]
      )

let get_state_as_save_file ?(include_reports = false) () =
  let open Learnocaml_data.Save in
  let open Learnocaml_local_storage in
  let answers = retrieve all_exercise_states in
  {
    nickname = retrieve nickname;
    all_exercise_editors =
      if include_reports then SMap.empty
      else SMap.map (fun a -> a.Answer.mtime, a.Answer.solution) answers;
    all_exercise_states =
      if include_reports then answers
      else SMap.empty;
    all_toplevel_histories = retrieve all_toplevel_histories;
    all_exercise_toplevel_histories = retrieve all_exercise_toplevel_histories;
  }

let rec sync_save token save_file =
  Server_caller.request (Learnocaml_api.Update_save (token, save_file))
  >>= function
  | Ok save -> set_state_from_save_file ~token save; Lwt.return save
  | Error (`Not_found _) ->
      Server_caller.request_exn
        (Learnocaml_api.Create_token (Some token, None)) >>= fun _token ->
      assert (_token = token);
      Server_caller.request_exn
        (Learnocaml_api.Update_save (token, save_file)) >>= fun save ->
      set_state_from_save_file ~token save;
      Lwt.return save
  | Error e ->
      lwt_alert ~title:[%i"SYNC FAILED"] [
        H.p [H.pcdata [%i"Could not synchronise save with the server"]];
        H.code [H.pcdata (Server_caller.string_of_error e)];
      ] ~buttons:[
        [%i"Retry"], (fun () -> sync_save token save_file);
        [%i"Ignore"], (fun () -> Lwt.return save_file);
      ]

let sync token = sync_save token (get_state_as_save_file ())

let sync_exercise token ?answer ?editor id =
  let nickname = Learnocaml_local_storage.(retrieve nickname) in
  let toplevel_history =
    SMap.find_opt id Learnocaml_local_storage.(retrieve all_toplevel_histories)
  in
  let txt = match editor with None -> None | Some e -> Some (max_float, e) in
  let opt_to_map = function
    | Some i -> SMap.singleton id i
    | None -> SMap.empty
  in
  let save_file = Save.{
    nickname;
    all_exercise_editors = opt_to_map txt;
    all_exercise_states = opt_to_map answer;
    all_toplevel_histories = SMap.empty;
    all_exercise_toplevel_histories = opt_to_map toplevel_history;
  } in
  Lwt.catch (fun () -> sync_save token save_file)
    (fun e ->
       (* save the text at least locally (but not the report & grade, that could
          be misleading) *)
       let txt = match editor, answer with
         | Some t, _ -> Some t
         | _, Some a -> Some a.Answer.solution
         | _ -> None
       in
       (match txt with
        | Some txt ->
            let key = Learnocaml_local_storage.exercise_state id in
            let a0 = Learnocaml_local_storage.retrieve key in
            Learnocaml_local_storage.store key
              {a0 with Answer.
                    solution = txt;
                    mtime = gettimeofday () }
        | None -> ());
       raise e)


let string_of_seconds seconds =
  let days = seconds / 24 / 60 / 60 in
  let hours = seconds / 60 / 60 mod 24 in
  let minutes = seconds / 60 mod 60 in
  let seconds = seconds mod 60 in
  if days >= 1 then Printf.sprintf [%if"%dd %02dh"] days hours else
  if hours >= 1 then Printf.sprintf [%if"%02d:%02d"] hours minutes else
    Printf.sprintf [%if"0:%02d:%02d"] minutes seconds

let countdown ?(ontimeout = fun () -> ()) container t =
  let deadline = gettimeofday () +. t in
  let update_interval seconds =
    if seconds >= 24 * 60 * 60 then 1000. *. 60. *. 60.
    else if seconds >= 60 * 60 then 1000. *. 60.
    else 1000.
  in
  let update remaining =
    Manip.setInnerText container (string_of_seconds remaining)
  in
  let rec callback () =
    let remaining = int_of_float (deadline -. gettimeofday ()) in
    if remaining <= 0 then
      (update 0;
       ontimeout ())
    else
      (update remaining;
       ignore (window##setTimeout
                 (Js.wrap_callback callback)
                 (update_interval remaining)))
  in
  callback ()

let flog fmt = Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) fmt

let stars_div stars =
  H.div ~a:[ H.a_class [ "stars" ] ] [
    let num = 5 * int_of_float (stars *. 2.) in
    let num = max (min num 40) 0 in
    let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
    let src = Format.asprintf "/icons/stars_%02d.svg" num in
    H.img ~alt ~src ()
  ]

let exercise_text ex_meta exo =
  let mathjax_url =
    "/js/mathjax/MathJax.js?delayStartupUntil=configured"
  in
  let mathjax_config =
    "MathJax.Hub.Config({\n\
    \  jax: [\"input/AsciiMath\", \"output/HTML-CSS\"],\n\
    \  extensions: [],\n\
    \  showMathMenu: false,\n\
    \  showMathMenuMSIE: false,\n\
    \  \"HTML-CSS\": {\n\
    \    imageFont: null\n\
    \  }
          });"
    (* the following would allow comma instead of dot for the decimal separator,
       but should depend on the language the exercise is in, not the language of the
       app
       "AsciiMath: {\n\
       \  decimal: \"" ^[%i"."]^ "\"\n\
        },\n"
    *)
  in
  (* Looking for the description in the correct language. *)
  let descr =
    let lang = "" in
    try
      List.assoc lang (Learnocaml_exercise.(access File.descr exo))
    with
      Not_found ->
        try List.assoc "" (Learnocaml_exercise.(access File.descr exo))
        with Not_found -> [%i "No description available for this exercise." ]
  in
  Format.asprintf
    "<!DOCTYPE html>\
     <html><head>\
     <title>%s - exercise text</title>\
     <meta charset='UTF-8'>\
     <link rel='stylesheet' href='/css/learnocaml_standalone_description.css'>\
     <script type='text/x-mathjax-config'>%s</script>
            <script type='text/javascript' src='%s'></script>\
     </head>\
     <body>\
     %s\
     </body>\
     <script type='text/javascript'>MathJax.Hub.Configured()</script>\
     </html>"
    ex_meta.Exercise.Meta.title
    mathjax_config
    mathjax_url
    descr

let string_of_exercise_kind = function
  | Exercise.Meta.Project -> [%i"project"]
  | Exercise.Meta.Problem -> [%i"problem"]
  | Exercise.Meta.Exercise -> [%i"exercise"]

let grade_color = function
  | None -> "#808080"
  | Some score ->
      Printf.sprintf "hsl(%d, 100%%, 67%%)"
        (int_of_float (float_of_int score /. 100. *. 138.))

let get_assignments tokens exos_status =
  let module ES = Exercise.Status in
  let module ATM = Map.Make(struct
      type t = (float * float) * Token.Set.t * bool
      let compare (d1, ts1, dft1) (d2, ts2, dft2) =
        match compare d1 d2 with
        | 0 -> (match Token.Set.compare ts1 ts2 with
            | 0 -> compare dft1 dft2
            | n -> n)
        | n -> n
    end)
  in
  let atm_add atm key id =
    match ATM.find_opt key atm with
    | None -> ATM.add key (SSet.singleton id) atm
    | Some set -> ATM.add key (SSet.add id set) atm
  in
  let atm =
    SMap.fold (fun id st atm ->
        let assg = st.ES.assignments in
        let default = ES.default_assignment assg in
        let stl = ES.by_status tokens assg in
        let atm = match default with
          | ES.Assigned {start; stop} ->
              let explicit_tokens =
                Token.Map.fold (fun tok _ -> Token.Set.add tok)
                  assg.ES.token_map Token.Set.empty
              in
              let implicit_tokens =
                Token.Set.diff tokens explicit_tokens
              in
              atm_add atm ((start, stop), implicit_tokens, true) id
          | _ -> atm
        in
        List.fold_left (fun atm (status, tokens) ->
            match status with
            | ES.Open | ES.Closed -> atm
            | ES.Assigned {start; stop} ->
                let key = (start, stop), tokens, (status = default) in
                match ATM.find_opt key atm with
                | None ->
                    ATM.add key (SSet.singleton id) atm
                | Some ids ->
                    ATM.add key (SSet.add id ids) atm)
          atm
          stl)
      exos_status
      ATM.empty
  in
  ATM.fold (fun (assg, tokens, dft) exos l ->
      (assg, tokens, dft, exos) :: l)
    atm []
  |> List.rev

let string_of_date ?(time=false) t =
  let date = new%js Js.date_fromTimeValue (t *. 1000.) in
  if time then
    Printf.sprintf "%04d-%02d-%02d %02d:%02d"
      date##getFullYear (date##getMonth + 1) date##getDate
      date##getHours date##getMinutes
  else
    Printf.sprintf "%04d-%02d-%02d"
      date##getFullYear (date##getMonth + 1) date##getDate

let date ?(time=false) t =
  let date = new%js Js.date_fromTimeValue (t *. 1000.) in
  H.time ~a:[ H.a_datetime (Js.to_string date##toISOString) ] [
    H.pcdata
      (Js.to_string (if time then date##toLocaleString
                     else date##toLocaleDateString))
  ]

let tag_span tag =
  let color =
    Printf.sprintf "#%06x" ((Hashtbl.hash tag lor 0x808080) land 0xffffff)
  in
  H.span ~a:[H.a_class ["tag"];
             H.a_style ("background-color: "^color)]
    [H.pcdata tag]

let get_worker_code name =
  let worker_url = ref None in
  fun () -> match !worker_url with
    | None ->
        retrieve (Learnocaml_api.Static ["js"; name]) >|= fun js ->
        let url = js_code_url js in worker_url := Some url; url
    | Some url -> Lwt.return url

let create_toplevel =
  let get_worker = get_worker_code "learnocaml-toplevel-worker.js" in
  fun
    ?display_welcome ?on_disable_input ?on_enable_input ?history ?after_init
    ~timeout_prompt ~flood_prompt ~container () ->
    get_worker () >>= fun worker_js_file ->
    Learnocaml_toplevel.create ~worker_js_file
      ?display_welcome ?on_disable_input ?on_enable_input ?history ?after_init
      ~timeout_prompt ~flood_prompt ~container ()
