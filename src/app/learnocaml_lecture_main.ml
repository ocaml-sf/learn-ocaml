(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_data

module H = Tyxml_js.Html

let init_tabs, select_tab =
  let names = [ "text" ; "toplevel" ; "editor" ] in
  let current = ref "text" in
  let select_tab name =
    set_arg "tab" name ;
    Manip.removeClass
      (find_component ("learnocaml-exo-button-" ^ !current))
      "front-tab" ;
    Manip.removeClass
      (find_component ("learnocaml-exo-tab-" ^ !current))
      "front-tab" ;
    Manip.enable
      (find_component ("learnocaml-exo-button-" ^ !current)) ;
    Manip.addClass
      (find_component ("learnocaml-exo-button-" ^ name))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-exo-tab-" ^ name))
      "front-tab" ;
    Manip.disable
      (find_component ("learnocaml-exo-button-" ^ name)) ;
    current := name in
  let init_tabs () =
    current := begin try
        let requested = arg "tab" in
        if List.mem requested names then requested else "text"
      with Not_found -> "text"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-exo-button-" ^ name))
           "front-tab" ;
         Manip.removeClass
           (find_component ("learnocaml-exo-tab-" ^ name))
           "front-tab" ;
         Manip.Ev.onclick
           (find_component ("learnocaml-exo-button-" ^ name))
           (fun _ -> select_tab name ; true))
      names ;
    select_tab !current in
  init_tabs, select_tab

let display_descr ex_meta =
  let open Tyxml_js.Html5 in
  let open Learnocaml_data.Exercise in
  match ex_meta.Meta.short_description with
  | None -> div ~a:[ a_class [ "descr" ] ] []
  | Some descr ->
      div ~a:[ a_class [ "descr" ] ] [
        h2 ~a:[ a_class [ "learnocaml-exo-meta-category" ] ]
          [ pcdata ex_meta.Meta.title ] ;
        p [ pcdata descr ] ;
      ]

let display_stars ex_meta =
  let open Tyxml_js.Html5 in
  let open Learnocaml_data.Exercise in
  let stars =
    let num = 5 * int_of_float (ex_meta.Meta.stars *. 2.) in
    let num = max (min num 40) 0 in
    let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
    let src = Format.asprintf "/icons/stars_%02d.svg" num in
    img ~alt ~src ()
  in
  div ~a:[ a_class [ "stars" ] ] [
    p [
      pcdata [%i "Difficulty:"];
      pcdata " "; (* lets not add whitespaces into translation strings (double
                     colon are mandatory, though, since rules are not the same
                     in english or french for example). *)
      stars
    ]
  ]

let display_kind ex_meta =
  let open Tyxml_js.Html5 in
  let open Learnocaml_data.Exercise in
  let kind_repr = string_of_exercise_kind ex_meta.Meta.kind in
  div ~a:[ a_class [ "length" ] ] [
    p [ pcdata (Format.sprintf [%if "Kind: %s"] kind_repr) ]
  ]

let exercise_link ?(cl = []) id content =
  let open Tyxml_js.Html5 in
  a ~a:[ a_href ("/lectures/"^id^"/") ;
         a_class cl ;
       ]
    content

let display_exercise_meta id meta content_id =
  let content = find_component content_id in
  let descr =
    exercise_link ~cl:[ "exercise" ] id [
      display_descr meta ;
      H.div ~a:[  ] [
        display_stars meta ;
        display_kind meta ;
      ]
    ]
  in
  Manip.replaceChildren content [ descr ];
  Lwt.return ()

let display_list ?(sep=Tyxml_js.Html5.pcdata ", ") l =
  let open Tyxml_js.Html5 in
  let rec gen acc = function
    | [] -> [ pcdata "" ]
    | a :: [] -> a :: acc
    | a :: ((_ :: _) as rem) ->
        gen (sep :: (a  :: acc)) rem
  in
  gen [] l |> List.rev

let get_skill_index token =
  let index = lazy (
    retrieve (Learnocaml_api.Exercise_index token)
    >|= fun (index, _) ->
    Exercise.Index.fold_exercises (fun (req, focus) id meta ->
        let add sk id map =
          SMap.add sk
            (SSet.add id (try SMap.find sk map with Not_found -> SSet.empty))
            map
        in
        List.fold_left (fun acc sk -> add sk id acc) req
          meta.Exercise.Meta.requirements,
        List.fold_left (fun acc sk -> add sk id acc) focus
          meta.Exercise.Meta.focus
      ) (SMap.empty, SMap.empty) index
  ) in
  fun skill ->
    Lazy.force index >|= fun (req, focus) ->
    try match skill with
      | `Requirements s -> SSet.elements (SMap.find s req)
      | `Focus s -> SSet.elements (SMap.find s focus)
    with Not_found -> []

let display_skill_meta _skill exs content_id =
  let content = find_component content_id in
  Manip.replaceChildren content
    (display_list @@
     List.map (fun ex_id ->
         exercise_link ex_id [Tyxml_js.Html5.pcdata ex_id]) exs);
  Lwt.return ()

let display_link onclick content_id value =
  let open Tyxml_js.Html5 in
  let cid = Format.asprintf "%s-%s" content_id value in
  let expand_id = Format.asprintf "%s-expand" cid in
  let displayed = ref false in
  let onclick _ =
    let exp = find_component expand_id in
    if not (!displayed) then
      begin
        ignore @@ onclick cid;
        displayed := true;
        Manip.removeChildren exp;
        Manip.appendChild exp (pcdata "[-]")
      end
    else
      begin
        Manip.removeChildren (find_component cid);
        displayed := false;
        Manip.removeChildren exp;
        Manip.appendChild exp (pcdata "[+]")
      end;
    true
  in
  div [
    p ~a:[ a_class [ "learnocaml-exo-expandable-link" ];
           a_onclick onclick;
         ]
      [
        span ~a:[ a_id expand_id; a_class ["expand-sign"] ] [ pcdata "[+]" ];
        pcdata value
      ] ;
    div ~a:[a_id cid;
            a_class [ "learnocaml-exo-meta-category" ] ] [] ]

let display_skill_link index content_id s =
  let skill = match s with `Focus s | `Requirements s -> s in
  display_link (display_skill_meta s index) content_id skill

let display_exercise_link content_id meta e =
  display_link (display_exercise_meta e meta) content_id e

let display_authors authors =
  let open Tyxml_js.Html5 in
  let author (name, mail) =
    span [ pcdata name;
           pcdata " <";
           a ~a:[ a_href ("mailto:" ^ mail) ]
             [ pcdata mail ];
           pcdata ">"
         ] in
  display_list @@ List.map author authors

let set_string_translations () =
  let translations = [
    "learnocaml-exo-button-editor", [%i"Editor"];
    "learnocaml-exo-button-toplevel", [%i"Toplevel"];
    "learnocaml-exo-button-text", [%i"Lecture"];
    "learnocaml-exo-editor-pane", [%i"Editor"];
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations

let is_readonly = ref false

let make_readonly () =
  is_readonly := true;
  alert ~title:[%i"TIME'S UP"]
    [%i"The deadline for this exercise has expired. Any changes you make \
        from now on will remain local only."]

let local_save ace id =
  let key = Learnocaml_local_storage.exercise_state id in
  let ans =
    try Learnocaml_local_storage.retrieve key with Not_found ->
      Answer.{solution = ""; mtime = 0.; report = None; grade = None}
  in
  Learnocaml_local_storage.store key
    { ans with Answer.solution = Ace.get_contents ace;
               mtime = gettimeofday () }


let () =
  Lwt.async_exception_hook := begin fun e ->
    Firebug.console##log (Js.string
                            (Printexc.to_string e ^
                             if Printexc.backtrace_status () then
                               Printexc.get_backtrace ()
                             else ""));
    match e with
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  Lwt.async @@ fun () ->
  set_string_translations ();
  Learnocaml_local_storage.init () ;
  let token =
    try
      Learnocaml_local_storage.(retrieve sync_token) |>
      Lwt.return
    with Not_found ->
      ask_string ~title:"Secret"
        [H.pcdata [%i"Enter the secret"]]
      >>= fun secret ->
      retrieve (Learnocaml_api.Create_token (Sha.sha512 secret, None, None))
      >|= fun token ->
      Learnocaml_local_storage.(store sync_token) token;
      token
  in
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let nickname_div = find_component "learnocaml-nickname" in
  (match Learnocaml_local_storage.(retrieve nickname) with
   | nickname -> Manip.setInnerText nickname_div nickname
   | exception Not_found -> ());
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let id = match Url.Current.path with
    | "" :: "lectures" :: p | "lectures" :: p ->
        String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
    | _ -> arg "id"
  in
  Dom_html.document##.title :=
    Js.string (id ^ " - " ^ "Learn OCaml" ^" v."^ Learnocaml_api.version);
  let exercise_fetch =
    token >>= fun token ->
    retrieve (Learnocaml_api.Exercise (token, id))
  in
  let after_init top =
    exercise_fetch >>= fun (_meta, exo, _deadline) ->
    begin match Learnocaml_exercise.(decipher File.prelude exo) with
      | "" -> Lwt.return true
      | prelude ->
          Learnocaml_toplevel.load ~print_outcome:true top
            ~message: [%i"loading the prelude..."]
            prelude
    end >>= fun r1 ->
    Learnocaml_toplevel.load ~print_outcome:false top
      (Learnocaml_exercise.(decipher File.prepare exo)) >>= fun r2 ->
    if not r1 || not r2 then failwith [%i"error in prelude"] ;
    Learnocaml_toplevel.set_checking_environment top >>= fun () ->
    Lwt.return () in
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup
      ~on_show: (fun () -> select_tab "toplevel")
      () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup
      ~on_show: (fun () -> select_tab "toplevel")
      () in
  let history =
    let storage_key =
      Learnocaml_local_storage.exercise_toplevel_history id in
    let on_update self =
      Learnocaml_local_storage.store storage_key
        (Learnocaml_toplevel_history.snapshot self) in
    let snapshot =
      Learnocaml_local_storage.retrieve storage_key in
    Learnocaml_toplevel_history.create
      ~gettimeofday
      ~on_update
      ~max_size: 99
      ~snapshot () in
  let toplevel_launch =
    create_toplevel
      ~after_init ~timeout_prompt ~flood_prompt
      ~on_disable_input: (fun _ -> disable_button_group toplevel_buttons_group)
      ~on_enable_input: (fun _ -> enable_button_group toplevel_buttons_group)
      ~container:(find_component "learnocaml-exo-toplevel-pane")
      ~history () in
  init_tabs () ;
  toplevel_launch >>= fun top ->
  exercise_fetch >>= fun (ex_meta, exo, deadline) ->
  (match deadline with
   | None -> ()
   | Some 0. -> make_readonly ()
   | Some t ->
       match Manip.by_id "learnocaml-countdown" with
       | Some elt -> countdown elt t ~ontimeout:make_readonly
       | None -> ());
  let solution =
    try Some (Learnocaml_local_storage.(retrieve (exercise_state id))).Answer.solution with
    | Not_found -> None in
  (* ---- toplevel pane ------------------------------------------------- *)
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin toplevel_button
      ~icon: "reload" [%i"Reset"] @@ fun () ->
    toplevel_launch >>= fun top ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;
  (* ---- text pane ----------------------------------------------------- *)
  let text_container = find_component "learnocaml-exo-tab-text" in
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren text_container
    Tyxml_js.Html5.[ h1 [ pcdata ex_meta.Exercise.Meta.title ] ;
                     Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
  let prelude = Learnocaml_exercise.(decipher File.prelude exo) in
  if prelude <> "" then begin
    let open Tyxml_js.Html5 in
    let state = ref (match arg "prelude" with
        | exception Not_found -> true
        | "shown" -> true
        | "hidden" -> false
        | _ -> failwith "Bad format for argument prelude.") in
    let prelude_btn = button [] in
    let prelude_title = h1 [ pcdata [%i"OCaml prelude"] ;
                             prelude_btn ] in
    let prelude_container =
      pre ~a: [ a_class [ "toplevel-code" ] ]
        (Learnocaml_toplevel_output.format_ocaml_code prelude) in
    let update () =
      if !state then begin
        Manip.replaceChildren prelude_btn [ pcdata ("↳ "^[%i"Hide"]) ] ;
        Manip.SetCss.display prelude_container "" ;
        set_arg "prelude" "shown"
      end else begin
        Manip.replaceChildren prelude_btn [ pcdata ("↰ "^[%i"Show"]) ] ;
        Manip.SetCss.display prelude_container "none" ;
        set_arg "prelude" "hidden"
      end in
    update () ;
    Manip.Ev.onclick prelude_btn
      (fun _ -> state := not !state ; update () ; true) ;
    Manip.appendChildren text_container
      [ prelude_title ; prelude_container ]
  end ;
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       d##open_;
       d##write (Js.string (exercise_text ex_meta exo));
       d##close) ;
  (* ---- editor pane --------------------------------------------------- *)
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in
  Ace.set_contents ace ~reset_undo:true
    (match solution with
     | Some solution -> solution
     | None -> Learnocaml_exercise.(access File.template exo)) ;
  Ace.set_font_size ace 18;
  begin editor_button
      ~icon: "cleanup" [%i"Reset"] @@ fun () ->
    confirm ~title:[%i"START FROM SCRATCH"]
      [H.pcdata [%i"This will discard all your edits. Are you sure?"]]
      (fun () ->
         Ace.set_contents ace (Learnocaml_exercise.(access File.template exo)));
    Lwt.return ()
  end ;
  begin editor_button
      ~icon: "sync" [%i"Sync"] @@ fun () ->
    token >>= fun token ->
    sync_exercise token id ~editor:(Ace.get_contents ace) >|= fun _save -> ()
  end ;
  begin editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
    let name = id ^ ".ml" in
    let contents = Js.string (Ace.get_contents ace) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  end ;
  begin editor_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval code"] @@ fun () ->
    Learnocaml_toplevel.execute_phrase top (Ace.get_contents ace) >>= fun _ ->
    select_tab "toplevel";
    Lwt.return_unit
  end ;
  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor error warnings  >>= fun () ->
    Ace.focus ace ;
    Lwt.return () in
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string "/index.html#activity=exercises") ;
    Lwt.return ()
  end ;
  Window.onunload (fun _ev -> local_save ace id; true);
  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  Lwt.return ()
