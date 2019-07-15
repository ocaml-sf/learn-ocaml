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
open Grade_exercise
module H = Tyxml_js.Html

let init_tabs, select_tab =
  mk_tab_handlers "text"  [ "toplevel" ; "report" ; "editor"; "meta" ]

let check_if_need_refresh () =
  let local_server_id = Learnocaml_local_storage.(retrieve server_id) in
  retrieve @@ Learnocaml_api.Version ()
  >|= fun (_, server_id) ->
  if local_server_id <> server_id then
    let title = [%i "WARNING: You have an older grader version than the server"]
    and ok_label = [%i "Refresh the page"]
    and refresh () = Dom_html.window##.location##reload
    and cancel_label = [%i "I will do it myself!"]
    and message = [%i "The server has been updated, please refresh the page to make sure you are using the latest version of Learn-OCaml server (none of your work will be lost)."] in
    let contents = [ H.p [H.pcdata (String.trim message) ] ] in
  confirm ~title ~ok_label ~cancel_label contents refresh

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
  a ~a:[ a_href ("/exercises/"^id^"/") ;
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

let display_meta token ex_meta id =
  let open Learnocaml_data.Exercise in
  let open Tyxml_js.Html5 in
  let ident =
    Format.asprintf "%s %s" [%i "Identifier:" ] id in
  let opt l f = match l with [] -> None | l -> Some (f l) in
  let authors =
    opt ex_meta.Meta.author @@ fun author ->
    let title = match author with [_] -> [%i"Author"] | _ -> [%i"Authors"] in
    span [ pcdata title; pcdata " " ] ::
    display_authors author
  in
  retrieve (Learnocaml_api.Exercise_index token)
  >|= fun (index, _) ->
  let req_map, focus_map =
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
  in
  let focus =
    opt ex_meta.Meta.focus @@ fun focus ->
    [%i "Skills trained:"],
    display_list ~sep:(pcdata "") @@
    List.map (fun s ->
        display_skill_link
          (try SSet.elements (SMap.find s focus_map) with Not_found -> [])
          "learnocaml-exo-focus-meta" (`Focus s))
      focus in
  let requirements =
    opt ex_meta.Meta.requirements @@ fun requirements ->
    [%i "Skills required:"],
      display_list ~sep:(pcdata "") @@
      List.map (fun s ->
          display_skill_link
            (try SSet.elements (SMap.find s req_map) with Not_found -> [])
            "learnocaml-exo-requirements-meta" (`Requirements s))
        requirements in
  let backward =
    let l =
      List.fold_left (fun acc id ->
          match Exercise.Index.find_opt index id with
          | Some meta ->
              display_exercise_link "learnocaml-exo-backward-meta" meta id
              :: acc
          | None -> acc)
        []
        (List.rev ex_meta.Meta.backward)
    in
    opt l @@ fun l ->
    [%i"Previous exercises:"], display_list ~sep:(pcdata "") l
  in
  let forward =
    let l =
      List.fold_left (fun acc id ->
          match Exercise.Index.find_opt index id with
          | Some meta ->
              display_exercise_link "learnocaml-exo-backward-meta" meta id
              :: acc
          | None -> acc)
        []
        (List.rev ex_meta.Meta.forward)
    in
    opt l @@ fun l ->
    [%i"Next exercises:"], display_list ~sep:(pcdata "") l
  in
  let tab = find_component "learnocaml-exo-tab-meta" in
  Manip.replaceChildren tab @@
  Tyxml_js.Html5.([
    h1 ~a:[ a_class [ "learnocaml-exo-meta-title" ] ]
      [ pcdata [%i "Metadata" ] ] ;

    div ~a:[ a_id "learnocaml-exo-content-meta" ]
      (display_descr ex_meta ::
       display_stars ex_meta ::
       display_kind ex_meta ::
       p [ pcdata ident ] ::
       (match authors with Some a -> p a | None -> div []) ::
       List.map (function
           | Some (title, values) ->
               div
                 (h2 ~a:[ a_class [ "learnocaml-exo-meta-category-title" ] ]
                    [ pcdata title ] ::
                  values)
           | None -> div [])
         [ focus ; requirements ; backward ; forward ])
  ])

let is_readonly = ref false

let make_readonly () =
  is_readonly := true;
  alert ~title:[%i"TIME'S UP"]
    [%i"The deadline for this exercise has expired. Any changes you make \
        from now on will remain local only."]

(* experiment button of editor.html redirects to the html associated to this ml
   to know if we are in this page because of that we decide
   to put a '.' before the id
   therefore idEditor looks for a '.' before the id *)

let idEditor s =  not ((Regexp.string_match (Regexp.regexp "^[.]+") s 0) = None) 

  
let () =
  run_async_with_log @@ fun () ->
  set_string_translations_exercises ();
  Learnocaml_local_storage.init ();
  retrieve (Learnocaml_api.Version ())
  >|= fun (_,server_id) ->
    Learnocaml_local_storage.(store server_id) server_id;
  let token =
    try
      Learnocaml_local_storage.(retrieve sync_token) |>
      Lwt.return
    with Not_found ->
      retrieve (Learnocaml_api.Nonce ())
      >>= fun nonce ->
      ask_string ~title:"Secret"
        [H.pcdata [%i"Enter the secret"]]
      >>= fun secret ->
      retrieve
        (Learnocaml_api.Create_token (Sha.sha512 (nonce ^ Sha.sha512 secret), None, None))
      >|= fun token ->
      Learnocaml_local_storage.(store sync_token) token;
      token
  in
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let toplevel_button =
    button ~container: toplevel_toolbar ~theme: "dark" ~group:toplevel_buttons_group ?state:None in
  let id = match Url.Current.path with
    | "" :: "exercises" :: p | "exercises" :: p ->
        String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
    | _ -> arg "id"
  in
  Dom_html.document##.title :=
    Js.string (id ^ " - " ^ "Learn OCaml" ^" v."^ Learnocaml_api.version);

  (* if we came from a true exercise we search in the server.
   In the other case we get the exercise information from the Local storage *)
  let exercise_fetch = match idEditor id with
    | false -> token >>= fun token ->
               retrieve (Learnocaml_api.Exercise (token, id))
               
    | true -> let proper_id = String.sub id 1 ((String.length id)-1) in
              Lwt.return ((Editor_lib.get_editor_state proper_id).Editor.metadata,
                          (Editor_lib.exo_creator proper_id ),
                          None)
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
  let toplevel_launch =
    toplevel_launch ~after_init (find_component "learnocaml-exo-toplevel-pane")
      Learnocaml_local_storage.exercise_toplevel_history
      (fun () -> select_tab "toplevel") toplevel_buttons_group id
  in
  init_tabs () ;
  set_nickname_div ();
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
    match Learnocaml_local_storage.(retrieve (exercise_state id)) with
    | { Answer.report = Some report ; solution ; _ } ->
        let _ : int = display_report exo report in
        solution
    | { Answer.report = None ; solution ; _ } ->
        solution
    | exception Not_found -> Learnocaml_exercise.(access File.template exo) in
  (* ---- details pane -------------------------------------------------- *)
  let load_meta () =
    Lwt.async (fun () ->
        token >>= fun token ->
        display_meta token ex_meta id)
  in
  if arg "tab" = "meta" then load_meta () else
    Manip.Ev.onclick (find_component "learnocaml-exo-button-meta") (fun _ ->
        load_meta ();
        select_tab "meta";
        true);
  (* ---- toplevel pane ------------------------------------------------- *)
  init_toplevel_pane toplevel_launch top toplevel_buttons_group toplevel_button ;
  (* ---- text pane ----------------------------------------------------- *)
    let text_container = find_component "learnocaml-exo-tab-text" in
    let text_iframe = Dom_html.createIframe Dom_html.document in
    Manip.replaceChildren text_container
      Tyxml_js.Html5.[ h1 [ pcdata ex_meta.Exercise.Meta.title ] ;
                       Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
  (* ---- editor pane --------------------------------------------------- *)
  let editor, ace = setup_editor solution in
  let module EB = Editor_button (struct let ace = ace let buttons_container = editor_toolbar end) in
  EB.cleanup (Learnocaml_exercise.(access File.template exo));
  EB.sync token id;
  EB.download id;
  EB.eval top select_tab;
  let typecheck = typecheck top ace editor in
(*------------- prelude -----------------*)
  setup_prelude_pane ace Learnocaml_exercise.(decipher File.prelude exo);
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       d##open_;
       d##write (Js.string (exercise_text ex_meta exo));
       d##close) ;
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  let () =
    if idEditor id then
      begin
        let id = String.sub id 1 ((String.length id)-1) in
        begin toolbar_button
       ~icon: "upload" [%i"Edit"] @@ fun ()->
       Dom_html.window##.location##assign
       (Js.string ("editor.html#id=" ^ id ^ "&action=open"));
       Lwt.return_unit
        end;
      end
  in
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string "/index.html#activity=exercises") ;
    Lwt.return ()
  end ;
  let messages = Tyxml_js.Html5.ul [] in
  let callback text =
    Manip.appendChild messages Tyxml_js.Html5.(li [ pcdata text ]) in
  let worker =
    ref (get_grade ~callback exo)
  in
  
  begin toolbar_button
      ~icon: "typecheck" [%i"Compile"] @@ fun () ->
    typecheck true
  end;
  begin toolbar_button
      ~icon: "reload" [%i"Grade!"] @@ fun () ->
    check_if_need_refresh ()
    >>= fun () ->
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn = Tyxml_js.Html5.(button [ pcdata [%i"abort"] ]) in
      Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata " ?" ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata [%i"Launching the grader"] ] ] ;
    let submit_report = not !is_readonly in (* Don't count the grading time *)
    show_loading ~id:"learnocaml-exo-loading" [ messages ; abort_message ]
    @@ fun () ->
    Lwt_js.sleep 1. >>= fun () ->
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top solution >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
        let grading =
          Lwt.finalize
            (fun () ->
               !worker >>= fun w ->
               w solution >>= fun (report, _, _, _) ->
               Lwt.return report)
            (fun () ->
               worker := get_grade ~callback exo;
               Lwt.return_unit)
        in
        let abortion =
          Lwt_js.sleep 5. >>= fun () ->
          Manip.SetCss.opacity abort_message (Some "1") ;
          aborted >>= fun () ->
          Lwt.return Learnocaml_report.[ Message ([ Text [%i"Grading aborted by user."] ], Failure) ] in
        Lwt.pick [ grading ; abortion ] >>= fun report ->
        let grade = display_report exo report in
        let editor, answer =
          if submit_report then
            None,
            Some { Answer.grade = Some grade ;
                   solution ;
                   report = Some report ;
                   mtime = max_float } (* To ensure server time will be used *)
          else
            Some solution, None
        in
        token >>= fun token ->
        sync_exercise token id ?answer ?editor >>= fun _save ->
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        Ace.focus ace ;
        Lwt.return ()
    | Toploop_results.Error _ ->
        let msg =
          Learnocaml_report.[ Text [%i"Error in your code."] ; Break ;
                   Text [%i"Cannot start the grader if your code does not typecheck."] ] in
        let report = Learnocaml_report.[ Message (msg, Failure) ] in
        let grade = display_report exo report in
        Learnocaml_local_storage.(store (exercise_state id))
          { Answer.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } ;
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        Ace.focus ace ;
        typecheck true
  end ;
  Window.onunload (fun _ev -> local_save ace id; true);
  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  Lwt.return ()
;;
