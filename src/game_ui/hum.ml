open Std
open React
open Batteries
open Printf

(* SDL initialization *)
let _ =
  Sdl.init( [`VIDEO ; `EVENTTHREAD] ) ;
  at_exit Sdl.quit ;
  Sdlttf.init ();
  at_exit Sdlttf.quit(*;*)
(*  Sdlkey.enable_key_repeat () ~delay:500 ~interval:50 *)


module Video = Video.Make(struct end)
module UI = Frui.Make(Video)

(* User interface definition *)
let k x = S.const x

let ui status (module Tk : UI.Toolkit) =
  let open Tk in

  let topbar status = 
    let open WindowID in
    let bt_help,_,bt_help_clicks = button (k "Help")
    and bt_game,_,bt_game_clicks = button (k "Game")
    and bt_display,_,bt_display_clicks = button (k "Display")
    and bt_esc,_,bt_esc_clicks = button (k "ESC") in
    vpack
      ~w:(k `expands) ~h:(k `expands)
      ~spacing:(k `justified)
      [
	hpack
	  ~w:(k `expands)
	  ~spacing:(k `justified) ~valign:(k `top)
	  [ bt_esc ; bt_help ; bt_game ; bt_display ] ;
	void ~h:(k `expands) ()
      ],
    E.select [
      E.stamp bt_esc_clicks     [`wOpen (Quit, Default)] ;
      E.stamp bt_help_clicks    [`wOpen (Help, Default)] ;
      E.stamp bt_game_clicks    [`wOpen (Game, Default)] ;
      E.stamp bt_display_clicks [`wOpen (Display, Default)] ;
    ]
  in

  let topbar, topbar_e = topbar () in
  [
    window `topbar (k topbar) ;
  ],
  E.never,
  E.select [ topbar_e ]



let update_status old = function _ -> old

let status, ui =
  let initial_status = Status.create () in
  let define status =
    let ui, _, tasks = UI.make (ui status) in
    let status' = S.fold update_status initial_status tasks in
    status', (status', ui)
  in
  S.fix initial_status define



(* let queen status = Windows.queen (Status.windows status) *)
(* let active_window status = Windows.activeWindow (Status.windows status) *)

(* let difff f s =  *)
(*   S.diff f s |> E.fmap identity *)

(* let window_focus_event wid status = *)
(*   difff *)
(*     (fun olds news ->  *)
(*       match active_window olds = Some wid, active_window news = Some wid with *)
(* 	| true, false -> Some `focus *)
(* 	| false, true -> Some `unfocus *)
(* 	| _ -> None) *)
(*     status *)

(* let window_state_signal wid status =  *)
(*   let state_of_status status =  *)
(*     if queen status = Some wid then `active else `inactive *)
(*   in *)
(*   S.map state_of_status status *)

(* let esc_window status =  *)
(*   let open UI in *)
(*   let bt_quit,_,bt_quit_clicks = button ~shortcut:Sdlkey.KEY_q ~w:(c `expands) (c"Quit game") *)
(*   and bt_cancel,_,bt_cancel_clicks = button ~shortcut:Sdlkey.KEY_c ~w:(c `expands) (c"Cancel")  *)
(*   and focus = window_focus_event WindowID.Quit status in *)
(*   frame ~focus ( *)
(*     vpack [ *)
(*       label (c"Quit") ; *)
(*       bt_quit ; *)
(*       bt_cancel *)
(*     ] *)
(*   ), *)
(*   E.select [ *)
(*     E.stamp bt_quit_clicks [`quit] ; *)
(*     E.stamp bt_cancel_clicks [`wClose WindowID.Quit] *)
(*   ] *)

(* let help_window status =  *)
(*   let open UI in *)
(*   let bt_quit,_,bt_quit_clicks = button ~shortcut:Sdlkey.KEY_q ~w:(c `expands) (c"Q : Quit Humanitas") *)
(*   and bt_help,_,bt_help_clicks = button ~shortcut:Sdlkey.KEY_h ~w:(c `expands) (c"H : Open this help window")  *)
(*   and state = window_state_signal WindowID.Help status *)
(*   and focus = window_focus_event WindowID.Help status in *)
(*   frame ~focus ( *)
(*     vpack [ *)
(*       label (c"Getting started") ; *)
(*       bt_help ; *)
(*       bt_quit ; *)
(*     ] *)
(*   ), *)
(*   E.select [ *)
(*     E.stamp bt_quit_clicks [`wOpen WindowID.(Quit, Default)] ; *)
(*     E.stamp bt_help_clicks [] ; *)
(*   ] *)


(* let ui, effects = UI.( *)
(*   let main status =  *)
(*     let topbar, topbar_tasks = topbar status *)
(*     and esc_window, esc_window_tasks = esc_window status *)
(*     and help_window, help_window_tasks = help_window status in *)
(*     overlay [ *)
(*       vpack [ topbar ; void ~h:(c `expands) () ] ; *)
(*       esc_window ; *)
(*       help_window ; *)
(*     ], *)
(*     E.select [ *)
(*       topbar_tasks ; *)
(*       esc_window_tasks ; *)
(*       help_window_tasks ; *)
(*     ] *)
(*   in *)
(*   let ui, tasks = main status in *)
(*   make (S.const ui), E.trace (List.iter push) tasks *)
(* ) *)

let sdl_mouse_button = Sdlmouse.(
  function
  | BUTTON_LEFT -> Some `left
  | BUTTON_MIDDLE -> Some `middle
  | BUTTON_RIGHT -> Some `right
  | _ -> None
)

(* when a key is pressed, it is first sent to the UI, which answers if
   the UI used it. If not the following function is called *)
let global_keyboard_shortcut =
  let open Sdlkey in
      let open WindowID in  
	  function
	    | KEY_q -> [`wOpen (Quit, Default)]
	    | KEY_F1 -> [`wOpen (Help, Default)]
	    | _ -> []

(* Main loop *)
let _ = 
  let quit = ref false in
  while not !quit do
    (* Affichage *)
    UI.draw ui ;
    Video.flip () ;

    (* Traitement des évènements SDL *)
    Sdlevent.(
      match wait_event() with

      | KEYDOWN { keysym = Sdlkey.KEY_ESCAPE } -> exit 0
      (* | KEYDOWN key ->  *)
      (* 	ignore (UI.keyboard_event ui key.keysym `press) *)

      (* | KEYUP key ->  *)
      (* 	if not (UI.keyboard_event ui key.keysym `release) *)
      (* 	then List.iter push (global_keyboard_shortcut key.keysym) *)

      (* | MOUSEMOTION _ -> () *)
      (* | MOUSEBUTTONUP e | MOUSEBUTTONDOWN e -> *)
      (* 	Option.may *)
      (* 	  (fun b ->  *)
      (* 	    let s = match e.mbe_state with  *)
      (* 	      | PRESSED -> `pressed *)
      (* 	      | RELEASED -> `released in *)
      (* 	    let _ = UI.mouse_button_event ui b s ~x:e.mbe_x ~y:e.mbe_y in *)
      (* 	    ()) *)
      (* 	  (sdl_mouse_button e.mbe_button) *)
	
      | _ -> ()
    ) ;
  done
