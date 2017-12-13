(*

 ****************************** src/devel_ui/handler.ml ******************************


 *  This file is part of Humanitas.

 *  Humanitas is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version.

 *  Humanitas is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.

 *  You should have received a copy of the GNU General Public License
 *  along with Humanitas; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

 *)

open Std

module E = Sdlevent
module K = Sdlkey
module M = Sdlmouse
module RS  = React.S
module S   = Status
module SA  = Status.Atelier
module SS  = Scene
module W   = WindowID
module Ws  = Windows

type mods = {
  shift : bool;
  lalt : bool;
  ralt : bool;
  lctrl : bool;
  rctrl : bool;
  }

type event = 
| Mouse of [`press of ([`left | `middle | `right] as 'a) | `release of 'a] * int * int
| Key of Sdlkey.t * [`press | `release]

type t = mods * event

let mods keo (*keyboard_event option*) =
  let mod_state = match keo with
  | Some ke -> ke.Sdlevent.keymod
  | None    -> Sdlkey.get_mod_state() in
  let f key = boi(mod_state land key) in
  {
  shift = f Sdlkey.kmod_shift;
  lalt  = f Sdlkey.kmod_lalt;
  ralt  = f Sdlkey.kmod_ralt;
  lctrl = f Sdlkey.kmod_lctrl;
  rctrl = f Sdlkey.kmod_rctrl;
  }

let no_mod mods =
     mods.shift = false
  && mods.lalt  = false
  && mods.ralt  = false
  && mods.lctrl = false
  && mods.rctrl = false

let mouse_button = function
| M.BUTTON_LEFT   -> Some `left
| M.BUTTON_MIDDLE -> Some `middle
(*| M.BUTTON_WHEELDOWN -> Some `wheeldown*)
(*| M.BUTTON_WHEELUP -> Some `wheelup*)
| M.BUTTON_RIGHT  -> Some `right
| _ -> None


let mouse_state = function
| E.PRESSED  -> `pressed
| E.RELEASED -> `released

let rec next_event () = 
  match Sdlevent.wait_event() with
| E.KEYDOWN         ke -> mods (Some ke), Key   (ke.E.keysym, `press)
| E.KEYUP           ke -> mods (Some ke), Key   (ke.E.keysym, `release)
| E.MOUSEBUTTONDOWN me -> (
  match mouse_button me.E.mbe_button with
  | Some b -> mods None, (Mouse (`press b, me.E.mbe_x, me.E.mbe_y))
  | _ -> next_event()
)
| E.MOUSEBUTTONUP   me -> (
  match mouse_button me.E.mbe_button with
  | Some b -> mods None, (Mouse (`release b, me.E.mbe_x, me.E.mbe_y))
  | _ -> next_event()
)
| _ -> next_event()


(* when a key is pressed, it is first sent to the UI, which answers if
   the UI used it. If not the following function is called *)

let move e scene gr m k =
  let module E = Espace in
  let module ER = Espace.Regio in
  let module ED = Espace.Direction in
  let cr = Scene.cr ~e scene in
  let dir, kp = K.(match k with
    | KEY_KP8   -> ED.nord, true
    | KEY_UP    -> ED.nord, false
    | KEY_KP6   -> ED.east, true
    | KEY_RIGHT -> ED.east, false
    | KEY_KP2   -> ED.sud , true
    | KEY_DOWN  -> ED.sud , false
    | KEY_KP4   -> ED.west, true
    | _         -> ED.west, false) in
  match Scene.sr scene, (both_or_none kp m.lalt) with
    | Some sr, true -> ( let ib = Scene.GeoRect.is_borderline gr (ER.coords e cr) (ER.coords e sr) in match ib with
                         | Some pos when ( ED.includes pos dir ) -> [ `move_sr (dir, 1) ; `map_move(dir,1) ]
                         | _                                             -> [ `move_sr (dir, 1)  ] )
    | None   , true -> [ `select_regio (Some (Scene.cr ~e scene)) ]
    | _             -> [ `map_move(dir, (if m.shift then 1 else 6)) ]



let atelier_tasks atelier pick (m, nextEvent) =
  let scene = SA.scene atelier in
  let geoRect = SA.geoRect atelier in
  let pid   = SA.pid atelier in (* player id *)
  let game  = SA.game atelier in
  let pov   = Game.get_player_pov game pid in
  let orbis = Game.orbis game in
  let nil   = orbis.Orbis.natioIdList in
  let ior rid = match Im.incola orbis.Orbis.imperiumMap rid with None -> Nid.none | Some inc -> Rv.Incola.nid inc in
  let sfollowing = try (Tlist.following_or_first pov nil) with Failure _ -> List.hd nil in
  let sprevious  = try (Tlist.previous_or_first  pov nil) with Failure _ -> List.hd nil in
  match nextEvent with
  | Mouse (`release mb, x, y) -> (match mb, (pick x y) with
(*    | `right, Some (Picking.Regio rid)              -> [`wClose W.Regio; `select_regio None; `map_center rid ] *)
    | `right, Some (Picking.Regio rid)              -> [`map_center rid ] 
    | `left , Some (Picking.Regio rid) when m.lctrl -> [`select_regio (Some rid); `alter_player_pov (pid,(ior rid)) ]
    | `left , Some (Picking.Regio rid) when m.shift -> [`select_regio (Some rid); `wOpen (W.Regio,W.Default) ]
    | `left , Some (Picking.Regio rid)              -> [`select_regio (Some rid) ]
(*    | `left                            -> [`describe_element (mx, my) ]*)
(*    | `wheeldown when m.lalt -> [`angle_down ]*)
(*    | `wheelup   when m.lalt -> [`angle_up ]*)
(*    | `wheeldown             -> [`zoom_out ]*)
(*    | `wheelup               -> [`zoom_in ] *)
    | _                               -> [])
  | Key   (k, `release) -> K.(match k with
    | KEY_r      when Scene.sr scene  = None -> [`do_nothing ]
    | KEY_ESCAPE when Scene.sr scene <> None -> [`wClose W.Regio ; `select_regio None ]
    | KEY_HOME   when not m.rctrl            -> [`move_to_capitolium ]
    | KEY_RIGHT  when m.lctrl                -> [`alter_player_pov (pid,sfollowing ); `move_to_capitolium ]
    | KEY_LEFT   when m.lctrl                -> [`alter_player_pov (pid,sprevious  ); `move_to_capitolium ]
    | KEY_n      when m.lctrl                -> [`alter_player_pov (pid,Nid.none   ) ]
    | KEY_a     -> [`toggle_altitude]
    | KEY_b     -> [`toggle_borders]
    | KEY_e     -> [`toggle_earthMode]
    | KEY_f     -> [`switch_filter]
    | KEY_n     -> [`toggle_nations]
    | KEY_o                                  -> [`wOpen (W.Orbis, W.Default) ]
    | KEY_z
    | KEY_PLUS
    | KEY_KP_PLUS  -> [`zoom_in  ]
    | KEY_w
    | KEY_MINUS
    | KEY_KP_MINUS -> [`zoom_out ]
    | KEY_KP2
    | KEY_KP4
    | KEY_KP6
    | KEY_KP8
    | KEY_UP    
    | KEY_RIGHT
    | KEY_DOWN 
    | KEY_LEFT  -> ( move orbis.Orbis.espace scene geoRect m k )
    | KEY_RETURN   -> [`end_of_turn 1]
    | KEY_v        -> [`end_of_turn 5]
    | KEY_y        -> [`end_of_turn 10]
    | KEY_l        -> [`end_of_turn 50]
    | _ -> [])
  | _ -> []
  (* gestion tabula : fin *)
  

let task_list status pick (m, nextEvent) = 
  let ws = Status.windows status in
  let atelier_tasks = match Status.atelier status with 
  | Some atelier -> atelier_tasks atelier pick (m, nextEvent) 
  | _ -> []
  in match nextEvent, atelier_tasks with
  | Key (k, `release), [] ->
    K.(match k with
    | KEY_k        when (m.rctrl && m.lctrl) -> exit 0
    | KEY_HOME     when (m.rctrl && m.lctrl) -> [`switch_baby_mode false ]
    | KEY_END      when (m.rctrl && m.lctrl) -> [`switch_baby_mode false ; `quit ]
    | KEY_TAB   -> [`sFocus (Some W.Alter)]
    | KEY_F11   -> [`switch_fullscreen]
    | KEY_q     -> [`wOpen (W.Quit, W.Default)]
    | KEY_k     -> [`wOpen (W.Keys, W.Default)]
    | KEY_h     -> [`sHide W.Left; `sHide W.Right]
    | KEY_t     -> [`wOpen (W.TaskHistory, W.Default)]
    | KEY_SLASH    -> [`screenSizeAlter (1. /. 1.10)]
    | KEY_ASTERISK -> [`screenSizeAlter  1.10]
    | KEY_ESCAPE   ->(match Ws.queen ws with 
      | Some wid -> [`wClose wid] 
      | _ when Ws.activeWindow ws <> None -> [`sFocus None] 
      | _                                 -> [`wOpen (W.Quit,W.Default)])
    | _         -> [] ) (* end of match key_released *)
  | _ , list -> list



let modulate_tasks taskList =
      let m = mods None in
      let g = function 
	| `wOpen (W.Time, _     ) when m.lctrl -> `end_of_turn 1
	| `wOpen (wid, W.Default) when m.lalt  -> `wOpen (wid, W.Alter)
	| `wOpen (wid, W.Default) when m.lctrl -> `wOpen (wid, W.Left)
	| `wOpen (wid, W.Default) when m.rctrl -> `wOpen (wid, W.Right)
	| task -> task in
      List.map g taskList
(* modulation de la tâche associée au bouton en fonction de l’état des mod(ifier)s (alt, ctrl) *)

(* EOF *)
