(*

 ****************************** hum_devel.ml ******************************


 *  This file is part of Humanitas.

 *  Humanitas is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License,
 *  or (at your option) any later version.

 *  Humanitas is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.

 *  You should have received a copy of the GNU General Public License
 *  along with Humanitas; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

 *)


open Humanitas_tools
open Humanitas_physis 
open Humanitas_orbis 
open Humanitas_game
open Humanitas_devel_ui
open Std

module RS = React.S
module RE = React.E
module RSS = React.S.Make(struct
  type 'a t = Status.t
  let equal = ( == )
end)
module H  = Handler
module W  = WindowID

let () =
  Sdl.init( [`VIDEO ; `EVENTTHREAD] ) ;
  at_exit Sdl.quit ;
  Sdlttf.init ();
  at_exit Sdlttf.quit(* ; *)
(*  Sdlkey.enable_key_repeat () ~delay:500 ~interval:50 *)
(* Video.print_info (Video.video_info (Status.screen (RS.value status))) *)

module Draw = Video.Draw(struct end)
module UI = Frui.Make(Draw)

module Earth = Scene2D.Display (Draw)
module Graph = Graphique.Display (Draw)

module Gui = Gui.Make(Draw)

let agenda, send_agenda = RE.create ()

let status, gui =
  let initial_status = Status.create () in
  let define status =
    let gui, tasks = Gui.main status in
    let all_tasks = RE.merge ( @ ) [] [ tasks ; agenda ] in
    let status' = RSS.fold (List.fold_left Status.update) initial_status all_tasks in
    status', (status', gui)
  in
  (RSS.fix initial_status define : Status.t React.signal * Gui.t)

let rien = 
  RS.trace 
    (fun w -> Printf.printf "status: %s\n%!" (Core.Option.value_map w ~default:"none" ~f:Draw.Window.string_of_id))
    (RS.map (fun s -> Windows.activeWindow (Status.windows s)) status)

let handle_next_event ws pick =
  let mods, next_event = Handler.next_event() in
  let gui_event = match next_event with
  | H.Key   (k,s) -> Gui.UI.keyboard_input gui k s
  | H.Mouse (evt, x, y) -> Gui.UI.mouse_input gui x y (evt :> Gui.UI.mouse_event)
  in
  if not gui_event then send_agenda (Task.check_odyssey (Handler.task_list ws pick (mods, next_event)))


let display_atelier atelier _screen =
    let module SA = Status.Atelier in
    let espace = Orbis.espace (Game.orbis (SA.game atelier)) in
    match SA.background atelier, SA.tabula atelier with
    | SA.Tabula, Tabula.Qtree qtree -> Earth.display_scene (SA.scene atelier) (SA.geoRect atelier) (SA.game atelier) (SA.player atelier) qtree ;
                                       Earth.regio_of_pos espace (SA.scene atelier) (SA.geoRect atelier)
    | SA.Graphique, _               -> Graph.display_graphique atelier ; (fun (_x,_y) -> None)
  (*  | _ -> (fun (x,y) -> None)*)
  (* affiche la carte, et retourne une fonction de picking des regiones *)
  

let pick get_element get_regio x y = match get_element x y with
  | Some pe  -> Some pe
  | None     -> (match get_regio (x,y) with
    | Some rid -> Some (Picking.Regio rid)
    | None     -> None)
(* fonction globale de picking : on regarde d'abord s'il y a un element sur la carte, puis, sinon, si une regio est visée *)


let rec cycle () =
  let s = RS.value status in
(*Draw.clear_screen();*)
  Draw.clear_picking();
  let get_regio = match Status.atelier s with
  | None         -> (fun (_x,_y) -> None)
  | Some atelier -> display_atelier atelier (Status.screen s) in
  let pick = pick (Draw.get_element) (get_regio) in
  Gui.UI.draw gui;
  Draw.resolution();
  Draw.flip();
  handle_next_event s pick ;
  if Status.is_running (RS.value status) then cycle()

let () =
  Random.self_init();
  send_agenda [
    `new_game { Game.resolution = Espace.Lower } ;
(*   `wOpen (W.TaskHistory, W.Right) ;*)
(*    `sFocus None ;*)
  ] ;
  cycle()


(*EOF*)
