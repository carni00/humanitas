(*

 ****************************** src/game/status.ml ******************************


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

module Atelier = struct
  
  type t = {
    game   : Game.t; 
    tabula : Tabula.t;
    pid    : Game.Player.id;
  (* un atelier affiche une partie (game) aux yeux d’un *player particulier *)
    scene  : Scene.t
    }
(** la partie actuellement affichée à l’écran *)
  let game    a = a.game
  let tabula  a = a.tabula
  let pid     a = a.pid
  let player  a = Game.get_player a.game a.pid
  let scene   a = a.scene
  
  let createb laws =
    let game = Game.create laws in
    let pid  = Game.first_pid game in
    {
    game;
    tabula = Tabula.make game;
    pid;
    scene  = Scene.create (Game.orbis game) (Game.get_player game pid) ;
    }

  let create game pid    = {
    game;
    tabula = Tabula.make game;
    pid;
    scene = Scene.create (Game.orbis game) (Game.get_player game pid) ;
    }

  let update atelier task = 
    let game , tabula_need_update = match task with 
    | `end_of_turn int            -> Game.update_orbis (atelier.game) int, true
    | `alter_player_pov (pid,nid) -> Game.alter_player_pov (atelier.game) pid nid, false
    | _ -> atelier.game, false in
    {
    atelier with
    scene = Scene.update game (atelier.scene) (player atelier) task;
    tabula= if tabula_need_update then Tabula.make game else atelier.tabula;
    (* le calcul de tabula prend 0.3 secondes ; n'en abusons pas *)
    game;
    (* on procède ainsi pour éviter un circular build : la task `load_game prend en arg un game *)
    }

end




type t = {
  screen       : Screen.t;
  windows      : Windows.t;
  atelier      : Atelier.t option;
  running      : bool;
  baby_mode    : bool;
  task_history : Task.t list;
  }
(** l’état courant du programme *)

let screen   s = s.screen
let atelier  s = s.atelier
let windows  s = s.windows
let is_running  s = s.running
let task_history  s = s.task_history

let create () =
  {
  screen  = Screen.create();
  windows = Windows.create();
  atelier = None;
  running = true;
  baby_mode = false;
  task_history = Tlist.make 8 (`do_nothing); (*connerie pour que Window.task_history ne provoque pas une erreur de segmentation *)
  }


let update status task = 
  let screen  = Screen.alter   status.screen  task in
  let windows = Windows.update status.windows task in
  let atelier = match status.atelier, task with
(*    | _     , `load_game (game, player) -> Some (Atelier.create game player )*)
    | _     , `new_game  laws           -> Some (Atelier.createb laws )
    | Some a,  _                        -> Some (Atelier.update a    task)
    | None  ,  _                        -> None in
  let running = if task == `quit && status.baby_mode == false then false else status.running in
  let baby_mode = match task with `switch_baby_mode bool -> bool | _ -> status.baby_mode in
  {
  baby_mode;
  screen;
  windows;
  atelier;
  running;
  task_history = task :: status.task_history;
  }
   
