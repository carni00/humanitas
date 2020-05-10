(*

 ****************************** task.ml ******************************


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

open Humanitas_physis
open Humanitas_orbis

type t = [
| `do_nothing
| `quit
| `switch_baby_mode of bool
| `switch_fullscreen
| `screenSizeAlter of float
| `note_screenUpdate
| `describe_element of int*int
| `wait
| `save_game
| `end_of_turn of int
| `next_event
| `load_game of (Game.t * Game.Player.t)
| `new_game of Game.laws

| `alter_player_pov of (Game.Player.id*Nid.t)

| `zoom_in
| `zoom_out
| `angle_up
| `angle_down
| `map_center of Rid.t
| `move_to_capitolium
| `select_regio of (Rid.t option)
| `secure_sr
(**)
| `map_move of (Espace.direction*int)
| `move_sr  of (Espace.direction*int)
| `switch_filter
| `switch_background
| `select_filter of Tabula.filter
| `defaultDisplay
| `toggle_earthMode
| `toggle_altitude
| `toggle_borders
| `toggle_nations
(*| `toggle_grid*)
(*| `toggle_polyhedron*)
| `toggle_stacks
(**)
| `wOpen  of WindowID.t * WindowID.position
| `wClose of WindowID.t
| `wNext
| `wPrevious
| `wUndo
| `wMove
(*| `wScroll of int*int*int*)
| `sFocus of(WindowID.position option)
| `sHide  of WindowID.position
| `sClose of WindowID.position
(*| `bPress of WindowID.t*Button.name*)
(*| `bRelease*)
]

module W = WindowID

let is_odyssey = function
| `end_of_turn n when n >= 50 -> true
| `new_game _ -> true
| _ -> false

let check_odyssey task_list =
  if List.exists is_odyssey task_list 
  then `wOpen (W.Computing, W.Default) :: task_list @ [`wClose W.Computing]
  else task_list


