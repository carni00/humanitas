(*

 ****************************** src/game/graph.ml ******************************


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

type t = {
  xMin : float; (** plus petite valeur de x affichée à l’écran *)
  yMin : float; (** plus petite valeur de y affichée à l’écran *)
  xppt : float; (** x pixel per turn, nombre de pixels pour chaque tour *)
  yppu : float; (** y pixel per unit, nombre de pixels pour chaque Tfloat.u *)
  }

let make = {
  xMin =   0. ;
  yMin =   0. ;
  xppt =   1. ;
  yppu = 400. ;
  }
  
let xMin g = g.xMin 
let yMin g = g.yMin 
let xppt g = g.xppt 
let yppu g = g.yppu 


(*
let update g s p task = 
  let o = Game.orbis g in
  match task with
| `map_center rid      -> { s with cr = rid } 
| `map_move (dir,int)  -> map_move o s (dir,int)
| `move_to_capitolium  -> move_to_capitolium o s p 
| `move_sr  (dir,int)  -> move_sr  o s (dir,int)
| `select_regio ridOpt -> { s with sr = ridOpt } 
| `secure_sr           -> { s with sr = match s.sr with None -> Some s.cr | sr -> sr } 
| `select_filter f     -> { s with filter = f; options = { s.options with nations=false } }
| `switch_filter       -> { s with filter = try (Tlist.following_or_first s.filter filterList) with Failure _ -> `tegmen }
| `angle_up            -> { s with angle = Level.incr s.angle }
| `angle_down          -> { s with angle = Level.decr s.angle }
| `zoom_in             -> zoom s task
| `zoom_out            -> zoom s task
(*| `new_game o          -> create o player*)
| `toggle_earthMode    -> { s with earthMode = not s.earthMode }
| `toggle_altitude     -> { s with options = { s.options with altitude = not s.options.altitude } }
| `toggle_borders      -> { s with options = { s.options with borders  = not s.options.borders  } }
| `toggle_nations      -> { s with options = { s.options with nations  = not s.options.nations  } }
| `defaultDisplay      -> reset s
| _                    ->   s
*)
