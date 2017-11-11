(*

 ****************************** src/game/status.mli ******************************


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

module Atelier : sig
  
  type background = Tabula | Graphique
  type t
  val game   : t -> Game.t
  val tabula : t -> Tabula.t
  val pid    : t -> Game.Player.id
  val player : t -> Game.Player.t
  val scene  : t -> Scene.t
  val geoRect: t -> Scene.GeoRect.t
  val background: t -> background
end

type t 

val create : unit -> t
val screen : t -> Screen.t
val atelier: t -> Atelier.t option
val windows : t -> Windows.t
val is_running : t -> bool
val update : t -> Task.t -> t
val task_history : t -> Task.t list

(*val load_game : Game.t -> Game.Player.t -> t -> t*)
(*val switch_fullscreen : t -> t*)
(*val screen_size_alter : float -> t -> t*)
