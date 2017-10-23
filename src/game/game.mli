(*

 ****************************** src/game/game.mli ******************************


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

module Player : sig
  type t
  type id
  type role =
  | Admin
  | Basileus of Nid.t
  | Civis of Nid.t
  | Spectator

  val role  : t -> role
  val name  : t -> string
  val pov   : t -> Nid.t
end

type id
type t 

type laws = {
  resolution : Espace.resolution;
(*  playerList : Player.t list;*)
  }
(** caractères fondamentaux d'une partie *)

 
val orbis       : t -> Orbis.t
val strategies  : t -> StrategicaList.t
val players     : t -> (Player.t) Tid.Til.t
val get_player  : t -> Player.id -> Player.t
val first_pid   : t -> Player.id

val create       : laws -> t
val update_orbis : t -> int -> t

val alter_player_pov : t -> Player.id -> Nid.t -> t
val get_player_pov   : t -> Player.id -> Nid.t

