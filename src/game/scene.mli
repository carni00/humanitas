(*

 ****************************** src/game/scene.mli ******************************


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

(** état courant de l’affichage de la carte *)

type forme =
| Plan
| Sphere
(* La forme de la scène n’est pas la forme de l’espace : on peut souhaiter afficher la sphere en plan *)


type t
val create : Orbis.t -> Game.Player.t -> t
val update : Game.t  -> t -> Game.Player.t -> Task.t -> t

val cr         : ?e:Espace.t -> t -> Rid.t   (*centered_regio*)
val sr         : t -> Rid.t option (*selected_regio*) 
val earthMode  : t -> bool
val ascale     : t -> float
(** actual scale : earthMode = 0. , zoom max = 1. *)
val wik        : t -> float


val alpha      : t -> float
(** angle camera / surface terrestre *)
val altdip     : t -> int -> float
(** altitude delta (en y) en km *)
val fluwip     : t -> float -> float

val filter : t -> Tabula.filter
  
val altitude : t -> bool
val borders  : t -> bool
val nations  : t -> bool



module GeoRect : sig

  type scene = t
  type t =
    {
    lonMin : float;
    lonMax : float;
    latMin : float;
    latMax : float;
    }
  
  val fittingLat : float -> float -> float -> float 
  val make : (float*float) -> scene -> Screen.t -> t
  val is_visible    : t -> (float*float) -> bool 
  val visibleCoords : t -> (float*float) -> (float*float) option
end   

