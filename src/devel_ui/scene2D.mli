(*

 ****************************** Scene2D.mli ******************************


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

(** Affichage d’un plan de regiones (considéré comme un cylindre)

    Prise en compte des paramètres de Scene.t : échelle, position, angle comme variation des distance et position de
la camera au plan affiché

    Ne permet pas l’affichage d’un espace sphérique ; on projettera au préalable la sphère sur un plan pour obtenir un affichage
plan de la sphère *)

module Display : functor (Draw : Video.Draw) -> sig
  val display_scene : Scene.t -> Game.t -> Game.Player.t -> Tabula.regio Rid.Array.t * Tabula.regio Qtree.set -> unit
(**  affichage de la carte *)
  val regio_of_pos : Espace.t -> Scene.t -> int*int -> Rid.t option
(** regio située au pixel x,y *)
end
