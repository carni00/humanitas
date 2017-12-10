(*

 ****************************** map.mli ******************************


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

(** Synthèse des données cartographiable à afficher ; interface entre la partie tabula et l’affichage *)

type filter = [
| `imperii
| `dominium
| `artes
| `montes
| `nationes
| `politeia
| `populatio
| `tegmen
| `vis
  ]

type color = {
  artes    : Color.t;
  dominium : Color.t;
  tegmenF  : Color.t;
  tegAlt   : Color.t;
  imperii  : Color.t;
  impAlt   : Color.t;
  nationes : Color.t;
  natAlt   : Color.t;
  politeia : Color.t;
  polAlt   : Color.t;
  populatio: Color.t;
  popAlt   : Color.t;
  popNat   : Color.t;
  vis      : Color.t;
  visAlt   : Color.t;
  montes   : Color.t;
  }

type regio = {
  rid : Rid.t;
  alt : int;
  coast : bool;
  thermos : int;
  hydros : R.hydros;
  silva  : int;
(*  gAlt : float ;*)
(*  pAlt : float ;*)
  mountain : bool ;
  tegmen : Rv.tegmen ;
  color : color ;
  borders : Rid.t list; (*liste des regiones entre lesquelles et la présente regio il y a une frontière interimpériale *)
  brouillards : Rv.Brouillards.t;
}

type t = 
| Qtree  of ((regio Rid.Array.t) *  (regio Qtree.set) )
(*| Octree of (regio Octree.set * regio Rid.Array.t) *)

val make_ria : Game.t -> regio Rid.Array.t
(** l’élaboration de tabula se fait non seulement à partir d’un orbis, mais à partir d’un game.t entier, puisque le
brouillard de guerre est lié au player *)

val make : Game.t -> t

val get : t -> Rid.t -> regio


module Color : sig
  type t = Color.t
  val ocean : ?a:int -> ?t:int -> unit -> t
  val incognita : t
  val terra_incognita : t
  val flumen :  t
  val border :  t
  val snow   :  t
end

