(*

 ****************************** Imperium_map.mli ******************************


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




type nid = Nid.t

type t = private (Rv.t Rid.Array.t)

type natio = {
  centralized : bool;
  nav : bool;
  pop : float; (*(densité de) population*)
  artes : Ars.t list;
  sophia: float;
  fides : float;
  densitas : float;
  humanitas : float;
  instrumentum : float;
  plebs : float;
  facultas : float;
  plebsVar : float;
  alimonium_ratio : float;
  agriCopia : float;
  vis : float;
  }
(* ce que Im.create a besoin de savoir des nationes *)

val length : t -> int
val nIter : (Rid.t -> Rv.t -> unit) -> t -> unit

val get  : t -> Rid.t -> Rv.t
val facultas : Rm.t -> t -> Rid.t -> float
val incola   : t -> Rid.t -> Rv.Incola.t option
val incola_id: t -> Rid.t -> nid
val dominus  : t -> Rid.t -> nid

val create : Espace.t -> Rm.t -> (nid*Date.t) Rid.Array.t -> nid Rid.Array.t -> t
val update : Espace.t -> Rm.t -> t -> Junctiones.t -> natio Nid.Nia.t -> t * ((Rid.t*Rv.Incola.t) list)

val set_oikos_urbs : t -> Rid.t list -> unit
(** mise à jour de l’oikos en urbs pour les rid fournies *)

(*eof*)
