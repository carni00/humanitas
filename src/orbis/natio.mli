(*

 ****************************** n.mli ******************************


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

type regio =
| Cognita
| Terra_incognita
| Incognita

type t

val null   : t

val nid          : t -> Nid.t
val is_active    : t -> bool
val origo        : t -> Rid.t*Date.t
val has_urbs     : t -> bool
val urbsRid      : t -> Rid.t
val pil          : t -> Nid.t list
val artes        : t -> Ars.t list
val has_nav      : t -> bool
val ter_rayon    : t -> float
val mer_rayon    : t -> float
val regio        : t -> Espace.t -> float -> float -> int -> bool -> Rv.Brouillards.t -> regio
val partitio     : t -> Partitio.t
val kapital      : t -> Aedificium.t
val politeia     : t -> Politeia.t
val geographia   : t -> G.Natio.t
val seditio      : t -> float
val vis          : t -> float
val imperium     : t -> float
val chora        : t -> float
val facultas     : t -> float
val plebs        : t -> float
val hospitalitas : t -> float
val efficientia  : t -> float
val instrumentum : t -> float
val copia        : t -> float
val famine       : t -> float
val tfg          : t -> float
val isf          : t -> float
val dfn          : t -> float
val dxVar        : t -> float
val alimonium_ratio : t -> float
val facultas_ratio  : t -> float
val pyramid      : t -> Dx.Pyramid.t
val sophia       : t -> float
val fides        : t -> float
val libertas     : t -> float
val agriCopia    : t -> float
val densitas     : t -> float
(** densité de population en hab/km2 *)


(*val proxima : t -> Proxima.t*)
val jNatio  : t -> Junctiones.natio
val imNatio : t -> Im.natio
val pNatio  : t -> Partitio.natio

val update   : G.Natio.t -> Civitas.t list -> t -> Partitio.Record.t -> Partitio.t -> Partitio.t -> Ars.t list -> t

val create : Rm.t -> Im.t -> G.t -> Nid.t -> Civitas.t -> t

