(*

 ****************************** Natio_list.mli ******************************


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
type natio = Natio.t

type t = private (natio Nid.Nil.t)
val length : t -> int
val nil    : t -> nid list
val get    : t -> nid -> natio
val iter   : (natio -> unit) -> t -> unit
val optGet : t -> nid -> natio option
val nf     : (natio->'a) -> t -> nid -> 'a
val create : Rm.t -> Im.t -> G.t -> Civitas.t Nid.Nil.t -> t
val update   : G.Natio.t Nid.Nil.t -> CivitasList.t -> t -> Partitio.Record.t Nid.Nil.t -> Lucrum.t -> Partitio.t Nid.Nil.t -> Ars.t list Nid.Nil.t -> t

val politeia : t -> nid -> Politeia.t
val artes    : t -> nid -> Ars.t list
val vis      : t -> nid -> float
val pNatio   : t -> nid -> Partitio.natio
val origo    : t -> nid -> Rid.t*Date.t
val urbsRid  : t -> nid -> Rid.t

val jNatioList : t -> Junctiones.natio Nid.Nil.t
val pNatioList : t -> Partitio.natio Nid.Nil.t
val origoList  : t -> (Rid.t * Date.t) Nid.Nil.t
val imNatioArray : t -> Im.natio Nid.Nia.t
val instArray : t -> float Nid.Nia.t
(*val map  : (nid -> natio -> 'a) -> t -> 'a Nid.Nil.t*)
(*val map2 : (nid -> natio -> natio -> 'a) -> t -> t -> 'a Nid.Nil.t*)
 

val plebs        : t -> float
val instrumentum : t -> float
val sophia       : t -> float
val fides        : t -> float


