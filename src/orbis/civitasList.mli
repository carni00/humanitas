(*

 ****************************** cl.mli ******************************


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

type t 



val iter   : (Civitas.t -> unit) -> t -> unit

val get    : t -> Civitas.t Tid.t -> Civitas.t
val search : t -> Rid.t           -> Civitas.t option
val filter : t -> Nid.t           -> Civitas.t list

val create : Espace.t -> ((Rid.t * Date.t) Nid.Nil.t) -> (Rid.t*Rv.Incola.t) list -> t
val update : Espace.t -> Date.t -> Im.t -> t -> (Rid.t*Rv.Incola.t) list -> t


val create_nil : Imd.origo Nid.Nia.t -> Nid.t list -> Civitas.t Nid.Nil.t
