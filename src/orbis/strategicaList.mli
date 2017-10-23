(*

 *************************** StrategicaList.mli ***************************


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
type t = private Strategica.t Nid.Nil.t

val make           : nid list -> t
val get_strategica : t -> nid -> Strategica.t


val update         : t -> NatioList.t -> Proxima.t Nid.Nil.t Nid.Nil.t -> Strategica.Data.d -> t
