(*

 ****************************** Regio_map.mli ******************************


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




type t = private (R.t Rid.Array.t)

val create : Espace.t -> t 

val get : t -> Rid.t -> R.t

val length : t -> int
val nIter : (Rid.t -> R.t -> unit) -> t -> unit

val altitude : t -> Rid.t -> int
val alt : t -> Rid.t -> int
val hydros    : t -> Rid.t -> R.hydros
val thermos  : t -> Rid.t -> int
val physis : t -> Rid.t -> int
(*val is_passable : t -> Rid.t -> bool*)

(*val is_farmable : t -> Rid.t -> bool*)
(* à déplacer dans la partie humaine *)
