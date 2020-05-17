(*

 ****************************** date.mli ******************************


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

(*type turn = private int*)
type gregorian = private int
type national  = private (Nid.t * int)

type t =
| G of gregorian
| N of national
| Unknown

val agriculture : t
val writing     : t
val metallurgy  : t
val beginning   : t

val make        : int -> t
val to_int      : t -> int
val compare     : t -> t -> int
val precedes    : t -> t -> bool
val inc         : t -> t
val distance    : t -> t -> int
val add         : t -> int -> t

