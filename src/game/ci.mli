(*

 ****************************** ci.mli ******************************


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



val font  : Color.t
val wsb   : Color.t
(*val wgb   : Color.t*)
val wib   : Color.t
val focus : Color.t
(*val natioColumn : Nid.t -> Color.Nil.t *)
(*val wScrollBar  : Color.t*)
(*val bBackground : Windows.buttonAlert -> int -> Color.t*)

(*val ocean : Color.t*)
(*val river : Color.t*)
val bsl : Color.t
val bsb : Color.t
val bpl : Color.t
val bpb : Color.t


val natio    : Nid.t      -> Color.Nuance.t
val politeia : Politeia.t -> Color.Nuance.t

