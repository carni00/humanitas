(*

 ****************************** ars.mli ******************************


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

type t = 
| N_ARS 
| AGR 
| MET 
| WRI 
| NAV 
| GUN 
| STE 
| CMB 
| ELE

type cognitio = [
| `inventio 
| `propagatio
]


val eff : t list -> t -> float

type eff_tab =
  {
  sap : float;
  mil : float;
  opp : float;
  }

val eff_tab : t list -> eff_tab
 
val level : t -> int

val list : t list
val beginList : t list


val instMax : t list -> float

val to_nil : t -> Color.Nil.t

val artes_to_rvb : t list -> Color.Rvb.t
val artes_to_nil : t list -> Color.Nil.t

