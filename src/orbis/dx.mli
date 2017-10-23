(*

 ****************************** dx.mli ******************************


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


module Pyramid : sig
  type t = (int*float*float) list
  val sum              : t -> float
  val facultas_ratio   : t -> float -> float
  val alimonium_ratio  : t -> float -> float
  val vigesimal        : t -> (int list * float) list
end

type t

val null    : t
val create  : float -> float -> t
val pyramid : t -> Pyramid.t
val copia   : t -> float
val famine  : t -> float
val tfg     : t -> float (*taux de fécondité général*)
val isf     : t -> float*float (*isf, descendance finale nette*)
val sum     : t -> float 
val var     : t -> float
val facultas_ratio   : t -> float
val alimonium_ratio  : t -> float

val update  : t -> float -> t
val preview : t -> float -> float -> float -> float -> float -> float -> t
(*d(*demographics*) el(*effective lab*) s(*sophia*) f(*fides*) facultas m(*militaria*) warRatio lostRatio =*)

