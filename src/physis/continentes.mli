(*

 ****************************** Continentes.mli ******************************


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

(** fonctions relatives à la création des continents en particulier et du relief en général *)

open Std

type t = int Rid.Array.t

val lower_aside : (Rid.t -> int) -> int -> (Rid.t list) -> bool
val highest_alt_aside : (Rid.t -> int) -> int -> (Rid.t list) -> int

val create : Espace.t -> t

val ocean : int

val polarExclusion : float
