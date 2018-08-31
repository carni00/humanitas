(*

 ****************************** strategica.mli ******************************


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

(** Répartition de la capacité sociale à la disposition du prince
    entre les différentes attributiones impériales
    leur somme s'éleve à 10 *)


type t
val null  : t
val init  : int   -> int   -> int   -> t
val float : float -> float -> float -> t

val mil : t -> float
val rel : t -> float
val opp : t -> float
val oti : t -> float

