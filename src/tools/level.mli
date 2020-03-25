(*

 ****************************** src/tools/level.mli ******************************


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
(** 
    Typed discrete levels 

    incr and decr do nothing when trying to go outside bounds

*)

type t
(** a level is an integer that is limited by a lower bound and an upper bound *)

val make        : int -> int -> int -> t
(** [make a b c] returns a level whose lower_bound is [a], initial_value is [b], and upper_bound is [c] 

    raises [Invalid argument] when initial value is below [a] or above [c]. *)

val smake        : int -> int -> int -> t
(** secured make ensures initial_value is between the bounds *)


val to_int      : t -> int
(** returns the current value of the level *)

val to_float    : t -> float
(** returns the current value of the level, turned into a float *)

val lower_bound : t -> int
(** returns the lower bound of the level *)

val upper_bound : t -> int
(** returns the upper bound of the level *)

val incr        : t -> t
(** increases the current value of the level ; do nothing if trying to go above the upper bound *)

val decr        : t -> t
(** decreases the current value of the level ; do nothing if trying to go below the lower bound *)

val set         : t -> int -> t
(** set the current value of the level, with a maximum value of upper_bound, and a minimum value of lower_bound *)

val floor_at    : int -> t -> t
(** ensures current value is at least x *)


val extent      : t -> int
(** distance between the upper and lower bounds *)

val range       : t -> int
(** distance between the current level value and the lower bound *)


val min         : t -> int
(** alias for {!lower_bound} *)

val max         : t -> int
(** alias for {!upper_bound} *)

val int         : t -> int
(** alias for {!to_int} *)

val float       : t -> float
(** alias for {!to_float} *)
