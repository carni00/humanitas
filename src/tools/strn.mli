(*

 ****************************** Strn.mli ******************************


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

(** Custom formatted strings *)

val pth : string -> string
(** Add parentheses to a string *)
val rdblint : int -> int -> string
(** Returns a lisible string for a long integer. 
    For example, [rdblint 3 8456789] returns ["8 450 000"]. *)
val longInt : int -> int -> string
(** Returns a lisible string for a long integer. 
    For example, [longInt 5 8456789] returns ["8 450 000"]. *)
val float : int -> float -> string
(** Returns a lisible string for a float. 
    For example, [float (-2) 2.2568648] returns ["2.25"]. *)
val rdblfloat : int -> float -> string
(** Returns a lisible string for a float. 
    For example, [rdblfloat (-3) 0.02568648] returns ["0.0256"]. *)
val percent : int -> float -> string
(** Returns a lisible string for a float. 
    For example, [percent (-2) 0.0225688] returns ["2.25%"]. *)

val latitude  : float -> string
val longitude : float -> string

val coords : float -> float -> string
(** [coords lat lon] returns lisible geographical coordinates. 
    For example, [coords (-10.5465) (10.2165)] returns ["(10.5°N,10.2°E)"] *)


val km  : int -> int -> string
val km2 : int -> int -> string
