(*

 ****************************** src/tools/tmatrix.mli ******************************


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

(** two-dimensional arrays *)

type 'a t = private 'a array array

val height : 'a t -> int
(** returns the height (dimy) of the matrix *)

val width  : 'a t -> int
(** returns the width (dimx) of the matrix *)

val surface: 'a t -> int
(** returns the surface of the matrix *)

val line   : 'a t -> int -> 'a array
(** [line mx y] returns the line [y] of the matrix*)

val column : 'a t -> int -> 'a list
(** [column mx x] builds a list for the column [x] of the matrix*)

val get    : 'a t -> int -> int -> 'a
(** [get mx y x] returns the element [x] in line [y] *)

val set    : 'a t -> int -> int -> 'a -> unit
(** [set mx y x] sets the value of the element [x] in line [y] *)

val make   : int -> int -> 'a -> 'a t
(** [make h w a] returns a fresh matrix of height [h] and width [w], with all element initialized to [a] *)

val init   : int -> int -> (int -> int -> 'a) -> 'a t
(** [init h w f] returns a fresh matrix of height [h] and width [w], with element [x] in line [y] initialized to the result of [f y x] *)

val ySum   : int t -> int -> int
(** [ySum mx y] returns the sum of the integers in line [y] *)

val xSum   : int t -> int -> int
(** [xSum mx x] returns the sum of the integers in column [x] *)

val nIter : (int -> int-> 'a -> unit) -> 'a t -> unit
