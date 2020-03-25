(*

 ****************************** src/tools/tcube.mli ******************************


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
(** three-dimensional arrays *)

type 'a t = private 'a array array array

val depth  : 'a t -> int
(** returns the depth (dimz) of the cube *)

val height : 'a t -> int
(** returns the height (dimy) of the cube *)

val width  : 'a t -> int
(** returns the width (dimx) of the cube *)

val volume: 'a t -> int
(** returns the volume of the cube *)

val plan   : 'a t -> int -> 'a array array
(** [plan c z] returns the plan [z] of the cube*)

val line   : 'a t -> int -> int -> 'a array
(** [line c z y] returns the line [y] in plan [z] of the cube*)

val get    : 'a t -> int -> int -> int -> 'a
(** [get c z y x] returns the element [x] of line [y] in plan [z] *)

val make   : int -> int -> int -> 'a -> 'a t
(** [make d h w a] returns a fresh cube of depth [d], height [h] and width [w], with all element initialized to [a] *)

val init   : int -> int -> int -> (int -> int -> int -> 'a) -> 'a t
(** [init d h w f] returns a fresh cube of depth [d], height [h] and width [w], with element [x] of line [y] in
plan [z] initialized to the result of [f z y x] *)
