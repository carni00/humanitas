(*

 ****************************** src/tools/tarray.mli ******************************


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

(*include module type of Array*)

(** Array additionnal operations *)

val len : 'a array -> int
(** alias for [Array.length]*)

val sum : int array -> int
(** sum of the elements of an [int array]*)

val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
(** [iter2 f [|a0; ...;an|] [|b0; ...;bn|] ] runs [f a0 b0; ... ; f an bn]. Raises [Invalid_argument "Array.iter2"]
if the two arrays are of various length*) 

val nIter : (int -> 'a -> unit) -> 'a array -> unit
(** [nIter f [|a0; ...;an|] ] runs [f 0 a0; ... ; f n an].*)

val nIter2 : (int -> 'a -> 'b -> unit) -> 'a array -> 'b array -> unit
(** [nIter2 f [|a0; ...;an|] [|b0; ...;bn|] ] runs [f 0 a0 b0; ... ; f n an bn]. Raises [Invalid_argument "Array.nIter2"] if the two arrays are of various length*) 

val filter_n : ('a -> bool) -> 'a array -> int list
(** [filter_n p array] builds the list of the ranks of the elements of [array] that satisfy the predicate [p].*)

val alter : ('a -> 'a) -> 'a array -> unit
(** [alter f array] replaces each element [en] of [array] by [f en]*)

val nAlter : (int -> 'a -> 'a) -> 'a array -> unit
(** [nAlter f array] replaces each element [en] of [array] by [f n en]*)

val exists : ('a -> bool) -> 'a array -> bool
(** [exists p [|a0; ...; an|]] checks if at least one element of the array satisfies the predicate [p]. That is, it returns [(p a0) || ... || (p an)]. *) 

