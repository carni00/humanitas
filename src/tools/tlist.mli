(*

 ****************************** tlist.mli ******************************


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

(*include module type of List*)
(** List additionnal operations *)

val len  : 'a list -> int
(** alias for [List.length] *)

val snth : 'a -> 'a list -> int -> 'a
(** secured [List.nth] (a default value must be given as first arg) *)

val last : 'a list -> 'a
(** last element of a list *)

val optLast : 'a list -> 'a option
(** last element of a list, returns as an [option] *)

val int  : int -> int list
(** [int n] builds the list of the integers from 0 to [n-1] *)

val make : int -> 'a -> 'a list
(** returns a list of [n] similar elements *)

val init : ?b:int -> int -> (int->'a) -> 'a list
(** [init ?b n f] builds the list [f b; f(b+1); ...; f(b+n-1)], default value for b is zero *)

val remove : 'a list -> 'a -> 'a list
(** [remove l a] eventually removes element [a] from list [l] *)

val nRemove : 'a list -> int -> 'a list
(** [remove l n] eventually removes the [n]th element from list [l] *)

(** {6 Association lists} *)

val assRev     : ('a*'b) list -> ('b*'a) list
(** reverse la clé et l’élément associé, conserve l’ordre initial *)

val optAssoc  : 'b -> ('b*'a) list -> 'a option
(** similar as [List.assoc], excepting that it returns an option *)

val keyList   : ('a*'b) list -> 'a list

val valueList : ('a*'b) list -> 'b list
(** decomposition of an association list *)


(** {6 Iterators} *)

val filter_and_map : ('a -> 'b option) -> 'a list -> 'b list

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** [mapi f [a0; ...; an]] builds the list [f 0 a0; ...; f n an]*)

val iteri : (int -> 'a -> unit) -> 'a list -> unit
(** [iteri f [a0; ...; an]] is equivalent to [begin f 0 a0; f 1 a1; ...; f n an; () end].*)

val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val map3 : ('a->'b->'c->'d) -> 'a list -> 'b list -> 'c list -> 'd list
val map4 : ('a->'b->'c->'d->'e) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list
val map5 : ('a->'b->'c->'d->'e->'f) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list
val fold_left3 : ('d->'a->'b->'c->'d) -> 'd -> 'a list -> 'b list -> 'c list -> 'd

val applique   : 'a -> ('a -> 'a ) list -> 'a
(** [applique a [f0; f1; ...; fn]] returns [fn( ...(f1 (f0 a)))] *)

(** {6 List scanning} *)

val census : ('a -> bool) -> 'a list -> int
(** [census p l] returns the number of elements of the list [l] that satisfy the predicate [p] *)

val mem_n : 'a -> 'a list -> int
(** [men_n a l] returns the rank of the element [a] in the list [l]. 
    Raise [Not_found] if [l] does not include [a] *)

val filter_n : ('a -> bool) -> 'a list -> int list
(** [filter_n p l] builds the list of the ranks of the elements of the list [l] that satisfy the predicate [p].*)

(** {6 List searching} *)

val min  : 'a list -> 'a
(** returns the smallest int/float of the list*)

val max  : 'a list -> 'a
(** returns the biggest int/float of the list*)

val champ : 'a list -> int -> int -> 'a list
(** returns a section of a list ; [champ list a n] buils the list of the elements from the [a]th to the [a+n-1]th *)

val nFilter : (int -> bool) -> 'a list -> 'a list
(** [nFilter p l] returns all the elements of the list [l] whose rank in the list satisfy the predicate [p].
    The order of the elements in the input list is preserved.  *)

val following : 'a -> 'a list -> 'a
(** [following a l] returns the element of [l] that follows [a].
    Raise [Failure "List.following"] if none can be found *)

val following_or_first : 'a -> 'a list -> 'a
(** [following_or_first a l] returns the element of [l] that follows [a], or the first element if [a] is the last
    element.
    Raise [Failure "List.following_or_first"] if [l] does not include [a] *)

val following_or_last : 'a -> 'a list -> 'a
(** [following_or_last a l] returns the element of [l] that follows [a], or [a] if it is the last element.
    Raise [Failure "List.following_or_last"] if [l] does not include [a] *)

val previous_or_last : 'a -> 'a list -> 'a
(** [previous_or_last a l] returns the element of [l] that precedes [a], or the last element if [a] is the first
    element.
    Raise [Failure "List.previous_or_last"] if [l] does not include [a] *)

val previous_or_first : 'a -> 'a list -> 'a
(** [previous_or_first a l] returns the element of [l] that precedes [a], or [a] if it is the first element.
    Raise [Failure "List.previous_or_last"] if [l] does not include [a] *)

(** {6 List computing} *)

val sum  : int   list -> int
(** returns the sum of the integers of a list *)

val fsum : float list -> float
(** returns the sum of the floats of a list *)

val prod : int   list -> int
(** returns the product of the integers of a list *)

val fprod: float list -> float
(** returns the product of the floats of a list *)

val mean  : int list -> int
(** returns the average value of the integers of a list *)


