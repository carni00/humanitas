(*

 ****************************** src/tools/ilist.mli ******************************


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

(** Indexed list *)

type ('k, 'a) t = private ('k*'a) list

val null : ('k, 'a) t
(** empty ilist *)

val add  : ('k, 'a) t -> 'k -> 'a -> ('k, 'a) t 

val nth  : ('k, 'a) t -> 'k -> 'a
(** [nth l k] returns the leftmost value associated with key [k]. Raises [Not_found] if there is none.*)

val snth : 'a -> ('k, 'a) t -> 'k -> 'a
(** [snth a l k] returns the leftmost value associated with key [k]. Returns [a] if there is none.*)

val to_list : ('k, 'a) t -> 'a list
(** builds a non indexed list from an ilist. Order of the elements is preserved *)

val init    : ('k -> 'a) -> 'k list -> ('k, 'a) t
(** [init f [k1; ...; kn]] builds an ilist from a list of keys and the function computing an element from its
    key*)

val nMap     : ('k->'a->'b) -> ('k, 'a) t -> ('k, 'b) t
(** [nMap f ilist] builds a new ilist from a previous one. Keys are preserved ; New elements are computed from
    keys and elements of the ilist given as argument, that is [f k e]  *)

val iteri    : ('k->'a-> unit) -> ('k, 'a) t -> unit
(** [iteri f ilist] runs [ { f k1 a1; ...; f kn an } ]*)

val fold_left : ('a -> 'b -> 'a) -> 'a -> ('k, 'b) t -> 'a
(** [fold_left f s ilist] returns [f (... (f (f s e0) e1) ...) e(n-1)] with e0, e1,...,e(n-1) being the n elements of ilist *)

val remove : ('k, 'a) t -> 'a -> ('k, 'a) t
(** [remove ilist e] removes the element [e] from [ilist] *)

val nRemove : ('k, 'a) t -> 'k -> ('k, 'a) t
(** [nRemove ilist k] removes the (first) element associated with the key [k] *)

val nFilter : ('k -> bool) -> ('k, 'a) t -> ('k, 'a) t
(** [nFilter p ilist] returns all the elements of [ilist] whose key satisfy the predicate [p].
    The order of the elements in the input list is preserved.  *)

val filter : ('a -> bool) -> ('k, 'a) t -> ('k, 'a) t
(** [nFilter p ilist] returns all the elements of [ilist] that satisfy the predicate [p].
    The order of the elements in the input list is preserved.  *)
