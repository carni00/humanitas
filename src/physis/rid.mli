(*

 ****************************** Rid.mli ******************************


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

(** Identifiant de regio *)

type t = private int
type rid = t

val none : t
val first : t

val oi : int -> t
val oe : t option -> t

val ti : t -> int
val add : t -> int -> t
val iter : int -> (t -> unit) -> unit

module Array : sig
  type 'a t
  val empty  : 'a t 
(*  val length : 'a t -> int*)
  val init : int -> (rid -> 'a) -> 'a t     
  val get : 'a t -> rid -> 'a
  val length : 'a t -> int
  val make : int -> 'a -> 'a t 
  val copy : 'a t -> 'a t
  val blit : 'a t -> 'a t -> unit
  val set : 'a t -> rid -> 'a -> unit
  val nIter : (rid -> 'a -> unit) -> 'a t -> unit
  val iter : ('a -> unit) -> 'a t -> unit
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val to_array : 'a t -> 'a array
  val update : (rid -> 'a -> 'a) -> 'a t -> unit
  end

