(*

 ****************************** Typed_ID.mli ******************************


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

(** {5 Typed id as integers }*)


type 'a t  = private int
type 'a id = 'a t

val unus : 'a t

val oi : int -> 'a t

module Til : sig
  type ('a) t = ('a id * 'a) list
  val empty     :  'a t
  val len       :  'a t -> int
  val nth       :  'a t -> 'a id -> 'a
  val snth      :  'a -> 'a t -> 'a id -> 'a
  val to_list   :  'a t -> 'a list
(** returns the liste of values (keys are lost) *)
  val key_list  :  'a t -> ('a id) list
(** returns the liste of keys ('a id) (values are lost) *)
  val iter      : ('a -> unit) -> 'a t -> unit 
  val map       : (         'a -> 'b) -> 'a t -> 'b t
  val mapi      : ('a id -> 'a -> 'b) -> 'a t -> 'b t
  val find      : ('a -> bool) -> 'a t -> 'a
  val search    : ('a -> bool) -> 'a t -> 'a option
  val filter    : ('a -> bool) -> 'a t -> 'a list
  val add       : ('a) t -> 'a    -> ('a) t
  val alter     : ('a) t -> 'a id -> ('a -> 'a) -> ('a) t
  val first_id  : ('a) t -> 'a id
  end


