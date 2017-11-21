(*

 ****************************** Natio_id.mli ******************************


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

type t = private int
type nid = t
val nb : int

val none : t

val oi : int -> t

val ti : t -> int
val tf : t -> float

val compare : t -> t -> int
val sort    : t list -> t list


module Nil : sig
  type 'a t = (nid*'a) list
  val empty : 'a t
  val len  : 'a t -> int
  val nth  : 'a t -> nid -> 'a
  val snth : 'a -> (nid*'a) list -> nid -> 'a
  val to_list  : 'a t -> 'a list
(** returns the liste of values (keys are lost) *)
  val key_list : 'a t -> nid list
(** returns the liste of keys (nation id) (values are lost) *)
  val init : (nid->'a) -> nid list -> 'a t
  val iter : ('a->unit) -> 'a t -> unit 
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map  : ('a->'b) -> 'a t -> 'b t
  val mapi : (nid->'a->'b) -> 'a t -> 'b t
  val mapi_to_list : (nid->'a->'b) -> 'a t -> 'b list
(** similar as mapi, excepting that it returns a list of the mapped values (keys are lost) *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  val nfilter : (nid -> bool) -> 'a t -> 'a t
  val sort : 'a t -> 'a t
  val map2  : ('a->'b->'c) -> 'a t -> 'b t -> 'c t
  val map3  : ('a->'b->'c->'d) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4  : ('a->'b->'c->'d->'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val set  : 'a t -> (nid*'a) -> 'a t
  val add  : 'a t -> (nid*'a) -> 'a t
  end

module Nim : sig
  type 'a t = ((nid*nid)*'a) list
  val empty : 'a t
  val nth  : 'a t -> nid -> nid -> 'a
  val snth : 'a -> 'a t -> nid -> nid -> 'a
  val bnth : 'a -> 'a t -> nid -> nid -> 'a
(** à utiliser sur la map équivalente à une matrice symétrique (c-à-d ou nth y x == nth x y, mais ou seule nth y x est encodé *)
  val init : (nid -> nid -> 'a) -> nid list -> 'a t
  val iteri: (nid -> nid -> 'a -> unit) -> 'a t -> unit
  val mapi : (nid -> nid -> 'a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val nfilter: ((nid*nid) -> bool) -> 'a t -> 'a t
  val sort  : 'a t -> 'a t
  val norm  : 'a t -> 'a t
  val sym   : 'a t -> 'a t
  val sym_mapi : (nid->nid->'a->'a) -> 'a t -> 'a t
  val smap2 : 'a -> 'b -> ((nid*nid)->'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** mappage de deux Nil.t possiblement incomplètes (des couples clé/élément manquent). Les valeurs a0 et b0 sont retenues dans
  ces cas en input *)
  (** smap2 suppose que al et bl sont classées dans le même ordre (selon les k(eys)) *)
  val line  : 'a t -> nid -> 'a Nil.t
  val of_ll : 'a Nil.t Nil.t -> 'a t
  val to_ll : 'a t -> 'a Nil.t Nil.t
  end

module Nia : sig
  type 'a t
  val get : 'a t -> nid-> 'a
  val length : int
  val make : 'a -> 'a t 
  val init : (nid-> 'a) -> 'a t     
  val blit : 'a t -> 'a t -> unit
  val set : 'a t -> nid-> 'a -> unit
  val nIter : (nid-> 'a -> unit) -> 'a t -> unit
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b array -> unit
  val to_array : 'a t -> 'a array
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val to_nil  : 'a t -> 'a Nil.t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  
  val map_of_nil : 'a -> ('a -> 'b) -> 'a Nil.t -> 'b t
  val fsum : float t -> float
(*  val to_nil : 'a -> 'a t -> 'a Nil.t*)
  end


module Nix : sig
  type 'a t
(*  val length : int*)
(*  val surface : int*)
  val make  : 'a -> 'a t 
  val empty : unit -> 'a option t 
  (** returns a matrix sized for the max number of nations, and filled with the option None *)
  val nIter : (nid -> nid-> 'a -> unit) -> 'a t -> unit
  val line   : 'a t -> nid -> 'a Nia.t
  val column : 'a t -> nid -> 'a Nil.t
(*  val init : (nid-> nid-> 'a) -> 'a t     *)
  val ySum : int t -> nid -> int
  val xSum : int t -> nid -> int
  val get : 'a t -> nid-> nid-> 'a
  val set : 'a t -> nid -> nid -> 'a -> unit
  end


module List : sig
  type t = nid list
  val make : t
  val random : t -> nid
  val sort   : t -> t 
  end


