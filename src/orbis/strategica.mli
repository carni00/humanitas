(*

 ****************************** strategica.mli ******************************


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

(** stratégie d’une natio (centralisée) pour un tour *)

type nid = Nid.t

type t


module Data : sig
  type d = {
    stratiotikon_list : Stratiotikon.t Nid.Nil.t;
    rogatio_map       : Junctiones.rogatio        Nid.Nim.t;
    tactic_map        : Junctiones.tactic         Nid.Nim.t;
    }
  
  val make    : t Nid.Nil.t -> d
  val of_nil  : Nid.t list -> d
  val rogatio : d -> Nid.t -> Nid.t -> Junctiones.rogatio
  val tactic  : d -> Nid.t -> Nid.t -> Junctiones.tactic
  val jStrategies  : d -> Junctiones.strategies
end


val stratiotikon : t -> Stratiotikon.t
val tactic_list  : t -> Junctiones.tactic  Nid.Nil.t
val rogatio_list : t -> Junctiones.rogatio Nid.Nil.t

val make   : t
val update : Data.d -> Natio.t -> Proxima.t Nid.Nil.t -> t -> t

val tactic : t -> nid -> Junctiones.tactic
