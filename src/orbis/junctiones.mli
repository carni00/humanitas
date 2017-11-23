(*

 ****************************** Junctiones.mli ******************************


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



type t

type nid = Nid.t

type offensive =
| Conquest
| Release

type tactic = 
| Offensive of offensive
| Retreat
| Defensive 

type relatio = 
| Bellum 
| Pax
| N_relatio

type rogatio =
| R_Bellum
| R_Pax
| N_rogatio

type natio = {
  chora : float;
  imperium : float;
  vis : float;
  }

type strategies = {
  rogatio_map   : rogatio Nid.Nim.t;
  tactic_map    : tactic  Nid.Nim.t;
  }
(* données stratégiques à partir desquelles on met à jour les junctiones *)

val relatio : t -> nid -> nid -> relatio

val is_attacking : t -> nid -> nid -> offensive option
(** is y attacking x ? if y is attacking, returns if the offensive is Some conquest or release, else returns None *)

val warNb   : t -> nid -> int

val warNatioList : t -> nid -> nid list

val make      : t 

val update    : t -> natio Nid.Nil.t -> strategies -> t

val newRelationes : rogatio Nid.Nim.t -> relatio Nid.Nim.t

module Natio : sig
  type j = t
  type t
  val null : t
  val make : j -> nid -> t 

  val relatio_list  : t -> relatio Nid.Nil.t 
  (** liste de nos relations diplomatiques *)
  (** seules les valeurs différentes de N_relatio sont enregistrées *)

  val theirTactic_list : t -> tactic  Nid.Nil.t 
  (** liste des tactiques des autres nations envers nous *)
  (** la liste de nos tactiques envers les autres nations est dans notre strategica *)

end

