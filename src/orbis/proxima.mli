(*

 ****************************** Prox(ima natio).ml ******************************


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



type t = {
  politeia : Politeia.t;
  artes : Ars.t list;
  }
(* proxima = proxima natio
les champs mentionnent ce que l'on sait des natio voisines *)


val proximae : NatioList.t -> Nid.t list -> t Nid.Nil.t
(** initialisation d'une liste de proximae (= les proximae d'une natio) *)

val proxArtes : t Nid.Nil.t -> Ars.t list
(** liste des artes connus d’une ou plusieurs des nationes voisines *)

