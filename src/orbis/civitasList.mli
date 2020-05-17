(*

 ****************************** cl.mli ******************************


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

open Humanitas_tools
open Humanitas_physis

type t 
(** liste des civitates. La plus récente est en haut de la pile *)

val rev    : t -> t 
(** reverse order : pour l'affichage de la plus ancienne à la plus récente *)

val iter   : (Civitas.t -> unit) -> t -> unit
val map    : (Civitas.t -> 'a) -> t -> 'a list


val get    : t -> Civitas.t Tid.t -> Civitas.t
val search : t -> Rid.t           -> Civitas.t option
val filter : t -> Nid.t           -> Civitas.t list

val create : Espace.t -> ((Rid.t * Date.t) Nid.Nil.t) -> (Rid.t*Rv.Incola.t) list -> t * (Civitas.t list)
val update : Espace.t -> Date.t -> Im.t -> t -> (Rid.t*Rv.Incola.t) list -> t * (Civitas.t list)
(** Attention : l’im est mise à jour par effet de bord (regiones urbs) par cette fonction *)


val create_nil : Imd.origo Nid.Nia.t -> Nid.t list -> Civitas.t Nid.Nil.t
