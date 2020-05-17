(*

 ****************************** civitas.mli ******************************


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

(** Chaque nation peut bâtir une cité *)

open Humanitas_physis


type rank =
| Not_a_civitas
| Ring  (* capitale sans développement urbain *)
| Civitas  (* cité libre *)
| Urbs  (* capitale avec développement urbain *)
| Municipium (* cité étrangère soumise *)
| Colonia (* ancienne cité étrangère assimilée *)
(* le terme vicus est réservé aux gros villages non érigé en cité *)

type origo =
| Equal of Date.t
| Post of Date.t

type t

val make_from_imd : Date.t -> Rid.t -> t
val create : Rid.t -> float -> Rv.Incola.t -> int -> origo -> t
val update : t     -> float -> Rv.Incola.t -> t

val origo : t  -> Date.t
(** date de fondation *)

val rid : t -> Rid.t
(** emplacement *)

val rank : t -> rank
val civ  : t -> Nid.t
val incola : t -> Nid.t
val nth  : t -> int
val plebs: t -> float
val name_key : t -> Nid.t * int
val compare_origo : t -> t -> int
