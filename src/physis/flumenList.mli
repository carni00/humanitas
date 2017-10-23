(*

 ****************************** flumenList.mli ******************************


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

(** Génération de l’hydrographie d’une physis, sous la forme d’une liste de flumines *)

type t = Flumen.t list

val create : Espace.t -> int Rid.Array.t -> int Rid.Array.t -> t*(int Rid.Array.t)
(** associe à un espace, et à une carte des altitudes, et une carte des latitudes de températures, une liste de fleuves, et une carte d’altitude un peu modifiée (ravinement) *)


(* EOF *)
