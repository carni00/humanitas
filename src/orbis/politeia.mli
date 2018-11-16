(*

 ****************************** politeia.mli ******************************


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

open Std

type t
type nid = Nid.t

type dominus = 
| Demos   (** le peuple *)
| Aristoi (** les meilleurs citoyens *)
(** Souverain, maître et propriétaire de la cité *)

type arkhe =
| Monarch
| Council
| Anarchy
(** Détenteur du pouvoir exécutif *)

type summa =
| Anarkhia
| Feudalism
| Respublica
| Plutocracy
| Regnum
(** résumé du régime politique *)


type natio =
  {
  p          : t;
  poleis     : bool;
  latifundium: float;
  writing    : bool;
  metallurgy : bool;
  agriCopia  : float;
  sophia     : float;
  pil        : nid list;
  }


val is_aristocratic : t -> bool
val is_democratic   : t -> bool
val is_centralized : t -> bool
val is_civilized   : t -> bool
val has_boule   : t -> bool
val to_string : t -> string
val dominus   : t -> dominus
val arkhe     : t -> arkhe
val summa     : t -> summa

val anarchy : t
val update  : natio -> t

