(*

 ****************************** src/tools/tmap.mli ******************************


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

(** Map operations *)
(** La map diffère de la matrice en ce qu’elle est la liste des seules cases non vides de la matrice équivalente *)

type ('c, 'b, 'a) t = private (('c*'b)*'a) list

val null : ('c, 'b, 'a) t
(** map dépourvue de données *)

val nth  : ('c, 'b, 'a) t -> 'c -> 'b -> 'a
(** [nth map y x] renvoie la valeur associée à la clé [(y,x)] *)

val snth : 'a -> ('c, 'b, 'a) t -> 'c -> 'b -> 'a
(** [snth a map y x] renvoie [a] si aucune valeur n’est associée à la clé [(y,x)] *)

val init : ('c->'b->'a) -> 'c list -> 'b list -> ('c, 'b, 'a) t
(** [init f [y0; ...; yn] [x0; ...; xn]] construit une map associant à chaque couple [(y,x)] la valeur [(f y x)] *)

val nMap : ('c->'b->'a->'d) -> ('c, 'b, 'a) t -> ('c, 'b, 'd) t
(** construction d’une nouvelle map d’égales dimensions *)
