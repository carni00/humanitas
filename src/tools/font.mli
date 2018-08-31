(*

 ****************************** font.ml ******************************


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

(** Common data about building good-looking font *)
type id = 
  | Default
  | Sans
  | Serif

type t = id*int 

val idList : id list
  (** list of every font style *)

val path : id -> string
  (** returns a string usable as a path for the font [id] *)

val sizeMax : int
  (** maximum size of the fonts *)

val valid : id -> int -> t
(** [valid id size] returns a [Font.t] with compatible id and size size *)

