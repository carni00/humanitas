(*

 ****************************** src/game/picking.ml ******************************


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

open Humanitas_physis


type element =
| Regio of Rid.t


(*module type ColorEnv = sig
  val espace  : Espace.t
end*)

module type Color = sig
  type t = private int
(*  val espace  : Espace.t*)
  val none    : t
  val of_element : element -> t
  val to_element : t -> element option
  val to_int32   : t -> int32
  val of_int32   : int32 -> t
end


module Color = (*functor (ColorEnv : ColorEnv) ->*) struct
  type t = int
  let none       = (-1)
  
  let of_element = function
  | Regio rid  -> (rid:>int)
  
  let to_element i =
         if i < 0 then None
    else (*if i < espaceSize then*) Some(Regio (Rid.oi i))
(*    else None*)

  let to_int32   = Int32.of_int
  let of_int32   = Int32.to_int
end




