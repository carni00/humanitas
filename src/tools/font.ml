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

open Std

(** common data about building good-looking font *)
  type id = 
  | Default
  | Ubuntu
  | Freesans
  | Freeserif

  let idList = [
   Default;
   Ubuntu;
   Freesans;
   Freeserif;
  ]

  let path = function
  | Default   -> "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
  | Ubuntu    -> "/usr/share/fonts/truetype/freefont/FreeSerif.ttf"
(*  | Default   -> "/usr/share/fonts/truetype/ubuntu-font-family/Ubuntu-R.ttf"*)
(*  | Ubuntu    -> "/usr/share/fonts/truetype/ubuntu-font-family/Ubuntu-R.ttf"*)
  | Freesans  -> "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
  | Freeserif -> "/usr/share/fonts/truetype/freefont/FreeSerif.ttf"

  type t = id*int 

  let sizeMin = 6
  let sizeMax = 29

  (*ubuntu pour les petites polices, parce qu’elle est moins floue que les free*)
  (*freesans pour le 10 et le 11, parce que la ubuntu est «grasse» pour ces deux tailles*)

  let validId id size =
    if id <> Default then id 
    else if size<10 then Ubuntu
    else if size<12 then Freesans
    else Freeserif

  let valid id size = validId id size, cut sizeMin sizeMax size

(*EOF*)
