(*

 ****************************** Rid.ml ******************************


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

type t = int
type rid = t (*Rid.t*)

let first = 0
let none = first-1

let oi i = i

let oe = function
| None -> none
| Some rid-> rid

let ti i = i

let add rid i = rid + i

let iter n f = 
  for i=first to (first+n-1) 
  do f i done

module Array = 
  struct
  include Array
  include Tarray
  type 'a t = 'a array
  let empty = [| |]
  let blit a1 a2 = Array.blit a1 0 a2 0 (Array.length a1)
  let to_array a = a
  let update f ria = Array.iteri (fun rid e -> Array.set ria rid (f rid e) ) ria

  end 



