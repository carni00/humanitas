(*

 ****************************** src/tools/level.ml ******************************


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

type t = int * int * int

let make l i u = (*lower initial upper*)
  if (i < l) || (i > u) 
  then raise (Invalid_argument "Level.of_int: value out of bounds") ;
  (l,i,u)

let smake l i u = 
  let i = cut l u i in
  (l,i,u)


let to_float (_,i,_) = float i
let to_int   (_,i,_) = i

let lower_bound (l,_,_) = l
let upper_bound (_,_,u) = u

let set  (l,i,u) v = (l, cut l u v    , u)
let incr (l,i,u)   = (l, cut l u (i+1), u)
let decr (l,i,u)   = (l, cut l u (i-1), u)

let float = to_float
let int   = to_int
let min = lower_bound
let max = upper_bound

let extent  c = max c    - min c
let range   c = to_int c - min c

let floor_at f (l,i,u) = (l, cut f u i, u)

