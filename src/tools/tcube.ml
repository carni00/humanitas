(*

 ****************************** src/tools/tcube.ml ******************************


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

type 'a t = 'a array array array

let depth  c = Tarray.len c
let height c = Tarray.len c.(0)
let width  c = Tarray.len c.(0).(0)
let volume c = depth  c * height c * width c


let init d h w f =
  let wf k j = Array.init w (f k j) in
  let hf k = Array.init h (wf k) in
  Array.init d hf
(* comme Array.init *)

let make d h w a = init d h w (fun z y x -> a)

let plan c z = Array.get c z

let line c z y = Array.get (plan c z) y

let get  c z y x = Array.get (line c z y) x

