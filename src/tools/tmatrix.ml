(*

 ****************************** src/tools/tmatrix.ml ******************************


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


type 'a t = 'a array array

let height mx = Tarray.len mx
let width  mx = Tarray.len mx.(0)
let surface mx = height mx * width mx

let init h w f =
  let g j = Array.init w (f j) in
  Array.init h g
(* comme Array.init *)



let make h w a = init h w (fun _ _ -> a)

let line   mx y = Array.get mx y

let get mx y x = Array.get (line mx y) x

let column mx x = Tlist.init (height mx) (fun i -> get mx i x)

let nIter f mx =
  for y = 0 to height mx - 1 do
    for x = 0 to width  mx - 1 do
      f y x (get mx y x)
    done
  done


let set mx y x a = mx.(y).(x)<-a

let ySum mx y = Tarray.sum (line mx y)
(* somme des valeurs de la ligne y de la matrice mx *)

let xSum mx x = Std.Ext.fold_left (fun s y->s+get mx y x) 0 (height mx)
(* somme des valeurs de la colonne x de la matrice mx *)

(* let to_lili (mx : 'a array array) =
 *   let h = Array.length mx in
 *   let rec f j =
 *   if j = h then ( [] : 'a list list )
 *   else Array.to_list mx.(j) :: f (j+1) in
 *   f 0 *)
(* matrix_to_lili *)


