(*

 ****************************** src/tools/tarray.ml ******************************


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

(*include Array*)

let len = Array.length

let sum = Array.fold_left (+) 0

let iter2 g aa ba =
  let l = len aa in 
  if l <> len ba then raise (Invalid_argument "Array.iter2")
  else Std.Ext.iter l (fun i-> g aa.(i) ba.(i))

let nIter = Array.iteri

let nIter2 g aa ba =
  let l = len aa in 
  if l <> len ba then raise (Invalid_argument "Array.nIter2")
  else Std.Ext.iter l (fun i-> g i aa.(i) ba.(i))

let filter_n g a =
  let length = len a in
  let rec f = function
  | i when i=length -> []
  | i when (g (a.(i))) -> i :: f (i+1)
  | i -> f (i+1) in
  f 0
(* renvoie la liste des numeros des item d’un array répondant au prédicat f *)

let alter g a = nIter (fun i e -> a.(i)<-g e) a
(* altère chaque élément de l’array a par la fonction g *)

let nAlter g a = nIter (fun i e -> a.(i)<-g i e) a

let exists p a =
  let n = len a in
  let rec loop i =
    if i = n then false
    else if p a.(i) then true
    else loop (i+1)
  in
  loop 0

