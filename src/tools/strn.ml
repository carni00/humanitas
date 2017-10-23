(*

 ****************************** Strn.ml ******************************


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

let pth strn = "( "^strn^" )"

let commonInt ordreDeGrandeur int =
  let rec f intHead strnTail =
    let q,r = Ext.euclid intHead 1000 in
    if q>0 then f q ((Printf.sprintf " %03d" r)^strnTail)
    else (soi r)^strnTail in
  f (int / ordreDeGrandeur * ordreDeGrandeur) ""
(* limite la précision d’un entier et en fait un string avec des espaces séparant chaque groupe de 3 chiffres *)
(* ordreDeGrandeur est 100 pour un résultat comme 161 800 *)


let longInt approx int =
  let ordreDeGrandeur = iof(10. ** (foi approx)) in
  commonInt ordreDeGrandeur int
(* limite la précision d’un entier et en fait un string avec des espaces séparant chaque groupe de 3 chiffres *)
(* approx(imation) est le nombre de chiffres négligés (1=unités, 2=dizaines etc.) *)

let rdblint chiffres int =
  let log = iof(floor (log10 (foi int))) in
  let ordreDeGrandeur = iof (10. ** foi(max 0  (log - chiffres + 1))) in
  commonInt ordreDeGrandeur int
(* limite la précision d’un entier et en fait un string avec des espaces séparant chaque groupe de 3 chiffres *)
(* chiffres est le nb de chiffres significatifs que l'on souhaite conserver *)


let float precision float =
(*  let ordreDeGrandeur = 10. ** (foi (-precision)) in*)
(*  let float = foi(iof(float *. ordreDeGrandeur))/. ordreDeGrandeur in*)
  let float = Tfloat.arrondi ~p:precision float in
  match precision with
  | p when p>=0 -> longInt p (iof float)^"."
  | (-1) -> Printf.sprintf "%.1f" float
  | (-2) -> Printf.sprintf "%.2f" float
  | _    -> Printf.sprintf "%.3f" float
  (* réduit la précision d’un float à *precision chiffres après la virgule et en fait une string avec *precision
zéro après la virgule si nécessaire *)

let rdblfloat chiffres floatt =
  let log = iof(floor (log10 (floatt))) in
  let precision = (log - chiffres + 1) in
  float precision floatt


let percent precision fLoat = let centieme = fLoat *. 100. in
  if precision < 0 then float precision centieme^" %"
  else soi(iof centieme)^" %"
(* comme float, mais affiche du style 2,34% au lieu de 0,0234 *)


(*
(* let year y = Printf.sprintf "%04d" y *)
let year y = (soi y)

let date = function
| Date.G g -> let g = (g:>int) in if g<0 then soi(-g)^" BCE" else soi g ^" CE"
| Date.N _ -> "national date"
*)



let latitude   lat = 
  let sof = float (-1) in
  if lat<=0. then sof (-. lat)^"\176N" 
  else sof lat^"\176S"

let longitude  lon = 
  let sof = float (-1) in
  if lon<=0. then sof (-. lon)^"\176W" 
  else sof lon^"\176E"

let coords lat lon = pth(latitude lat^", "^longitude lon)

let km  precision int = longInt precision int^" km"
let km2 precision int = longInt precision int^" km2"

