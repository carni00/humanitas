(*

 ****************************** date.ml ******************************


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

(*type turn = int*)
type gregorian= int
type national = Nid.t * int

type t = 
| G of gregorian
| N of national
| Unknown

let agriculture = G(-8500)
(*let ceramique   = G(-5200) (*Obeid I : faucille en argile cuite*)*)
(*let masonry     = G(-5000) (*Obeid I : ziggurat*)*)
let metallurgy  = G(-4500) (*Bulgarie : fusion du cuivre*)
let irrigation  = G(-4200) (*Eridu : 4000 habitants*)
let writing     = G(-3400)
(*let bronze      = G(-3000) *)
(*let latifundium = G(-3000) *)
let beginning   = irrigation

let inc date    = match date with
| G date -> G (date + 1)
| N _date -> assert false
| Unknown-> assert false

let add date i  = match date with 
| G date -> G (date + i)
| N _date -> assert false
| Unknown-> assert false

let make int = G int

let to_int   = function
| G date -> (date:>int)
| _ -> assert false

let distance a b= match a,b with
| G a, G b -> abs (b-a)
| _ -> assert false

let precedes a b= match a,b with
| G a, G b -> a<b
| _ -> assert false
