(*

 ****************************** src/tools/tfloat.ml ******************************


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


  let ( ++ ) = ( +  )
  let ( -- ) = ( -  )
  let  mult  = ( *  )
  let  div   = ( /  )
  let ( +  ) = ( +. )
  let ( -  ) = ( -. )
  let ( /  ) = ( /. )
  let ( *  ) = ( *. )
  let u = 1.
  let abs x = if x<0. then -. x else x

  let next x = floor x + u

  let arrondi ?(p=0) x = let o=10. ** (Std.foi p) in floor ( x / o + 0.5 ) * o

  let modulo x m = x - m * floor (x / m)

  let barycenter ?(p = 0.5) x y = x *. p +. y *. (1. -. p)

  let ibarycenter p q x y = barycenter ~p:(float p /. float q) x y

  let logb base x = log x /. log (float base)

  let log2      x = log x /. log 2.

  let rec swy aList bList x = match aList, bList with
    | _::[]  , b::[]             -> b (*fin de courbe, ou point unique*)
    | a::_   , b::_    when x<=a -> b (*début de courbe*)
    | a::c::_, b::d::_ when x<=c -> d - (d - b) * (c - x) / (c - a)
    | _::q   , _::r              -> swy q r x
    | _                          -> raise (Invalid_argument "Tfloat.swy")
  (* associe son ordonnée à l’abscisse d’un point d’une ligne brisée définie
     par une liste de points de coord (a,b) *)
  (* les abscisses données sont supposées croissantes *)

  let squot a x y = match y with
  | 0. -> a
  | _  -> x / y
  
  let quote x y s = match y with
  | 0. -> failwith s
  | _  -> x / y

(***************************** MODULE COUPLE *********************************)

module Couple =
struct

let make a b = (a,b)

let sum (a,b) (c,d) = (a+c),(b+d)

let mean (a,b) (c,d) = (a+c)/2.,(b+d)/2.

end


