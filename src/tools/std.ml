(*

 ****************************** Std.ml ******************************


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


type 'a id = int


let xor  a b = if a then not b else b
let xnor a b = if a then b else not b
let both_or_none = xnor
let is bool = fun a b -> if bool then a else b
let ( =>  ) = is
let ( ^^  ) = ( @ ) (*concaténation de liste*)
let ( |>  ) a f = f a

let identity    = fun  a     -> a
let flip      f = fun  x y   -> f  y x
(* let curry     f = fun  x y   -> f (x,y)
 * let uncur     f = fun (x,y)  -> f  x y *)
let compose g f = fun  x     -> g (f x)
let ( |- )  g f = fun  x     -> f (g x)

(*let ( |- ) f g a = g (f a)*)

let fst3 (i,_,_) = i
let snd3 (_,i,_) = i
let trd3 (_,_,i) = i

let fst4 (i,_,_,_) = i
let snd4 (_,i,_,_) = i
let trd4 (_,_,i,_) = i
let frh4 (_,_,_,i) = i

let fst5 (i,_,_,_,_) = i
let snd5 (_,i,_,_,_) = i
let trd5 (_,_,i,_,_) = i
let frh5 (_,_,_,i,_) = i
let ffh5 (_,_,_,_,i) = i

let boi i = (i > 0)
let bof f = (f > 0.0)

let iob b = if b then 1 else 0
let iof = int_of_float
let ios = int_of_string

let foi = float_of_int
let fos = float_of_string

let soi = string_of_int
let sof = string_of_float

let min3 a b c = min a (min b c)
let max3 a b c = max a (max b c)
let cut  a b x = max a (min b x)

let pi = 3.14159269


(*module Array' = struct
  include Array
  type ('a,'b) t = 'b array
end*)

module Opt = struct
let value = function
| Some a  -> a
| None -> raise (Failure "Opt.value")

let optdo f = function
| Some x -> f x
| None -> ()

let smap default f = function (*optfun*)
| Some a  -> f a
| None    -> default

end

module Ext =
struct

let  fill_nth_bit int k = int lor        (1 lsl k)
let clear_nth_bit int k = int land (lnot (1 lsl k))
let  read_nth_bit int k = (int lsr k) land 1


let squot a x y = match y with
| 0 -> a
| _ -> x / y

let quote x y s = match y with
| 0 -> failwith s
| _ -> x / y

let modulo x m = 
  let r = x - x/m*m in
  if x>=0 then r
  else if r<>0 then r+m
  else r

let euclid x y = x/y, x mod y
(*let intSum n = (n+1)*n/2 (* somme des entiers de 1 à n *)*)

let arithmean a b = (a+b)/2
let weighmean a b c d = (a*c + b*d) / (c+d)

let rangeMean a b range d =
  let d = cut 0 range d in
  (a*(range-d) + b*d)/range

let rec swy aList bList x = match aList, bList with
  | _::[]  , b::[]             -> b (*fin de courbe, ou point unique*)
  | a::_   , b::_    when x<=a -> b (*début de courbe*)
  | a::c::_, b::d::_ when x<=c -> d - (d - b) * (c - x) / (c - a)
  | _::q   , _::r              -> swy q r x
  | _                          -> raise (Invalid_argument "swy")
(* associe son ordonnée à l’abscisse d’un point d’une ligne brisée définie
   par une liste de points de coord (a,b) *)
(* les abscisses données sont supposées croissantes *)

let log2 int = 
  let rec f b = 
    if b=0 then 0
    else if int / (1 lsl b)>0 then b
    else f (b-1) in
  f 29
(* 29 : apparemment, les int valant 10^9 ou plus ne sont pas gérés (cf ocaml interpréteur)*)
(* logarithme de base 2 *)

let iter n f = 
  for i=0 to (n-1) 
  do f i done

let rec fold_left f a = function
    0 -> a
  | n -> f (fold_left f a (n-1) ) (n-1)

let rec applique f a = function
    0 -> a
  | n -> f (applique f a (n-1) )


end


(***************************** MODULE COUPLE *********************************)

module Couple =
struct

let make a b = (a,b)

let sum (a,b) (c,d) = (a+c),(b+d)

let mean (a,b) (c,d) = (a+c)/2,(b+d)/2

let foi (a,b) = (foi a, foi b)

end

(***************************** MODULE RANDOM **********************************)


module Random = 
struct
include Random

let sFloat max = float(max *. 2.) -. max

let sign() = (bool() => 1) (-1)

end

(***************************** MODULE COMPARE **********************************)

module Compare = struct
  type sign =
  | Pos
  | Neg
  | Nul
  
  type situation =
  | Above
  | Below
  | Equal of int
  | Between of int*int
  
  let sign a =
    if a>0 then Pos
    else if a<0 then Neg
    else Nul
  
  let situ x a = match sign (compare x a) with
  | Neg -> Below
  | Nul -> Equal a
  | _   -> Above
  
  let bsitu x a b = match sign (compare x a), sign (compare x b) with
  | Neg, _ -> Below
  | Nul, _ -> Equal a
  | _  , Neg -> Between (a,b)
  | _  , Nul -> Equal b
  | _        -> Above
  (* suppose a<b *)
end


