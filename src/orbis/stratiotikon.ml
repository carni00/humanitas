(*

 ****************************** stratiotikon.ml ******************************


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


open Humanitas_tools
open Std


(*type attributio = Mil | Rel | Opp*)
type t = {
  mil : int;
  rel : int;
  opp : int;
  }

let null = {
  mil = 0;
  rel = 0;
  opp = 0;
  }

let sn (* step number *) = 10

let otium s = sn - ( s.mil + s.rel + s.opp )

let generic_init convert mil rel opp =
  let f max v = cut 0 max (convert v) in
  let o = f  sn      opp in
  let r = f (sn-o)   rel in
  let m = f (sn-o-r) mil in
  {
    mil = m;
    rel = r;
    opp = o;
  }

let init  = generic_init (fun i -> i)
let float = generic_init (fun v -> iof (v *. foi sn) )


(*let inc s a = if otium s < 1 then s else match a with*)
(*| Mil -> { s with mil = s.mil + 1 }*)
(*| Rel -> { s with rel = s.rel + 1 }*)
(*| Opp -> { s with opp = s.opp + 1 }*)

(*let dec s a = match a with*)
(*| Mil -> if s.mil > 0 then  { s with mil = s.mil - 1 } else s*)
(*| Rel -> if s.rel > 0 then  { s with rel = s.rel - 1 } else s*)
(*| Opp -> if s.opp > 0 then  { s with opp = s.opp - 1 } else s*)

let f i = foi i /. foi sn
let mil s = f  s.mil   
let rel s = f  s.rel  
let opp s = f  s.opp 
let oti s = f (otium s)

