(*

 ****************************** Ars.ml ******************************


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
open Tfloat

type t = 
| N_ARS 
| AGR 
| MET 
| WRI 
| NAV 
| GUN 
| STE 
| CMB 
| ELE


let first = AGR


let next = function
| AGR -> MET
| MET -> WRI
| WRI -> NAV
| NAV -> GUN
| GUN -> STE
| STE -> CMB
| CMB -> ELE
| ELE -> N_ARS
| N_ARS -> N_ARS


(*let list = [AGR; MET; WRI; NAV; GUN; STE; CMB; ELE]*)
let list = [AGR; MET]
let beginList = [AGR]


let eff (artes) (ars) = if List.mem ars artes then 2. else 1. 

type eff_tab =
  {
  sap : float;
  mil : float;
  opp : float;
  }

let eff_tab artes =
  let ae = eff artes in
  let met = ae MET in
  let gun = ae GUN in
  let opp = met * gun in
  {
  sap = ae WRI * ae ELE;
  mil = opp * ae CMB;
  opp = opp;
  }

let instMaxFun = function
  AGR ->   50.
| MET ->  100.
| STE ->  200.     
| CMB ->  300.
| ELE ->  400.
| _   ->  0.

let rec instMax = function
  | []     -> 0.
  | a :: q -> max (instMaxFun a) (instMax q)
(* instrumentum maximum pour une liste d’ars acquise *)


let level = function
  AGR ->   0
| MET ->  10
| WRI ->  10
| NAV -> 100
| GUN -> 100
| STE -> 160
| CMB -> 220
| ELE -> 220
| N_ARS -> 0


let to_nil = function
| AGR -> Color.Nil.make Color.Nuance.vert    400 200
| MET -> Color.Nil.make Color.Nuance.rouge   500 200
| WRI -> Color.Nil.make Color.Nuance.indigo  500 180
| NAV -> Color.Nil.make Color.Nuance.bleu    600 200
| GUN -> Color.Nil.make Color.Nuance.ambre   600 200
| STE -> Color.Nil.make Color.Nuance.corail  600 400
| CMB -> Color.Nil.make Color.Nuance.vert    900 200
| ELE -> Color.Nil.make Color.Nuance.jaune   900 400
| _   -> Color.Nil.make Color.Nuance.none    100   0


let artes_to_nil = function
| []    -> Color.Nil.make Color.Nuance.none  0 0
(*| artes -> to_nil (List.hd artes)*)
| artes -> to_nil (Tlist.last artes)

let rec artes_to_rvb = function
| []        -> Color.Rvb.black
| ars :: q  -> Color.Rvb.lum_mult (Color.Rvb.add (Color.Rvb.of_nil (to_nil ars)) (artes_to_rvb q) ) 0.9

