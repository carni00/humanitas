(*

 ****************************** strategica.ml ******************************


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
module Nil = Nid.Nil
module Nim = Nid.Nim
module N   = Natio
module Pa  = Politeia
module J   = Junctiones
module Jn  = Junctiones.Natio

type nid = Nid.t

(*******************************************************)

type t = {
  stratiotikon : Stratiotikon.t ;
  tactic_list  : J.tactic Nil.t ;
  rogatio_list : J.rogatio Nil.t ;
  }
(* la stratégie d'une natio (centralisée) *)

let stratiotikon s = s.stratiotikon
let tactic_list  s = s.tactic_list
let rogatio_list s = s.rogatio_list

let tactic s nid = Nil.snth (J.Defensive) (tactic_list s) nid 


(*******************************************************)

module Data = struct

  type d = {
    stratiotikon_list : Stratiotikon.t Nil.t;
    rogatio_map   : Junctiones.rogatio Nim.t;
    tactic_map    : Junctiones.tactic Nim.t;
    }
  (* l'ensemble des données stratégiques, classées par thème pour application *)
  
  let rogatio d i j = Nim.snth Junctiones.N_rogatio d.rogatio_map i j
  let tactic  d i j = Nim.snth Junctiones.Defensive d.tactic_map i j
  
  let of_nil (nil) =
  ( {
    stratiotikon_list = Nil.init (fun nid->Stratiotikon.make) nil;
    tactic_map    = Nim.empty;
    rogatio_map   = Nim.empty;
    } : d )
  (* Pour donner une valeur initiale à humanitas.strategics *)
  
  let make (sl: t Nid.Nil.t) =
    {
    stratiotikon_list = Nil.map (fun s->stratiotikon s) sl;
    tactic_map    = Nim.of_ll (Nil.map (fun s->tactic_list  s) sl);
    rogatio_map   = Nim.of_ll (Nil.map (fun s->rogatio_list s) sl);
    } 
  (* créations des stratégies IA puis
  reclassement par types de données de la liste des stratégies par nation *)
                                    
  let jStrategies d = ( {
    Junctiones.rogatio_map   = d.rogatio_map;
    Junctiones.tactic_map    = d.tactic_map;
    } : Junctiones.strategies )
    
end


(*********************************************)

let make = 
  {
  stratiotikon = Stratiotikon.make ;
  rogatio_list = Nil.empty;
  tactic_list  = Nil.empty;
  }

let feudum pil =
  {
  stratiotikon = Stratiotikon.init 6 2 0;
  rogatio_list = Nil.empty;
  tactic_list  = Nil.init (fun nid -> J.Offensive J.Conquest) pil; (* on attaque partout pour conquérir de nles latifundia *)
  }

let anarchy d n pil = 
  {
  stratiotikon = Stratiotikon.init 5 0 0;
  rogatio_list = Nil.empty;
  tactic_list  = 
    let tactic pid = 
(*    if gn.terre_cultivable_adjacente == 0 then J.Offensive J.Conquest (* manque de terre grave *)*)
      if N.copia n < 1.0  && Data.tactic d pid (N.nid n) == J.Offensive J.Conquest then J.Offensive J.Release
      (* attaque venant de l’étranger + manque de terre *)
      else J.Defensive in
    Nil.init (fun pid -> tactic pid) pil
  }
(* tactique des nations anarchiques *)


let tactic_tw_nationes d n pil =
  let tactic pid = 
(*  if gn.terre_cultivable_adjacente == 0 then J.Offensive J.Conquest (* manque de terre grave *)*)
    if N.copia n < 1.0  && Data.tactic d pid (N.nid n) == J.Offensive J.Conquest then J.Offensive J.Release
    else J.Defensive in
  Nil.init (fun pid -> tactic pid) pil
(* tactique des cités envers les autres nations *)


let tactic_tw_natives d n pil =
  let open Tfloat in
  let j = N.junctiones n in
  let under_attack = Jn.am_i_under_attack j in
  let ric = N.imperium n / N.chora n in
  if under_attack == true
  then (Nid.none, J.Retreat)
  else if ric < 2. 
  then (Nid.none, J.Offensive J.Conquest)
  else (Nid.none, J.Defensive)
(* tactique des cités envers les natives *)


let poleis_tactic d n pil = Nil.add (tactic_tw_nationes d n pil) (tactic_tw_natives d n pil)


let poleis s d n pil = 
  {
  stratiotikon = s;
  rogatio_list = Nil.empty;
  tactic_list  = poleis_tactic d n pil;
  }

let a_poleis d n pil = 
  let open Tfloat in
  let p = Natio.partitio n in
  let hum = Partitio.humanitas p in
  let str = Partitio.stratiotikon p in
  let lux = Partitio.lux p in
  let rel = iof (10. * (cut 0. u (0.3 * hum / str ))) in
  let opp = iof (10. * (cut 0. u (lux * hum / str ))) in
  let rel = min (9--opp) rel in
  poleis ( Stratiotikon.init (9--rel--opp) rel opp ) d n pil

let d_poleis d n pil = poleis ( Stratiotikon.init 10 0 0 ) d n pil

let republic d n pil = 
  {
  stratiotikon = Stratiotikon.init 9 1 0;
  rogatio_list = Nil.empty;
  tactic_list  = poleis_tactic d n pil;
  }

(*** FIXME : créer une IA républicaine  ***)



let update d n prl s =
  let pil = Nil.key_list prl in
  let pa  = N.politeia n in
  match Pa.is_civilized pa, Pa.is_centralized pa, Pa.is_aristocratic pa with
  | false, false, false -> anarchy d n pil
  | false, false, true  -> feudum pil
  | true , false, false -> d_poleis  d n pil
  | true , false, true  -> a_poleis  d n pil
  | _    , true , _     -> republic  d n pil
(* strategie initiale *)



