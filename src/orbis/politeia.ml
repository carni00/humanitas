(*

 ****************************** Politeia.ml ******************************


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

type dominus = 
| Demos 
| Aristoi
(** Souverain, maître et propriétaire de la cité *)

type arkhe =
| Monarch
| Council
| Anarchy
(** Type de pouvoir exécutif national *)

type nid = Nid.t

type t = 
  {
  poleis : bool; (* y a-t-il des cités (au moins une)  *)
  boule  : bool; (* les cités disposent-elles d’institutions démocratiques *) 
  latifundium : float; (* taux de latifundium *)
  arkhe       : arkhe;
  }

let dominus         p = match p.poleis with 
                      | true  -> if p.boule then Demos else Aristoi
                      | false -> if p.latifundium < 0.2 then Demos else Aristoi
let is_aristocratic p = dominus p = Aristoi
let is_democratic   p = dominus p = Demos
let is_centralized  p = p.arkhe <> Anarchy
let is_civilized    p = p.poleis
let arkhe           p = p.arkhe

type natio =
  {
  p          : t;
  poleis     : bool;
  writing    : bool;
  metallurgy : bool;
  agriCopia  : float;
  sophia     : float;
  pil        : nid list;
  }
(* paramètres du calcul de la politeia *)


let to_string p = match dominus p, p.arkhe, p.poleis with
| Demos,  Anarchy, false -> "Anarchy"
| _    ,  Anarchy, false -> "Feudalism"
| Demos,  Anarchy, true  -> "Democratic cities"
| Aristoi,Anarchy, true  -> "Aristocratic cities"
| Demos  ,Council, _     -> "Democratic republic"
| Aristoi,Council, _     -> "Oligarchic republic"
| _      ,Monarch, _     -> "Monarchy"
(*| _                      -> "unknown"*)

let anarchy = { poleis = false; boule = false ; latifundium = 0. ; arkhe = Anarchy }

open Tfloat

let famine agriCopia = 10. ** (10. * (1.2 - agriCopia))
(* pression du manque de terres sur les changements politiques *)


let next_arkhe n =
  match (dominus n.p) , n.p.arkhe with 
  | Demos, Anarchy -> if n.writing 
                      && let x = famine n.agriCopia * n.sophia
                         in Random.int 100 < iof x 
                      then Council (* passage en république *)
                      else Anarchy
  | _    , arkhe   -> arkhe


let seditio n = 0
(* (mil + rel + oti)/2 + lux + opp×2 + (u - dxCopia) *)

let update n = 
  {
  n.p with
  poleis  = n.poleis;
  arkhe   = next_arkhe n;
  }

