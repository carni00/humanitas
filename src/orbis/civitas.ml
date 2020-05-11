(*

 ****************************** Civitas.ml ******************************


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
open Humanitas_physis
open Std
open Tfloat

(*let urbsTypeList = [VICUS ; URBS ; COLONIA ; RUINA]*)

type rank =
| Not_a_civitas
| Ring     (* capitale sans développement urbain *)
| Civitas  (* cité libre *)
| Urbs     (* capitale avec développement urbain *)
| Municipium (* cité étrangère soumise *)
| Colonia (* ancienne cité étrangère assimilée *)
(* le terme vicus est réservé aux gros villages non érigé en cité *)

type origo =
| Equal of Date.t
| Post of Date.t

type t = {
  origo : Date.t; (*date de fondation*)
  rid : Rid.t;  (*identifiant de l’emplacement géographique (regioId)*)
  civ : Nid.t;  (*civ ayant fondé la cité*)
  inc : Nid.t;  (*natio peuplant curently la cité*)
  nth : int;    (*rang chronologique de fondation (détermine le nom de la cité)*)
  rank : rank;  
  plebs : float; (*plèbe urbaine*)
  }

let make_from_imd o r =
  {
  origo = o;
  rid = r;
  civ = Nid.none;
  inc = Nid.none;
  nth = (-1);
  rank = Not_a_civitas;
  plebs = 0.;
  } 
 

let create_origo origo plebs =
  match origo with
  | Equal turn -> turn
  | Post  civ_origo ->
    let d = foi (Date.distance civ_origo Date.beginning) in
    let v = Tfloat.swy [  0.; 4000.; 5000.; 9999. ] [ d; d*0.8; d*0.2; 0.] (cut 0. 9999. plebs) in
    Date.add civ_origo (iof v)
(* évaluation de la date de fondation des cités*)


let create rid rs incola nth origo =
  let module Rvi = Rv.Incola in
  let nid = Rvi.nid incola in
  let instr= Rvi.instrumentum incola in
  let plebs= Rvi.densitas (rs) incola * 30. * instr in
  {
  origo = create_origo origo plebs ;
  rid = rid;
  civ = nid;
  inc = nid;
  nth = nth;
  rank = Civitas;
  plebs;
  } 

let update civitas rs incola =
  let module Rvi = Rv.Incola in
  {
  civitas with
  inc = Rvi.nid incola;
  plebs= Rvi.densitas (rs) incola * 30. * Rvi.instrumentum incola ;
  }

 
let origo c = c.origo
let rid  c = c.rid
let civ  c = c.civ
let incola c = c.inc
let rank c = c.rank
let nth  c = c.nth
let plebs c = c.plebs

let name_key c = c.civ,c.nth

