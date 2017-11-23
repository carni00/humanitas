(*

 ****************************** Junctiones.ml ******************************


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
module Nix = Nid.Nix

type nid = Nid.t

type offensive =
| Conquest
| Release


type tactic = 
| Offensive of offensive
| Retreat
| Defensive 


type relatio = 
| Bellum 
| Pax
| N_relatio


type rogatio =
| R_Bellum
| R_Pax
| N_rogatio


type t = {
  relatioMap  : relatio Nid.Nim.t ;
(* matrice (symétrique) des relations diplomatiques *)
(* seules les valeurs différentes de N_relatio sont enregistrées *)
  tacticMap   : tactic  Nid.Nim.t ;
(* copie de la matrice des tactiques (importée depuis stratégies) *)
  }

type natio = {
  chora : float;
  imperium : float;
  vis : float;
  }
(* infos nationales dont finiumMutatio dépend *)

type strategies = {
  rogatio_map   : rogatio Nim.t;
  tactic_map    : tactic Nim.t;
  }
(* données stratégiques à partir desquelles on met à jour les junctiones *)
  

let relatio j y x = Nim.bnth (N_relatio) j.relatioMap y x 


let is_attacking j = 
  let mx = Nix.empty() in
  let set_mx y x a = ( match a with
    | Offensive off -> (
      match relatio j y x with
      | Pax -> ()
      | _   -> Nix.set mx y x (Some off) )
    | _ -> () ) in
  Nim.iteri set_mx j.tacticMap;
  fun y x -> Nix.get mx y x 
(** is y attacking x *)
(** performance : dès l’obtention de j, on calcule la matrice des résultats, et on renvoie la fonction qui pique dans cette matrice *)


let warNb j n = 
  let f ((n1,n2),re) = (n1=n || n2=n) && re=Bellum in
  Tlist.census f j.relatioMap
(* nombre de guerres actuellement menées par la nation n*)

let warNatioList j n =
  let rec f = function
  | [] -> []
  | ((n1,n2),re)::q when n1=n && re=Bellum -> n2::f q
  | ((n1,n2),re)::q when n2=n && re=Bellum -> n1::f q
  | _::q -> f q in f j.relatioMap
(* liste des nationes avec lesquelles n est en guerre *)




(*****************************************************************************************)

let make =
  {
  relatioMap= Nim.empty;
  tacticMap = Nim.empty;
  }
(* matrices initiales *)


let newRelationes rom =
  let f n1 n2 rog = match rog with
  | R_Bellum  -> Bellum
  | R_Pax when n1<n2 && Nim.snth N_rogatio rom n2 n1=R_Pax -> Pax
  | _ -> N_relatio in
  let map = Nim.mapi f rom in
  Nim.filter (fun rel->(rel<>N_relatio)) map
(* nles relatio consécutives des rogationes de rom *)
(* fonction used ici et dans Vetera *)
(* Vetera suppose que y déclare la guerre à x quand ((y,x),Bellum) *)


let nextRelatioMap rem rom =
  let newRelationes = Nim.sort (Nim.norm (newRelationes rom)) in
  (* Nim.norm a priori inutile, à vérifier le moment venu *)
  let nextRelatio (y,x) pr nr = match nr with
  | N_relatio -> pr (*absence de newRelatio <=> on garde la previousRelatio*)
  | _ -> nr in
  Nim.smap2 N_relatio N_relatio nextRelatio rem newRelationes
(* génération de la map des relations diplomatiques *)


let update j nl sd =
  let rem = nextRelatioMap j.relatioMap sd.rogatio_map in
  {
  relatioMap = rem;
  tacticMap = sd.tactic_map;
  }
(* mise à jour des matrices relatio_map et finiumMutatio après établissement des stratégies par les joueurs *)


module Natio = struct
  type j = t
  type t = {
    relatio_list  : relatio Nid.Nil.t ;
  (* liste de nos relations diplomatiques *)
  (* seules les valeurs différentes de N_relatio sont enregistrées *)
    theirTactic_list : tactic  Nid.Nil.t ;
  (* liste des tactiques des autres nations envers nous *)
  (* la liste de nos tactiques envers les autres nations est dans notre strategica *)
  }

  open Tfloat

(*  let make j nil nid = *)

  let null = {
    relatio_list     = Nid.Nil.empty ;
    theirTactic_list = Nid.Nil.empty ;
    }


  let compute_theirTactic_list j our_id =
    let rec filter = function
    | [] -> []
    | ( (sujet,objet), t ) :: q when objet == our_id -> (sujet,t) :: filter q
    | ( (sujet,objet), t ) :: q                      -> filter q in
    filter j.tacticMap
    (* liste des tactiques des autres nations envers nous *)

  let compute_relatio_list j our_id =
    let rec filter = function
    | [] -> []
    | ( (nid1,nid2), t ) :: q when nid1 == our_id -> (nid2,t) :: filter q
    | ( (nid1,nid2), t ) :: q when nid2 == our_id -> (nid1,t) :: filter q
    | ( (nid1,nid2), t ) :: q                     -> filter q in
    filter j.relatioMap
  (* liste de nos relations diplomatiques *)
  (* seules les valeurs différentes de N_relatio sont enregistrées *)


  let make j nid =
    {
    relatio_list     = compute_relatio_list j nid;
    theirTactic_list = compute_theirTactic_list j nid;
    }

  let relatio_list     nj = nj.relatio_list
  let theirTactic_list nj = nj.theirTactic_list
  
end
(*ce que la natio connait des relations internationales*)


let natioList j nil = Nid.Nil.init (fun nid -> Natio.make j nid) nil


(*EOF*)
