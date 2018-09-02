(*

 ****************************** Aedificium (anciennement Kapital).ml ******************************


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

module P  = Partitio
module Pa = Politeia


type t = {
  sophia   : float;
  fides    : float;
  seditio  : float;
  ususList : Partitio.UsusList.t;
  vis      : float;
  }
(* capitaux = données affectées par un processus d'accumulation *)
(* == elles dépendent entre autres d'elles-mêmes en N-1 *)

type natio = {
  k       : t;
  g      : G.Natio.t;
  plebs  : float;
  pArtes : Ars.t list;
  cArtes : Ars.t list;
  pp     : P.t;
  pr     : P.Record.t;
  luc    : P.t;
  }


let sophia     k = k.sophia
let fides      k = k.fides
let seditio    k = k.seditio
let ususList   k = k.ususList
let vis        k = k.vis

let libertas   k = max 0. (k.sophia - k.fides)


let null =
  {
  seditio= 0.;
  fides  = 0.;
  sophia = 0.;
  ususList = Partitio.UsusList.null ;
  vis = 0.;
  }

let make ~sophia ~fides ~seditio ~ususList ~vis = { 
  sophia;
  fides;
  seditio;
  ususList;
  vis;
  }


(*
let vis g nid eMil plebs =
  let exe (*exercitus*)= prod eMil (plebs/Dx.npc) in
(* Exercitus ne doit pas être dépendant de npc, de façon à ce que la correction de npc n’implique pas une correction de ic (ligne suivante). Plebs l'étant, on redivise par npc (sic) *)
  let impTiCost = min exe (prod (G.impTi g nid) J.icm) in
(* en unités d’exercitus *)
(* soit une extension d'une chora pour une militaria de 1% + xp + MET *)
  quot (exe - impTiCost) (G.impChora g nid + G.impCe g nid) 0
(* L'imperium a un coût d'occupation fixe (faible), le reste de l'armée défend en priorité la chora
et les chora étrangères occupées *)
*)
let  compute_vis n = 
  let ager = G.Natio.impAmp n.g in
  let desertum = G.Natio.imperium n.g - ager in
  let area = ager + desertum * 0.5 in
  let base = squot 0. (P.mil n.luc) area in
  max 0. (8. + log2 base)

let update n = 
  let atr = P.Record.attrib  n.pr in
  let act = P.Record.actio   n.pr in
  let fru = P.Record.fructus n.pr in
  let f k i p = k*i + p*(u-i) in
  let g i = f (n.k.sophia / 3.) i (P.sap fru * 0.40) in
  {
  seditio  = f (n.k.seditio) 0.90 ( P.opp fru + squot 0. (P.alienatio atr) (P.humanitas atr) );
  fides    = f (n.k.fides)   0.90 ( P.servitium act + (P.rel fru) ** 0.5 ) ;
  sophia   = g 0.99 + g 0.999 + g 0.9999 ;
  (* sophia tend très lentement vers sapientia * 1.20 *)
  ususList = Partitio.UsusList.create n.k.ususList n.pp atr n.pArtes n.cArtes;
  vis      = compute_vis n ;
  }
(* capitaux = données affectées par un processus d'accumulation *)
(* en fonction de k p et artes (tour-1), des productions (année passée) et de la pyramide et des artes du tour n *)

(**************************************************************************)
(*
let next (d, k, c, pp, pArtes) p  =
  let e = P.alter_all p [PYRAMID(Dx.pyramid d,(iof k.plebs)); PRODUCT(k.usus,(iof c.efficientia),pArtes)] in
  let d = d in (*Dx.v j g(d, e.labor, k.sophia, k.fides, c.copia, k.plebs) in*)
  v (k, pp, pArtes) (p, e) (Dx.pyramid d, pArtes, 0)
(* fonction used par W_partitio pour projection stratégique *)
(* la projection se fait à Artes constantes, les découvertes étant imprévisibles *)
*)


