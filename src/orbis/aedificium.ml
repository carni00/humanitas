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
(*  instrumentum : float;*)
  sophia   : float;
  fides    : float;
  seditio  : float;
  ususList : P.usus list;
(*  efficientia : float;*)
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
  ususList = [] ;
  vis = 0.;
  }

let make ~sophia ~fides ~seditio ~ususList ~vis = { 
  sophia;
  fides;
  seditio;
  ususList;
  vis;
  }

let ususFun (pu(*previous usus*), pp(*previous prod*), p(*current prod*), pa(*prev artes*), ca(*current artes*), ia(*involved artes*)) =
  let is_ars_new (a) = not(List.mem a pa) && (List.mem a ca) in
  let de (a) = if (is_ars_new a) then 0.5 else 1. in (*discovery effect*)
  let de = Tlist.fprod (List.map de ia)
  and re = max u (squot u p pp) (*recruitment effect*)
  and iu = (if p=0. then 0. else (pu + (u - pu) * 0.33) )(*increased usus*) in
  (squot 0. iu re) * de
(* expérience d'une catégorie de spécialiste *)

module UsusList = struct
  type t = P.usus list
  let f attrib list = match Tlist.optAssoc attrib list with Some v  -> v | None -> 1.
  let mil = f P.MIL
  let rel = f P.REL
  let opp = f P.OPP
  let create (pul(*prev ususList*), pp(*prev partitio*), p(*curr partitio*), pa(*prev artes*), ca(*curr artes*)) =
    let mil = ususFun(mil pul, P.mil pp, P.mil p, pa, ca, [Ars.MET;Ars.GUN;Ars.CMB])
    and rel = ususFun(rel pul, P.rel pp, P.rel p, pa, ca, [Ars.WRI;Ars.ELE])
    and opp = ususFun(opp pul, P.opp pp, P.opp p, pa, ca, [Ars.MET;Ars.GUN]) in
    [ (P.MIL,mil); (P.REL,rel); (P.OPP,opp) ]
  end
(* ensemble des usus *)

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
  let fru = P.Record.fructus n.pr in
  let f k i p = k*i + p*(u-i) in
  let g i = f (n.k.sophia / 3.) i (P.sap fru * 0.40) in
  {
  seditio  = f (n.k.seditio) 0.90 (P.opp fru + squot 0. (P.alienatio atr) (P.humanitas atr) );
(*  fides    = f (n.k.fides)   0.90 (P.rel fru + P.servitium atr);*)
(*  fides    = f (n.k.fides)   0.90 (max 0. (P.rel fru )) ;*)
  fides    = f (n.k.fides)   0.90 ( (P.rel fru )) ;
  sophia   = g 0.99 + g 0.999 + g 0.9999 ;
  (* sophia tend très lentement vers sapientia * 1.20 *)
  ususList = UsusList.create (n.k.ususList, n.pp, atr, n.pArtes, n.cArtes);
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


