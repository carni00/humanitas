(*

 ****************************** ImperiiMapDraft.ml ******************************


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

(** fonctions permettant de déterminer le nombre, la localisation, l'étendue des civilisations initiales ; fonctions appelées par Orbis.create uniquement *)

open Std

module Ria = Rid.Array
module Nia = Nid.Nia
module Nid_list = Nid.List
module E  = Espace
module ER = Espace.Regio
module D  = Date


type rid = Rid.t
type origo = Rid.t * Date.t (*lieu, date*)
(* origine d'une civ = lieu et date developpement agriculture *)

type t = {
  civMap        : (Nid.t*Date.t) Rid.Array.t; (*civ × date d'adhésion de la regio à la civ*)
  natioIdList   : Nid.t list;
  civCentersMap : Nid.t Rid.Array.t;
  civOrigoArray : origo Nid.Nia.t
  }

let agri = D.agriculture
let begn = D.beginning (* of the game *)
let neol = D.distance agri begn  (*neolithic length*)
(* le neolithic est le prologue de la partie durant laquelle naissent et commencent à s’étendre les civilisations agricoles *)
(* la partie commence avec la metallurgy *)


let reachable_terrae_map e rm =
  let s  = E.sir e in
  let wa = Ria.init s (fun i-> Rm.alt rm i >= 0) in (* toute terre est reachable a priori *)
  let ca = Ria.make s false in
  let proba, denom = [| 0; 9; 10; 10; 10 |],10 in
  let agglo i =
    let _ = Ria.blit wa ca in
    let proba l = proba.(Tlist.census (Ria.get ca) l) in
    let f id r =
      if r = false && Random.int(denom) < proba(ER.lesQuatre e id)
      then Ria.set wa id true in (* on élargit aux cases voisines = océans proches *)
    Ria.nIter f ca in
  let _ = Ext.iter ((E.wir e)/16) agglo in wa
(* définit les zones que l'humanité peut parcourir avant Date.beginning, soit les terres et océans jusqu'à une certaine distance des terres *)


let reached_terrae_map e retm rm =
  let s,w,h = E.dimir e in
  let wa = Ria.make s false in
  let rid =
    let rec f c =
      let rid = E.Cylinder.randomRid ~polarExclusion:0.25 (E.resolution e) in
      let r = Rm.get rm rid in
      if (R.alt r>=0 && R.thermos r>20 && R.pluvia r>500 && R.pluvia r<1000 )
      || (c>=8192 && R.alt r>=0)
      then rid
      else f (c+1) in f 0 in
  let _ = Ria.set wa rid true in (*lieu de naissance de l'humanité *)
  let is_reached wa l = List.fold_left (fun s i-> s || Ria.get wa i) false l in
  let dirArray = [| (0,h-1,0,w-1,1,1); (0,h-1,w-1,0,1,(-1)); (h-1,0,w-1,0,(-1),(-1)); (h-1,0,0,w-1,(-1),1) |] (*4 sens possible de balayge de la carte*) in
  let balayage i =
    let y_min, y_max, x_min, x_max, y_inc, x_inc = dirArray.(i mod 4) in
    let y = ref y_min
    and x = ref x_min in
    while !y<>y_max
    do
    while !x<>x_max
    do
    let rid = Rid.oi( (!y)*w + (!x)) in
    if (Ria.get retm rid) (* if reachable *)
    && (is_reached wa (ER.lesQuatre e rid)) (*et reached aside*)
    then Ria.set wa rid true; (*terre parcourue par l'humanité avant Date.beginning*)
    x:=!x+x_inc
    done;
    y:=!y+y_inc;
    x:=x_min
    done in
  let _ = Ext.iter 16 balayage in wa
(* définit les zones que l'humanité a parcouru avant Date.beginning, soit les terres pouvant être parcourues, à partir du lieu de naissance de l'espece homo sapiens *)


let civilization_centers e (rdtm: bool Ria.t) (rm) =
  let s,w,h = E.dimir e in
  let res = E.resolution e in
  let ccm (*civ centers map*)  = Ria.make (s) (Nid.none) in
  let zrm (*zone réservée map*)= Ria.make (s) (false) in
  let coa (*civ origo array*)  = Nia.make (Rid.none,Date.Unknown) (*lieu,date*) in
(*  let proximaeFarmable l = Tlist.census (fun rid -> Regio.is_farmable (Rm.get rm rid) ) l in*)
  let r = 1+w/100 (*66*) in (* rayon, arrondi au dessus *)
  let d = 2*r+1  in (* diamètre du disque, y compris case URBS *)
  let er= 1+w/18 in (* rayon du grand disque externe *)
  let disque_surface = d*d/2 in (* le "losange" carré dans le disque en réalité *)

  let reservation crid =
    let cy,cx = E.Cylinder.yx_of_rrid res crid in
    for i=(-er) to er do
    for j=(-er) to er do 
      Ria.set zrm (E.Cylinder.srid_of_ryx res (cy+j) (cx+i)) true
    done; done in

  let disque_potentiel (ccm) (crid) (*central regio id*) =
    let cy,cx = E.Cylinder.yx_of_rrid (res) crid in
    let rec boucle p i j =
      let j,i = if i<r then j,(i+1) else (j+1),(-r) in (*déplacement du curseur*)
      if j > r then (iof p) / disque_surface (*sortie du cadre après complet examen*)
      else if abs i + abs j > r then boucle p i j (*si pas dans le disque*)
      else let rid = E.Cylinder.srid_of_ryx (res) (cy+j) (cx+i) in
           let regio = Rm.get rm rid in
           if Rv.is_farmable regio = false then boucle p i j
           else boucle (p +. R.hospitalitas regio) i j in
    boucle 0. (-r) (-r) in

  let iMax = s in
  let rec createNations i bnl vnl =
    if i>iMax then bnl,vnl
    else let rid   = E.Cylinder.randomRid ~polarExclusion:0.15 (res) in
         let regio = Rm.get rm rid in
         if R.alt regio < 0 (*test efficace*)
         || Ria.get zrm rid = true
         || Rv.is_farmable regio = false 
         then createNations (i+1) bnl vnl
         else let dp = disque_potentiel (ccm) (rid) in
              let dpMin = 10 + 25 * (iMax-i) / iMax  in
              if dp < dpMin then createNations (i+1) bnl vnl
              else let nid = Nid_list.random vnl in (*une nation au pif*)
                   let _ = Ria.set ccm rid nid ;
                   reservation rid ;
                   Nia.set coa nid (rid,Date.make((Date.to_int begn)-(dp-8)*120)) in (*origo fonction de la fertilité*)
                   if List.length vnl = 1 
                   then (nid::bnl), (Tlist.remove vnl nid)
                   else createNations (i+1) (nid::bnl) (Tlist.remove vnl nid) in
  let bnl,vnl = createNations 0 [] Nid_list.make in
  ccm, coa (*civ origo array*), bnl
(* crée les centres de civ = 1 case initiale par nation *)


let civ_map e (ccm:Nid.t Ria.t) (coa:origo Nia.t) rm =
  let s,w,h = E.dimir e in
  let origoDate civ = snd(Nia.get coa civ)  in
  let f i = let civ=Ria.get ccm i in (civ, if civ=Nid.none then Date.Unknown else origoDate civ) in
  let wa (*work array*) = Ria.init s f in
  let ca (*copy array*) = Ria.make s (Nid.none, Date.Unknown) in
  let rec proximae_terrae_natioList year = function (*liste des éventuelles nations voisines*)
    []   -> []
  | t::q -> let natio = fst(Ria.get ca t) in 
            if natio<>Nid.none && D.precedes (Date.add (origoDate natio) (-500)) year
            (* les nationes ont une taille minimum « équivalente » à 500 ans de progression de l’agriculture*)
	    then natio :: proximae_terrae_natioList year q
            else proximae_terrae_natioList year q in
  let pnb (*passages nb*) = w/10 in
  let _ = for agglo_i = 1 to pnb
  do
    let _ = Ria.blit wa ca in
    for i = 0 to (s-1)
    do
      let rid = Rid.oi i in
      let r = Rm.get rm rid in
      if fst(Ria.get ca rid) = Nid.none
      && R.alt r>=0
      && Rv.is_farmable r (*|| (List.exists (function i -> (Regio.is_farmable r ) ) (ER.lesHuit e rid))*)
      && (Random.int(15) < iof(R.hospitalitas r) -16 ) 
      then let year = Date.add agri (neol*agglo_i/pnb) in
        let natioList = proximae_terrae_natioList year (ER.lesQuatre e rid)  in
        let len = Tlist.len natioList in
        if  len > 0
        then let civ = List.nth natioList (Random.int(len)) in
             Ria.set wa rid (civ, max year (origoDate civ) )
             (* comme on crée une taille minimum « équivalente » à 1000 ans de progression de l’agriculture, year
             peut être antérieure de 1000 ans à l’origo déterminée de la civ, on ramène donc cette valeur au plus
             tôt à la date de naissance de la civ, alors commune au centre géographique de la civ *)
    done
  done in wa
(* agglomère un territoire national autour du centre de civ *)
(* génère un (Nid.t*Date.t) Rid.Array.t *)
(* ancienneté de colonisation agricole de la regio, en année commune (entre Date.agri et Date.begin) *)


let create e rm =
  let retm = (reachable_terrae_map e rm) in
  let rdtm = (reached_terrae_map e retm rm) in
  let civCentersMap, civOrigoArray, natioIdList = civilization_centers e rdtm rm in
  let civMap = civ_map e civCentersMap civOrigoArray rm in
(*  let a = List.hd natioIdList in*)
  {
  civMap;
  natioIdList;
  civCentersMap;
  civOrigoArray
  }
(*on renvoie ccm pour définir l'emplacement des capitales des nations*)

