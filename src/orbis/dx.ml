(*

 ****************************** Dx.ml ******************************


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

let fertiList allen =
  let list =          [ (12,0.05); (13,0.15); (14,0.30); (15,0.50); (16,0.66); (17,0.74); (18,0.78); (19,0.80); 
  (20,0.80); (21,0.80); (22,0.80); (23,0.80); (24,0.80); (25,0.80); (26,0.80); (27,0.80); (28,0.80); (29,0.79); 
  (30,0.78); (31,0.76); (32,0.74); (33,0.71); (34,0.68); (35,0.65); (36,0.62); (37,0.58); (38,0.54); (39,0.50); 
  (40,0.45); (41,0.40); (42,0.35); (43,0.30); (44,0.25); (45,0.20); (46,0.16); (47,0.12); (48,0.09); (49,0.06); 
  ] in
  let corr f = u / (( u/f + allen/12. ) ) in 
  List.map (fun (a,f) -> (a,corr f) ) list 
(* taux de fécondité maximum (en proportion de chaque classe d’âge) *)
 
let ferti36 = fertiList 36.
(* taux de fécondité maximum par classe d’age pour une durée d’allaitement de 36 mois *)

(*************************** quelques constantes **************************************)

let ndc age = 
  let taux ageMin coef = (max 0. (foi age - foi ageMin)) * coef in
  taux 50 0.001 + taux 60 0.002 + taux 70 0.004
(*(max 0 (a-50))*10 + (max 0 (a-60))*20 + (max 0 (a-70))*40*)
(*natural death coef*)

let ddc a h = 0.05 * foi(1 lsl (3 -- min 3 a)) * (max 0.01 (1. - h/2.)) 
(*let ddc a h = 0.07 * foi(1 lsl (3 -- min 3 a)) * (max 0.01 (1. - h/2.)) *)
(*disease death coef*)
(*mortalité minimum ssi h>=2u*)

let copiaFun ar (*alimonium_ratio*) plebs cibus = cibus / ( plebs * ar )
(* abondance : rapport entre la nourriture dispo et les besoins alim *)

let fdc copia =
  let sa = if copia>u then 0. else u - copia (* sous-alimentation *) in
  4.406 * exp ( 3.323 * log sa ) (* équivalent *)
(*  (10. ** ( (log2 (sa * 100.)) - 6.))*)
(*famine death coef == effet d’une copia < 1. == effet de la sous alimentation *)

let mdc wam a = if (a>=15 && a<55) then (wam*0.01) else 0. (*1% des militaires at war chaque année*)
(*militaria death coef*)

(*let odc lostRatio = lostRatio*0.02 (*2% de la pop des terr occupés at war*)*)
(*occupation death coef*)

let survie_coef fdc mdc a h = (u-ndc a) * (u-ddc a h) * (u-fdc) * (u-mdc a) (** (u-odc)*)

let facultas_pyramid = 
  let f g =
  if g<5 then 0.
  else if g<15 then (foi g - 5.)*0.2
  else if g<60 then 2.
  else if g<70 then (70. -foi g)*0.2
  else 0. in
  Tlist.init 100 (fun n-> (n, f n)) 
(* taux de capacité productive de chaque génération *)
(* = 200% entre 15 et 60 ans *)
(* =   0% en dessous 5 ans et au delà 70 ans *)
(* ça donne un taux un taux moyen d’env 125% pour une pyramide pointue *)

let alimonium_pyramid =
  let a g =
  if g<10 then 0.2 + 0.8 * (foi g / 10.)
  else 1. in
  Tlist.init 100 (fun n-> (n, a n))
(* besoins nutritionnels de chaque génération *)
(* 100% après 10 ans, moins avant *)

(*********************************** MODULE PYRAMID *********************************************************)

  module Pyramid = struct

type t = (int*float*float) list

let sum pyramid = 
  let f = fun s (_,n,_) -> s+n in
  List.fold_left f 0. pyramid
(* nombre de personnes dans la pyramide l *)

let n_sum l min max =
  let rec f = function
  | [] -> 0.
  | (i,e,_)::q when i>=min && i<=max -> e + f q
  | t::q -> f q in
  f l
(* nombre de personnes de min à max compris ans dans la pyramide l *)

let facultas_ratio pyramid plebs =
  let f (_,n,_) (_,r) = n*r in
  squot 0. (Tlist.fsum (List.map2 f pyramid facultas_pyramid)) (plebs)
(* taux de capacité productive de la natio *)
(* varie entre 100 et 180% selon la forme de la pyramide (les adultes produisent 200%) *)

let alimonium_ratio pyramid plebs =
  let f (_,n,_) (_,r) = n*r in
  squot 0. (Tlist.fsum (List.map2 f pyramid alimonium_pyramid)) (plebs)
(* taux de besoins alimentaires *)
(* varie entre 70% et 98% selon la forme de la pyramide (les adultes consomment 100%) *)

let create n(*naissances*) (he:int->float) (fdc) =
  let mdc = fun a->0. in
  let rec f n h (*nb and health*) = function
  | 99 ->[]
  | a -> let sc = survie_coef fdc mdc a h in
	 let na = a++1 in
	 let nn = n*sc in
         let nh = he na in
         (na, nn, nh) :: f nn nh na  in
  let h = he 0 in
  (0, n, h) :: f n h 0
(* pyramide théorique pour une génération 0 et une health par âge donnée *)

let viellie pyramid sc s =  
  let next_health s h a = (h*(foi(a++1)) + s)/(foi(a++2)) in
  (* health de la génération d’age a et de health h un an plus tard, avec une natio ayant une sophia de s*)
  (* sophia impacte bcp plus l’health des nouveaux nés etc. *)
  let f (a,n,h) = (a++1, n*(sc a h), next_health s h a) in
  Tlist.nRemove (List.map f pyramid) 99
(*viellie pyramide*)

let homothetie r pyramid = List.map (fun (a,n,h) -> (a,n*r,h)) pyramid
(* renvoie une homothetie de raison r de la pyramide *)

let vigesimal pyramid =
  let f i =
    let min = mult i 5 in
    let max = min++4 in
    [min;max], n_sum pyramid min max in
  Tlist.init 20 f 
(* pyramide des âges standart : par tranches de 5 annés *)

let nbmList pyramid =
  let rec g fel pyr = match fel, pyr with
  | [], pyr -> [] (* cas terminal *)
  | (fa,f)::fq, (pa,n,h)::pq when pa< fa -> g fel pq (* cas premier *)
  | (fa,f)::fq, (pa,n,h)::pq when pa==fa -> (fa,n*f,h) :: g fq pq
  | _ -> print_endline "Dx.Pyramid.nbm : input non viable"; [] in
  g ferti36 pyramid 
(* liste des (par âge mère, nb max de naissances, h) *)

let descendanceFinale pyramid naissances nbm =
  let rec g v fel pyr = match fel, pyr with
  | [], pyr -> (0., 0.) (* cas terminal *)
  | (fa,f)::fq, (pa,n,h)::pq when pa==(fa--1)-> g n fel pq (* cas second *)
  | (fa,f)::fq, (pa,n,h)::pq when pa< fa     -> g u fel pq (* cas premier *)
(*  | (fa,f)::fq, (pa,n,h)::pq when pa==fa     -> f*(n/v) + g v fq pq*)
  | (fa,f)::fq, (pa,n,h)::pq when pa==fa     -> Couple.sum (f,f*n/v) (g v fq pq)
  | _ -> print_endline "Dx.Pyramid.descendanceFinale : input non viable"; (0., 0.) in
  let isf_nbm, dfn_nbm = g u ferti36 pyramid in (* isf et dfn max calculés sur la ferti36 *)
  let rnnbm = naissances / nbm in
  isf_nbm * rnnbm, dfn_nbm * rnnbm
(* indice synth. de fécondité = Somme des taux de fecond par gen *)
(* descendance finale nette = Somme des (taux de fecond par gen * taux de survie par gen // gen 11 ans) *)

end

(********************************************************************************************)

type t = {
  pyramid : Pyramid.t;  
  copia   : float;
  tfg     : float; (*taux de fécondité général*)
  isf     : float*float; (*isf, descendance finale nette*)
  sum     : float; 
  var     : float; (*coef de var de sum entre n-1 et n *)
}

let pyramid d = d.pyramid
let copia   d = d.copia
let tfg     d = d.tfg
let isf     d = d.isf
let sum     d = d.sum
let var     d = d.var
let famine  d = fdc d.copia

let naissances_objectif s f facultas pna nbm =
  let stpyr = Pyramid.create pna (fun i->s) (0.) in (*sophia theoric pyramid*)
  let stp = Pyramid.sum stpyr in (*plebs*)
  let ar  = Pyramid.alimonium_ratio stpyr stp in
  let maxp= facultas / ar in
  let nr  = (maxp / stp) ** 0.5 in (*naissances ratio pour une pyramid max*)
  let a = pna * nr in (*naissances pour une pyramide max*)
  let ndo,nio = a*0.95, a*1.20 in (*naissances democracy objective, imperium objective*)
  let ck = s+f in (*civ kapital = sophia + fides*)
  let ccm= 4.*u in (* civ coef max *)
  let cc = min ccm ck in (*civ coef*)
  let pc = ccm-cc in (*primitivité coef*)
  let dc = squot 0. (s * cc) ck (*democracy coef*)
  and ic = squot 0. (f * cc) ck in (*imperium coef*)
  nbm*pc + ndo*dc + nio*ic
(*naissances objectif*)

let naissances pyramid sc s f facultas wam =
  let ffn = (Pyramid.n_sum pyramid 13 50)/2. in (*femmes en âge de*)
  let pna = match (List.hd pyramid) with (_,n,_) -> n in (*previous naissances*)
  let nbmList = Pyramid.nbmList pyramid in
  let nbm = Pyramid.sum (nbmList) in
  let nao = naissances_objectif s f facultas pna nbm in 
(*  let paxN = min nbm ((pna + nao)/2) * (1000+Random.sign()*Random.int(10)) / 1000 in (*pax naissances*)*)
  let paxN = min nbm ((pna + nao)/2.) * (1. + Random.sFloat 0.01) in (*pax naissances*)
  let naissances = paxN * (u-wam) in (*les militaires au front sont autant de naissances perdues*)
(*  let naissances = pna * 1.02 in (*FIXME*)*)
(*  let naissances = nbm in (*FIXME*)*)
  let tfg = squot 0. naissances ffn in (*taux de fécondité générale*)
  let isf  = Pyramid.descendanceFinale pyramid nbm naissances in
  naissances, tfg, isf
(*naissances*)

let update d(*demographics*) plebs =
  let r   = plebs / (Pyramid.sum d.pyramid) in
  {
  pyramid = Pyramid.homothetie r d.pyramid;
  copia   = d.copia;
  tfg     = d.tfg;
  isf     = d.isf;
  sum     = plebs;
  var     = d.var;
  }
(* mise à jour démographie en fonction des gains et pertes territoriaux *)
(* ajustement linéaire de la pyramid de façon à ce que la somme des générations égale la somme des plèbes régionales *)


let preview d(*demographics*) cibus(*effective lab*) s(*sophia*) f(*fides*) facultas m(*militaria*) warRatio (*lostRatio*) =
  let wam = m*warRatio in (*militaria at war*)
  let ar  = Pyramid.alimonium_ratio d.pyramid (d.sum) in
  let copia = copiaFun ar (d.sum) cibus in
  let fdc = fdc copia
  and mdc = mdc wam in
  let sc  = survie_coef fdc mdc in
  let v_pyramid = Pyramid.viellie d.pyramid sc s in 
  let naissances, tfg, isf = naissances d.pyramid sc s f facultas wam in 
  let n_pyramid = (0,naissances,s) :: v_pyramid in
  let sum = Pyramid.sum n_pyramid in
  {
  pyramid = n_pyramid;
  copia = copia;
  tfg = tfg;
  isf = isf;
  sum = sum;
  var = sum / d.sum;
  }
(* démographie projetée pour le tour (n+1) en fonction des kapitaux en (n) et de partitio de la période (n-1 .. n) *)

let null=
  {
  pyramid = Pyramid.create 0. (fun x->0.) 0.;
  copia   = 1.;
  tfg     = 0.;
  isf     = 0.,0.;
  sum     = 0.;
  var     = 1.;
  }

let create sophia plebs = 
  let proportions = Pyramid.create 100. (fun i->sophia) (0.) in
  let denominateur = Pyramid.sum proportions in
  let pyramid = List.map (fun (a,p,h)->(a,plebs*p/denominateur,h)) proportions in 
  { 
  null with 
  pyramid = pyramid;
  sum=Pyramid.sum pyramid
  }
(* initialisation de dx (création d’une pyramide) pour une plèbe et une sophia données *)


let facultas_ratio  dx = Pyramid.facultas_ratio  dx.pyramid dx.sum
let alimonium_ratio dx = Pyramid.alimonium_ratio dx.pyramid dx.sum
