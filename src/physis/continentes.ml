(*

 ****************************** Continentes.ml ******************************


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

module Ria = Rid.Array
module E = Espace
module ER = Espace.Regio

type t = int Ria.t

let ocean = -6

let polarExclusion   = 0.00 (* si l’on souhaite n’avoir aucune terre en zone arctique *)
let polarRestriction = 0.10 (* si l’on souhaite que les grandes masses continentales évitent les zones arctiques ( cartes en cylindre plus esthétiques ) *)

let lower_aside alt a l = List.fold_left (fun v j->v || (alt j<a)) false l
(* true si un des voisins est plus bas *)


let higher_aside alt a l = List.fold_left (fun v j->v || (alt j>a)) false l 
(* true si un des voisins est plus haut *)

let highest_alt_aside alt a l = List.fold_left (fun h j-> ((alt j<h) => h) (alt j) ) a l 
(* altitude du voisin le plus haut, notre regio comprise *)

let continentalite alt l =
  let f s rid =
    let a = alt rid in
    if a>0 then 4 (*la projection du relief fait apparaître ce cas-ci *)
    else if a=0 then s+1
    else s in
  List.fold_left f 0 l
(* continentalité d’une regio (nb de terre autour, le relief compte pour 4) *)


let add_chains (e:E.t) (waa:int Ria.t) = (*working altitude array*)
  let s,w,h = E.dimir e in
  let res = E.resolution e in
  let wda = Ria.make s false in (* working dorsale array *)
  let legaldir(d) = d mod 4 in
  let rid = ref (Rid.none) in
  let dir = ref 0 in
  let alt rid = Ria.get waa (!rid) in
  let _ = for chain=0 to (w/4) (*plutot fn de w que de s : il s’agit plus de créer des Am du sud que des caps*)
    do
    dir:=Random.int(4); (*4 voisins=>4 directions possibles*)
    rid:=E.Cylinder.randomRid ~polarExclusion res ; (*il faut une sélection initiale *)
    while (alt rid=ocean && Random.int(50)<>0) (*98% de chaines parties des terres, 2% de nouvelles «Crête» *)
      do rid:=E.Cylinder.randomRid ~polarExclusion res done; 
    while (alt rid<>ocean || Random.int(6)>0) (*on s’arrête au bout d’environ 6 cases si on s’avance dans la mer*)
    && Ria.get wda (!rid)=false && E.Cylinder.is_polar res (!rid)=false
      do
      (if (Random.int(3)>0) && alt rid=ocean (*chaîne non continue*)
      then Ria.set waa (!rid) 1; Ria.set wda (!rid) true); (* 400 m, ça marche actuellement )*)
      rid:= List.nth (ER.lesQuatre e (!rid)) (legaldir(!dir+Random.int(2)));
      if (Random.int(8)=0) then dir:=legaldir(!dir+1)
      done
    done in waa
(*génère des chaines d’altitude 1 (400m) et d’une case de large sur le waa *)
(*la fonction est lancée à chaque projection. *)
(*L’objectif est de dés-arrondir les bords des continents qui ont tendance à ressembler à des boudins *)



let prim_agglo_c =
[|  1; 
    2; (* on limite l’agglo à ce moment-là pour ne pas avoir des continents en forme de disque *)
    2;
    1; (* au delà, on agglomère pour éviter les détritus de terres juxtaposées *)
    1;
    1;
    1; 
  256|] (* on conserve quelques mers intérieures *)
(* chances d’émerger selon le nb de voisins terrestres (de 1 à 8) : une chance sur X *)
(* les chances sont nulles pour 0 voisins *)
(* tableau optimum pour une carte MICRO (45*20) =>génère des continents massifs, mais non des disques *)


let prim_agglo (e:E.t) = 
  let s,w,h = E.dimir e in
  let proba = prim_agglo_c in
  let wa = Ria.make s (ocean) in (*working array*)
  let alt = Ria.get wa in (*on lit l’array sur lequel on travaille*)
  let tnb = ref 0 (*terres nb*) in
(*  let max = s*10/100 in (*on monte à 35% avec les chaines et les agglos des projections*)*)
  let max = s*18/100 in (*pour le développement*)
  let rrid() = 
    let open Tfloat in
(*    let bmax = h/6 in (*bord maximum, en % de la hauteur de la carte*)*)
    E.Cylinder.randomRid ~polarExclusion:(polarRestriction * (u - foi(!tnb) / (foi max))) (E.resolution e) in (* (bmax - (!tnb*bmax/max)) in*)
  let continents = 16 in (*le nb de terres émergées possiblement sans voisins terrestres*)
  let emerge rid = Ria.set wa rid 0 ; tnb := !tnb+1 in (*une procédure (!)*)
  let _ = while !tnb < max do
    let rid = rrid() in
    if !tnb mod (max/continents) = 0 then emerge rid
    else if alt rid < 0 then
    let continentalite = min 8 (continentalite alt (ER.lesHuit e rid)) in
    if continentalite > 0 && Random.int(proba.(continentalite-1)) = 0 then emerge rid;
    done in wa
(* creation d’une carte de continents selon les dimensions de e (32*18 -> 64*36) *)
(* méthode classique : on ajoute des terres en tirant des positions au hasard *) 
(* subtilité : le hasard fait bien les choses : pas de continents collés aux bords N/S du cylindre *)


let cyclic_agglo_c (s) =
 [|200000/s; (*la prob de naissance d'île décroît pptnlmt à la surface de la carte afin d'avoir en nombre égal des îles de toutes les tailles*)
   100000;
   200000;
   1000000; (*100% si 3 ou 4 voisins pour limiter les lacs*)
   1000000|],
  200000000/s, (*la prob de naissance de relief décroît pptnlmt à la surface de la carte*)
  1000000 (*dénominateur*)
(*constantes des passages*)

let passage e alt wa =
  let s,w,h = E.dimir e in
  let proba, alt_proba, denom = cyclic_agglo_c s in
  for i = 2*w to (s-1-2*w)
  do
    let i = Rid.oi i in
    let a = alt i in
    let l = ER.lesQuatre e i in
    if a < 0 
    then ( if Random.int(denom) < proba.(min 4 (continentalite alt l))
           then Ria.set wa i 0 ) 
    else if a<2
         && not (lower_aside alt a l)
         && ( ( higher_aside alt a l && Random.int(3)>0  )
              || Random.int(denom) < (alt_proba*(a+1))  )
         then Ria.set wa i (a+1)
  done
(* on regarde toutes les regiones du tableau une fois, pour les élever peut-être *)


let agglomeration (e:E.t) (wa:t) =
  let ca = Ria.make (E.sir e) (ocean) in  (*copy array*) 
  let _ = for agglo_i = 1 to 8 
  do
    let _ = Ria.blit wa ca in
    let alt = Ria.get ca in
    passage e alt wa 
  done in wa
(*crée des terres et du relief sur une carte par agglomeration*)
(*on distingue le tableau de lecture du tableau de travail *)


let projection (e:E.t) (ne:E.t) (a:t) =
  let f rid = let lat,lon = E.Cylinder.yx_of_rrid (E.resolution ne) rid in
              if (lat mod 2)=0 && (lon mod 2)=0 
              then Ria.get a (E.Cylinder.rid_of_ryx (E.resolution e) (lat/2) (lon/2))
              else (ocean) in
  Ria.init (E.sir ne) (f)
(* projection d’un array dans un espace plus vaste*)
(* on attribue la valeur à une case sur 4 (la case NW) *)


let cycle (oe,oa) =
  let ne = E.dilate oe (E.Cylinder E.Quadral) in
  let na = projection oe ne oa in
  let na = add_chains ne na in
  ne, agglomeration ne na 
(*projection, puis agglomération*)


let create (e:E.t) =
  let bs = E.Agglo in
  let projNb = E.projNb bs (E.resolution e) in
  let pe = E.create (E.Cylinder E.Octal) bs in (*prim espace*)
  (* la prime génération (des continents) nécessite huit voisins *)
  let ba = prim_agglo pe in (*basic array*)
  let be = E.create (E.Cylinder E.Quadral) bs in (*basic espace*)
  (* les agglomérations ultérieures ne requièrent que 4 voisins *)
  snd (Ext.applique cycle (be,ba) projNb)
 
