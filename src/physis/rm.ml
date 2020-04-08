(*

 ****************************** Regio_map.ml ******************************


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
open Std

module Ria = Rid.Array
module E = Espace
module ER= Espace.Regio
module ED= Espace.Direction
module F = Flumen

(******************************* MODULE LATITUDE *************************************)

module Latitude = struct

type latitude =
  | EQUAT
  | ARID
  | TEMPER
  | ARCTIC

type yve = {
  sud60 : int;
  sud30 : int;
  equat : int;
  nord30: int;
  nord60: int;
}
(* enregistrement des Variations en y *)

let nullYve() = {
  sud60 =0;
  sud30 =0;
  equat =0;
  nord30=0;
  nord60=0;
}
let cylLatMax = iof E.Cylinder.latMax

let tempConstantes =
  [EQUAT; ARID; TEMPER;    ARCTIC],
  [    0;   30;     60; cylLatMax],
  [EQUAT; ARID; TEMPER], (*reference lat list*)
  [   30;   30;     50]  (*stabilité list*)
(* constantes pour une génération de latitudes de température *)

let pluviaConstantes =
  [EQUAT; ARID; TEMPER;    ARCTIC],
  [    0;   28;     60; cylLatMax],
  [EQUAT; ARID; TEMPER], (*reference lat list*)
  [   50;   40;     55]  (*stabilité list*)
(* constantes pour une génération de latitudes de température *)

let hl = [ED.nord; ED.sud] (*hem list*)

let yve vm =
  {
  sud60 = - Tmap.nth vm ED.sud TEMPER ;
  sud30 = - Tmap.nth vm ED.sud ARID ;
  equat = ( (Tmap.nth vm ED.nord EQUAT)-(Tmap.nth vm ED.sud EQUAT) /2) ;
  nord30 = Tmap.nth vm ED.nord ARID ;
  nord60 = Tmap.nth vm ED.nord TEMPER;
  }

(* on récupère les valeurs pour calculer ensuite les intensités des latitudes climatiques frontières *)
let arrayCreate e (ll, dl, rl, sl) =
  let sInt   c = Random.int c - c/2 in
  let u = 10000 in
  let s,w,h = E.dimir e in
  let a    = Ria.make s 0 in
  let yvea = Array.make w (nullYve()) in
  let vm = Tmap.init (fun _h _l -> 0) hl rl in (*variation des lat, évoluera à chaque x*)
  let srd lat = List.assoc lat (List.combine ll dl) in (*standart relative degré*)
  let syd lat = (srd lat)*h/(cylLatMax*2) in (*standart y degré*)
  let asd vm hem lat = (* actual superior degré *) 
    let rl = (lat=ARCTIC => TEMPER) lat in (*reference lat*)
    ( syd lat + Tmap.nth vm hem rl ) in
  let asy vm hem lat = (* actual superior y (ie en partant de 0 au pole nord *)
    let latitude hem y = if hem = ED.sud then h/2+y else h/2-y in
    latitude hem (asd vm hem lat) in (*latitude of actual limit*)
  let equatLat vm = (asy vm ED.nord EQUAT + asy vm ED.sud EQUAT)/2 in
  let rvll = List.rev ll in
  let code vm y lat = 
    let equatLat = equatLat vm in
    let latMin, latMax = if y<equatLat 
                         then asy vm ED.nord lat, asy vm ED.nord (Tlist.following lat rvll)
                         else asy vm ED.sud  (Tlist.following lat rvll), asy vm ED.sud lat in
    let ryPos = u * (y-latMin) / (latMax-latMin) in
    let yPos = (y>=equatLat => ryPos) (u-ryPos) in
    let plsrd = (srd (Tlist.following lat rvll )) in (*previous lat srd*)
    yPos * (srd lat - plsrd) / u + plsrd in (* climatic lat en degrés *)
  let f x vm =
    let _ = Array.set yvea x (yve vm) in
    let g y =
      let rec f = function
      | [] -> 0 (*EQUAT*)
      | lat::_q when (y < asy vm ED.nord (Tlist.following lat rvll)
                  || y > asy vm ED.sud  (Tlist.following lat rvll)) -> code vm y lat 
      | _lat::q -> f q in
      Ria.set a (E.Cylinder.rid_of_ryx (E.resolution e) y x) (f (Tlist.remove rvll EQUAT)) in (* objet de la fonction *)
    let _ = Ext.iter h g in
    let module C=Compare in
    let alea _l s v iv sv = match C.situ (abs v) (w-x) with
    | C.Below -> let d=h*s/100 in (match C.bsitu (sInt (2*h)) (v-d) (v+d) with
      | C.Below when (iv-v)<(h/14) -> (-1)
      | C.Above when (v-sv)<(h/14) -> 1
(* NB:  quand il n’y a pas de lat voisine dont on doive veiller à ne pas s’approcher, iv (ou sv) renvoie v, donc la différence est nulle et donc inférieure à h/9 *)
      | _     -> 0)
    | _   -> (v>0 => -1) 1 in (*jointures au bord du cylindre*)
    let ss lat = List.assoc lat (List.combine rl sl) in 
    let iv h l = Tmap.nth vm h (Tlist.previous_or_first l rl) (* inferior variation *)
    and sv h l = Tmap.nth vm h (Tlist.following_or_last l rl) in (* superior variation *)
    let var h l e =
      if h=ED.sud && l=EQUAT then -(Tmap.nth vm ED.nord EQUAT)
(* Ainsi, les deux équateurs se suivent à la trace *)
      else e + alea l (ss l) (e) (iv h l) (sv h l) in
    Tmap.nMap (fun h l e -> var h l e) vm in

  let fIter n f vm = 
    let rec g i vm =
      if i=n then ()
      else g (i+1) (f i vm) in
    g 0 vm in
  let _ = fIter w f vm in a,yvea (*y var enregistrement list*)
end

(****************************************************************************************)

let chainesZoneCreate (e:E.t) (wma: bool Ria.t) (wca : bool Ria.t) (waa:int Ria.t) =
  (*working chaine array, working altitude array*)
  let res = E.resolution e in
  let legaldir(d) = if d<>4 then d else 0 in
  let rid = ref (Rid.oi 0) in
  let dir = ref 0 in
  let _ = for _chain=0 to 25
  do
  dir:=Random.int(4); (*4 voisins=>4 directions possibles*)
  rid:=E.Cylinder.randomRid ~polarExclusion:Continentes.polarExclusion res;
  while Ria.get waa (!rid)>=0 && Ria.get wca (!rid)=false && E.Cylinder.is_polar res (!rid)=false
  do
    if (Random.int(3)>0) then (Ria.set wma (!rid) true; Ria.set wca (!rid) true);
    rid:= List.nth (ER.lesQuatre e (!rid)) (legaldir(!dir+Random.int(2)));
    if (Random.int(20)=0) then dir:=legaldir(!dir+1)
  done
  done in wma, wca
  (*génère les futures lignes de crêtes dans un tableau (temporaire) de booléen*)
  (*sous-fonction de chain create*)


let chainsAgglo (e:E.t) (wca : bool Ria.t) (waa:int Ria.t) =
  let s = E.sir e in
  let caa (*copy altitude array*)= Ria.init s (function i -> Ria.get waa i) in
  let cca (*copy chaine array*) = Ria.init s (function i -> Ria.get wca i) in
  let alt = Ria.get caa in
  let dor = Ria.get cca in
  let chaine_among list = List.fold_left (fun r rid -> r || dor rid) false list in
  let _ = for i = 0 to (s-1)
  do
    let rid=Rid.oi i in
    let proximae = ER.lesQuatre e rid in
    let a = alt rid in
    (* création des tallus continentaux (eaux côtières) : *)
    (* par défaut l’altitude de l’océan est Continentes.ocean = -6 *)
    if a<0 then ( let h =min 0 (Continentes.highest_alt_aside alt (alt rid) proximae) in
		  if h > (alt rid +1) then Ria.set waa rid (h-1) )
    (* montagnes : *)
    else if dor rid && not(Continentes.lower_aside (alt) (alt rid) proximae)
    then (if a<3 || Random.int(5)<>0 then Ria.set waa rid (alt rid +1))
    (*montée systématique en altitude jusqu’à 2400m, ensuite 1 chance/2*)
    else if chaine_among proximae && Random.bool()
    then Ria.set wca rid true (*élargissement de la chaine*)
  done in wca, waa
  (*un passage pour agglomération autour des chaînes *)
  (*étend aléatoirement la largeur des chaines, et agglomère de l'altitude à l'intérieur*)
  (*sous-fonction de chain create*)


let chainsCreate (e:E.t) (iaa : int Ria.t) (*initial altitude array*) =
  let s,w,_h = E.dimir e in
  let ima (*initial mountain array*)= Ria.make s false in
  let ica (*initial chaine array*)= Ria.make s false in
  let cycle_nb = 
    let rec f(x)=
    if x<16 then 6
    else 1+f(x/2) in
    f(w) in
  let cycle (e, wma, wca, waa) =
    let nwma, nwca = chainesZoneCreate e wma wca waa in
    let swca, nwaa = chainsAgglo e nwca waa in
    e, nwma, swca, nwaa in
  let rec f = function
    0 -> (e, ima, ica, iaa)
  | i -> cycle(f(i-1)) in
  let _e, fma, _fca, faa = f(cycle_nb) in
  fma, faa
  (*ajoute des chaînes de montagne de largeur variée à l'altitude array*)
  (*Carte plus grande =>plus de cycle =>plus de chaînes, et plus de passages=chaines plus hautes *)


let lissage (e:E.t) (aa1:int Ria.t) =
  let f rid alt =
    let l = ER.lesQuatre e rid in
    if alt=0 && List.fold_left (fun s rid->s + iob(Ria.get aa1 rid <0)) 0 l >=3 
      then Ria.set aa1 rid (-1)
    else if List.fold_left (fun s rid->s && (Ria.get aa1 rid >alt)) true l 
      then Ria.set aa1 rid (alt+1) in
  Ria.nIter f aa1 
(* suppression des îles et presqu’îles d’une regio de large, et des mers d’une case *)
(* pour avoir des cotes moins découpées *)


type t = R.t Ria.t


let length (regio_map : t) = Ria.length regio_map

let nIter f rm = Ria.nIter f rm

let get rm (i:Rid.t) = Ria.get rm i

let altitude rm (i:Rid.t) = R.altitude(get rm i)
let alt rm (i:Rid.t) = R.alt(get rm i)
let hydros rm (i:Rid.t) = R.hydros(get rm i)
let thermos rm (i:Rid.t) = R.thermos(get rm i)

(*let is_passable rm (i:Rid.t) = R.is_passable (get rm i)*)
(*let climax rm (i:Rid.t) = R.climax(get rm i)*)
(*let tegmen rm (i:Rid.t) = Regio.tegmenFun (climax rm i) (Rv.Natura 80)*)
(*let is_farmable rm (i:Rid.t) = Regio.is_farmable (get rm i)*)
(* à déplacer dans la partie humaine *)
let physis rm (i:Rid.t) = R.physis(get rm i)

module L = Latitude

let pluviaArray e aa pla yvea =
  let es,ew,eh = E.dimir e in
  let pList = [ (-90); (-60); (-30);    0;   30;   60;   90] in (* parrallèles repères *)
  let pluvLA = (*pluvia list array*)
    let int hv = hv*100/(eh/6) in (*variation en % de la largeur stand. d’une zone climatique *)
    let hlist x = (*la hlist associée à pList varie pour chaque longitude *)
      let yve = yvea.(x) in
          [
          200;
          800 +  6 * int(yve.L.sud30 );
          120 -  7 * int(yve.L.equat - yve.L.sud60);
         2500 + 14 * int(yve.L.nord30 - yve.L.sud30);
          120 -  7 * int(yve.L.nord60 - yve.L.equat);
          800 +  6 * int(0 - yve.L.nord30);
          200;
          ] in
    (* l’extrema est augmenté par l’éloignement des latitudes frontières voisines *)
    Array.init ew hlist in
  let aList    = [    0;  500; 1500;  3000;  5000;  9000] in (* altitudes repères *)
(*  let pluvDList= [    0;  120;  240;   360;   480] in (* surplus de precipitations respectives sur un désert *)*)
  let pluvDList= [ 1200; 1000;  800;   600;   400;   400] in (* *limite de l’effet altitude à une altitude donnée *)
  let f rid = 
    let _lat,lon = E.Cylinder.yx_of_rrid (E.resolution e) rid in
    let latPluvia = Ext.swy pList (pluvLA.(lon)) (Ria.get pla rid) in
    let alt = R.altitudeFun (Ria.get aa rid) in
    let altPluvia = Ext.swy aList (pluvDList) (alt) in
    max 0 ( Ext.rangeMean latPluvia altPluvia 2600 (alt-400) ) in
  Ria.init es f

let thermosFun tLat altitude =
  let pList = [  0; 30; 60;  90] in (* parrallèles repères *)
  let tList = [ 30; 21;  2; -20] in (* températures respectives *)
  let latThermos = Ext.swy pList tList tLat in
  let altThermos = (max 0 altitude) /170 in (* la temperature diminue d'1°C pour 180m d'altitude (Andes), 170
  (Kilimanjaro), on met 170 pour avoir du -6°C (glacier) à 6000 m à l’équateur *)
  latThermos - altThermos

let create (e:E.t) =
  let s   = E.sir e in
  let aa0 = (Continentes.create e) in
  let fda, aa1 = (chainsCreate e aa0)  in
  let _   = lissage e aa1 in
(*  let aa1 = aa0  in*)
  let tla, _     = Latitude.arrayCreate e Latitude.tempConstantes in (*tLat array*)
  let pla, pyvea = Latitude.arrayCreate e Latitude.pluviaConstantes in (*pLat array*)
  let flumenList,aa2 = FlumenList.create e aa1 tla in
  let alt =Ria.get aa2 in (* fonction de lecture *)
  let ta = Ria.init s (fun i -> thermosFun (Ria.get tla i) (R.altitudeFun (alt i))) in (*thermos array*)
  let pa = pluviaArray e aa2 pla pyvea in (*pluvia array*)
  let thermos =Ria.get ta in(* fonction de lecture *)
  let pluvia  =Ria.get pa in(* fonction de lecture *)
  let mountain i = 
    let a = alt i in
    Ria.get fda i && thermos i >(-10) && (a>=5 || (a=4 && Random.int(3)=0) ) in 
    (* pas de montagne « sur » un inlandsis *)
  let flumenList = List.map (Flumen.fluxusUpdate (fun rid -> iof(ER.superficie e rid)) thermos pluvia) flumenList in
  let coast i = (alt i>=0) && List.exists (fun rid-> alt(rid)<0) (ER.lesQuatre e i) in
  let regio i = 
    let alt = alt i
    and t = thermos i
    and p = pluvia i in 
    let h = if alt<0 then (if R.is_seaIce ~alt ~t then R.SeaIce 
                           else if alt<(-2) then R.Ocean else R.Sea) 
            else (if R.is_glacier ~t then R.Inlandsis else R.Dry) in
    let area = ER.superficie e i in
    R.make i ~alt ~area ~h ~t ~p ~coast:(coast i) ~mountain:(mountain i) in
  let rm = Ria.init s regio in
  (* création de la rm par report des tableaux, mais sans les fleuves*)
  let _ =
    let g regio = 
      let rid=regio.F.rid in 
      Ria.set rm rid (R.riverUpdate (get rm rid) regio.F.fluxus (regio.F.direction) (regio.F.dest) ) in
    List.iter (Flumen.iter g) flumenList in
  (* écriture des fleuves sur la regio_map *)
  (rm)

(* EOF *)
