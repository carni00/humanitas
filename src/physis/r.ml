(*

 ****************************** Regio.ml ******************************


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

type climat =
| Arid
| Semiarid
| Tropical
| Equatorial
| Temperate
| Subarctic
| Arctic
| Alpine
| Subalpine

type forest=
| Taiga
| TropicalF
| RainF
| Deciduous
| Coniferous
| OtherF

type grassland=
| Savanna
| OtherG

type tundra=
| ArcticT
| AlpineT

type climax =
| Desert
| Steppe
| Tundra    of tundra
| Grassland of grassland
| Woodland  of forest
| Forest    of forest
(* végétation naturelle, aucune anthropisation *)

type river = {
  dir    : Espace.direction;
  dest   : Rid.t;
  fluxus : int;
  }

type hydros =
| Inlandsis
| SeaIce
| Ocean
| Sea
| Lake
| River of river
| Dry


type t = {
  rid  : Rid.t;
  alt  : int; (* en unité d’altitude (0 à altMax) *)
  area : float;
  hydros : hydros;
  thermos: int; (* en °C (-20 à +30) *)
  pluvia : int; (* en mm/an *)
  coast  : bool;
  mountain : bool;
  }

let make rid ~alt ~area ~h ~t ~p ~coast ~mountain = {
  rid  = rid;
  alt  = alt;
  area = area;
  hydros = h;
  thermos = t;
  pluvia = p;
  coast= coast;
  mountain= mountain;
  }

let riverUpdate regio fluxus dir dest = {
  regio with
  mountain = false;
  hydros = River {
           fluxus = fluxus;
           dir    = dir;
           dest   = dest;
           }
  }
(* pour mise à jour de la regio map après la création de la liste des fleuves*)

let none = {
  rid  = Rid.none;
  alt  = 0;
  area  = 0.;
  thermos =0;
  pluvia  =0;
  hydros = Dry;
  coast=false;
  mountain=false;
  }

(*****************************************************************************************************)

let altMax  = 6 

let altitudeFun = function
  | a when a<0 -> 0 (*niveau de la mer*)
  | 0 ->    1 (*code pour l’altitude des plaines*)
  | 1 ->  400
  | 2 -> 1000
  | 3 -> 1600
  | 4 -> 2400
  | 5 -> 3200
  | _ -> 4000
(* attribue à une alt (ie au code d’altitude) l’altitude en mètres *)

let river = function
  | River river -> river.fluxus
  | _           -> 0

let hugrosFun ~h ~p =  p + match h with
  | River river -> 46*(max 0 (Ext.log2 river.fluxus -4))
  | _           -> 0
(* associe à pluvia et river une humidité en équivalent-mm de précipitations *)

let inlandsisTemp = (-10)

let is_glacier ~alt ~t ~p = t<= inlandsisTemp
(* température faible et pluvia > fonte *)

let is_seaIce ~alt ~t = alt<0 && t<=(-15)
(* banquise *)

let ariditas ~t ~p = p / (max 1 (t + 10))
(*indice d’aridité de De Martonne*)
(* <5  : atacama
   <10 : sahara, thar
   <20 : steppe (kalahari, chaco)
   <30 : prairie/tundra
   <40 : forêt claire *)

let physis ~alt ~h ~t ~p =
  let hugros = hugrosFun ~h ~p in
  if alt<0 || is_glacier ~alt ~t ~p then 0
  else
  let sMax = if t < (-5) then (20+t)*2
             else             (35+t) in
  cut 0 sMax (ariditas t hugros)
(*aptitude de la regio au développement de la végétation naturelle*)
(*indice calqué sur l’indice d’aridité, mais en y ajoutant l'effet négatif du froid extrême *)

let clim ~altitude ~t ~a = 
         if a< 10  then Arid
    else if t<(-5) then (if altitude<2000 then Arctic else Alpine) 
    else if a< 20  then Semiarid
    else if t<  5  then (if altitude<1000 then Subarctic else Subalpine) 
    else if t< 21  then Temperate
    else if a< 40  then Tropical
    else Equatorial
(* fonction commune à climat et climax *)

let climat ~altitude ~t ~p = clim ~altitude ~t ~a:(ariditas ~t ~p)
(* climat *)

let climax ~altitude ~h ~t ~p = match h with
  | Ocean | Sea | SeaIce | Inlandsis -> Desert 
  | _ -> let a = ariditas ~t ~p:(hugrosFun ~h ~p) in
         match clim ~altitude ~t ~a with
         | Arid       -> Desert
         | Arctic     -> Tundra ArcticT
         | Alpine     -> Tundra AlpineT
         | Semiarid   -> Steppe
         | Subarctic  -> Woodland Taiga
         | Subalpine  -> Woodland Coniferous
         | Temperate  -> if a<30 then Grassland OtherG else if a<40 then Woodland Deciduous else Forest Deciduous
         | Tropical   -> if a<30 then Grassland Savanna else Woodland TropicalF
         | Equatorial -> Forest RainF
(* couvert végétal en l’absence d’anthropisation *)
(* similaire à climat, sauf qu'on calcule l’aridité à partir non pas de pluvia, mais d’hugros = on tient compte des
fleuves *)

let hospitalitasFun t hydros physis = 
  let r = 8*(max 0 (Ext.log2 (river hydros) -4)) in (*apport des fleuves : limon et moyen de transport*)
  let a = min physis 40 + min r 60 in
  let b = if t<10 then a*t/10 else if t>23 then a*(33-t)/10 else a in (*handicap des températures <10 et >23 *)
  foi (cut 0 100 b)
(* hospitalité de la regio = indice de productivité du travail potentielle, instrumentum non compris *)


let is_passable hugros glacier seaIce =
     hugros < 2000 
  && not glacier
  && not seaIce
(* les marécages, jungles, glaciers, et banquises ne peuvent être traversés par l’exercitus *)

let fluxus hydros = match hydros with
| River record -> record.fluxus 
| _            -> 0
(* débit en m3 / sec *)

let rid   r = r.rid
let alt   r = r.alt
let area  r = r.area
let hydros r = r.hydros
let fluxus r = fluxus (hydros r)
let coast r = r.coast
let mountain r = r.mountain
let altitude r = altitudeFun r.alt
let thermos  r = r.thermos
let pluvia   r = r.pluvia
let hugros   r = hugrosFun r.hydros r.pluvia
let ariditas r = ariditas (thermos r) (hugros r)
let physis   r = physis  r.alt r.hydros (thermos r) r.pluvia
let climat   r = climat  (altitude r) (thermos r) (pluvia r)
let climax   r = climax  (altitude r) (hydros r) (thermos r) (pluvia r)
let physisValue rs r = physis r * rs
let hospitalitas r = hospitalitasFun (thermos r) (hydros r) (physis r)

 
(*  let is_glacier   r = is_glacier r.alt (thermos r) (pluvia r)*)
(*  let is_seaIce    r = is_seaIce  r.alt  (thermos r)*)
(*  is_passable (hugros r) (is_glacier r) (is_seaIce r)*)


