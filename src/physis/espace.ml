(*

 ****************************** Espace.ml ******************************


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

type web =
| Quadral
| Octal
(* nb de voisins en mode cylindre *)
(* le mode quadral permet d’accélérer la génération de carte : la génération initiale se fait en Octal pour avoir de
beaux continents ; les espaces intermédiaires de projection sont en quadral pour une finition accélérée ; la carte
finale est en Octal *)

type forme =
| Cylinder of web
| Sphere

type resolution = 
| Higher
| High    
| Low   
| Lower 
| Agglo   (*résolution optimale pour l’agglomération initiale de continents*)


module Direction = struct
  type t = int
  let nord = 0
  let ne   = 1
  let east = 2
  let se   = 3
  let sud  = 4
  let sw   = 5
  let west = 6
  let nw   = 7
  let lesQuatre  = [ nord;     east;     sud;     west     ]
  let lesHuit    = [ nord; ne; east; se; sud; sw; west; nw ]
  let random4 () = 2 * Random.int 4
  let random8 () =     Random.int 8
  let next4   d  = ( d+2 ) mod 8
  let next8   d  = ( d+1 ) mod 8
  let to_vecteur d =
    let tab = [| (-1, 0); (-1, 1); ( 0, 1); ( 1, 1); ( 1, 0); ( 1,-1); ( 0,-1); (-1,-1); |] in
    Array.get tab d
  (* variations en (latitude, longitude) *)
  let includes pos dir2 = 
       ( pos == dir2 )
    || ( dir2 == nord && ( pos == ne || pos == nw ) )
    || ( dir2 == east && ( pos == ne || pos == se ) )
    || ( dir2 == sud  && ( pos == sw || pos == se ) )
    || ( dir2 == west && ( pos == sw || pos == nw ) )
  (** NE includes North etc. *)
end

type direction = Direction.t


let vecteur_of_direction = Direction.to_vecteur

let latMax = 80. 
let lonMax = 180.
let latMin = -. latMax
let lonMin = -. lonMax
let wid    = lonMax -. lonMin (* width  in degres *)
let hid    = latMax -. latMin (* height in degres *)
let sid    = wid *. hid       (* surface in degres2 = 64 800 *) 
let whdr   = wid /. hid
let lonEquals p a b = Tfloat.abs (Tfloat.modulo a wid -. Tfloat.modulo b wid) <= p
let rec legalLon lon = if lon > lonMax then legalLon(lon -. wid)
                  else if lon < lonMin then legalLon(lon +. wid)
                  else lon

let wik    = 4.00 *. (10. ** 4.)
let hik    =  wik /. whdr
let rayon  =  wik /. (2. *. pi)
let sik    = 4.   *. pi *. rayon *. rayon
let adik   = (sik /. sid) ** (0.5) (* degre moyen en km *)

let tw = 2 * iof latMax 

let yxr =
  let open Tfloat in
  let f x = 1.003 + 0.000075 * x in
  (* f 0. ;; - : float = 1.003*)
  (* f 40. ;; - : float = 1.00599999999999978*)
  (* f 80. ;; - : float = 1.009*)
  (* fn associant à une lat x le ratio entre la hauteur de la regio de latitude (x+1) et la hauteur de la regio de lat x *)
  let rec u n = if n = 0 then 1. else u (n--1) * f (float(n--1)) in
  let ua = Array.init (tw++1) u in
  (*# u 0;; - : float = 1.*)
  (*# u 1;; - : float = 1.003*)
  (*# u 2;; - : float = 1.00608422499999972*)
  (*# u 40;; - : float = 1.19493052434152758*)
  (*# u 80;; - : float = 1.608762110477443*)
  (* fn associant à une lat x (int) le ratio entre la hauteur et la largeur de la regio de lat x *)
(*  (fun lat -> 1.2 )*)
  (fun lat -> Array.get ua (cut 0 tw  lat))
(* yxr  : y/x ratio : rapport entre la hauteur et la largeur en km (et par conséquent devant être apparente/affichée) d'une regio, en fonction de sa latitude *)
(* à haute latitude, les regiones, aussi nombreuses sur une latitude, sont de plus en plus fines == le degré de longitude est de plus en plus court en kilomètres *)


let latdid = (* latitude distance in degres de longitude *)
  let open Tfloat in
  let rec v n = if n=0 then 0. else yxr (n--1) + v (n--1) in
  let va  = Array.init (tw++1) v in
  let get = (cut 0 tw) |- (Array.get va) in
  (*# v 0;;                                          - : float = 0.*)
  (*# v 80;;                                         - : float = 98.7710401646229315*)
  let ved slat = (*signed lat*) (* visible (from) equateur distance (en degré de lat apparent) *)
    let lat =  abs slat in 
    let int =  iof lat  in
    let p   = next lat - lat in
    let d   = barycenter ~p (get int) (get (int++1)) in
    (slat<0. => -. d) d in
  (* visible (from) equateur distance (en degré de lat apparent) *)
  (* fn associant à une lat x (float) la distance (float) à l'équateur, en degrés de latitude *APPARENTS* *)
  (fun las lat      -> ved lat - ved las)

(* latdid : latitude distance : distance (float) en degres de longitude, dans la hauteur de la carte, entre la latitude (float) d'une regio, et la latitude (float) d'une autre regio*)
(* En réalité (sur la sphère), les regiones sont de largeur décroissante aux poles. À l'affichage, elles sont de largeur constante et de hauteur croissante (on conserve les formes réelles en accroissant les surfaces comme Mercator). Latdid évalue cette distance déformée en latitude entre deux latitudes *)

(***********************************   MODULE CYLINDER   ******************************************)

module Cylinder = struct
  let latMax = latMax

  let ahir   = 24
  let awir   = ahir * (iof lonMax * 5) / (iof latMax * 6) 
(* × 5/6 pour avoir des regiones équatoriales 20% plus large que haute, tandis que les régions tempérées sont dans un rapport
 * env. inverse *)
  let rratio = function (* resolution ratio : pas touche *)
  | Higher ->  16 
  | High   ->   8
  | Low    ->   4
  | Lower  ->   2
  | Agglo  ->   1
  let wir r = awir * rratio r
  let hir r = ahir * rratio r
  (* multiplication par rratio à la fin de façon à ce qu’il y ait bien un rapport de 2 entre les hauteurs de chaque résolution
  (sans quoi on se paye un index out of bounds) *)
  let sir r = wir r * hir r

  let y r (i:Rid.t) = (i:>int) /   (wir r)
  let x r (i:Rid.t) = (i:>int) mod (wir r)
  let  rid_of_ryx r y x = let w=wir r in Rid.oi (y*w + Ext.modulo x w)
  let srid_of_ryx r y x = 
    let w=wir r
    and h=hir r in
    let y = if y >= h then h-1     else if y<0 then 0              else y
    and x = if x >= w then x mod w else if x<w then Ext.modulo x w else x in
    rid_of_ryx r y x
  (* secured rid_of_ryx *)

  let yx_of_rrid r rid = (y r rid), (x r rid)

  let is_polar r rid = let y = y r rid in (y=0 || y=hir r -1)
 
  let randomRid ?(polarExclusion=0.0) r = 
    let delta = iof(polarExclusion *. foi(sir r)) in
    Rid.oi (Random.int ((sir r) - 2*delta) + delta)
  (* une case au hasard, possiblement pas au bords N/S *)

  open Tfloat

  let regioHid res = hid / (foi(hir res)) (* hauteur des regio exprimée en degré de latitude *)
  let regioWid res = wid / (foi(wir  res)) (* largeur des regio exprimée en degré de longitude *)
  let regioSid res = regioWid res * regioHid res

  let regioHik res     = hik / (foi(hir res)) 
  let regioWik res lat = wik / (foi(wir res)) / yxr (iof (abs lat))

  let lat res rid = (foi(y res rid -- div (hir res) 2) + 0.5) * regioHid res (* latitude d’une regio d’un cylindre *)
  (* De -80 (=80° Nord) à +80 (=80° Sud) *)
  let lon res rid = (foi(x res rid -- div (wir  res) 2) + 0.5) * regioWid res
  (* De -180 (=80° West) à +180 (=80° Est) *)


end


(***********************************   MODULE SPHERE   ******************************************)
module Sphere = struct
  let sir = function
  | Higher->252144
  | High  -> 65536
  | Low   -> 16384
  | Lower ->  4096
  | Agglo ->  1024
(* le rapport *4 doit être respecté pour que les projections ne plantent pas *)
end



(******************************   MODULE PROXIMAE    **********************************************)

module Proximae = struct
(** liste des regiones voisines d’une regio *)

  type t = {
    lesQuatre : ((direction*Rid.t) list) option; (*option réservée aux formes Cylinder _*)
    lesHuit   : ((direction*Rid.t) list) option; (*option réservée à la forme Cylinder Octal *)
    toutes    : Rid.t list;        (*liste de toutes les proximae d’une regio, quelque soit la forme de l’espace *)
    }

  let lesQuatre p = Tlist.valueList(Opt.value p.lesQuatre)
  let lesHuit   p = Tlist.valueList(Opt.value p.lesHuit)
  let toutes    p = p.toutes

  let random_fold_left p s f rate = 
    let len = List.length p.toutes in
    let intList    = ref [] in
    let intListLen = 1 + iof (rate *. foi len) in
    let () = Ext.iter intListLen (fun _ -> let i = Random.int len in if (List.mem i !intList == false) then intList:= (i :: !intList)) in
    List.fold_left f s (List.map (List.nth p.toutes) !intList)



(******************************   MODULE PROXIMAE.CYLINDER   *************************************)
  module Cylinder = struct 
(*    let quatreDirList = [Nord; East; Sud; West]*)
    (* dorsaleCreate exige que les 4 dirs soient « à la suite » dans le sens d’une rotation *)
(*    let huitDirList   = [Nord; NE; East; SE; Sud; SW; West; NW]*)
  
    let dirList = function 
    | Quadral -> Direction.lesQuatre
    | Octal   -> Direction.lesHuit
    let toutes p = function
    | Quadral -> Opt.value p.lesQuatre
    | Octal   -> Opt.value p.lesHuit
    
    let proxima web proximae dir = List.assoc dir (toutes proximae web) 
    (* trouver une proxima en particulier en fonction de sa position, dans une liste de 4 ou 8 *)

    let proximaeCreate web r rid =
      let coords = Cylinder.y r rid, Cylinder.x r rid in
      let h = Cylinder.hir r in
      let cylinderSelection dirList =
        let f list dir = 
          let py, px = Couple.sum coords (vecteur_of_direction dir) in
          if py>=0 && py<h then (dir, Cylinder.rid_of_ryx r py px) :: list 
          else list in
        List.fold_left f [] dirList in
      let lesQuatre = cylinderSelection Direction.lesQuatre in match web with
      | Quadral -> {
                   lesQuatre = Some lesQuatre;
                   lesHuit   = None;
                   toutes    = Tlist.valueList lesQuatre;
                   }
      | Octal   -> let lesHuit = cylinderSelection Direction.lesHuit in
                   {
                   lesQuatre = Some lesQuatre;
                   lesHuit   = Some lesHuit;
                   toutes    = Tlist.valueList lesHuit;
                   }
    (* génération des listes de proximae d’une regio, en mode cylindre *)
  end
(******************************   MODULE PROXIMAE.SPHERE   *************************************)
  module Sphere = struct
    let proximaeCreate rid =
      {
      lesQuatre = None;
      lesHuit = None;
      toutes = []; (* à compléter *)
      }
  end
(*************************************************************************************************)
  
  let proxima forme proximae dir =
    match forme with
    | Cylinder web -> Cylinder.proxima web proximae dir
    | _ -> Rid.none
  (* trouver une proxima en particulier en fonction de sa position, dans une liste de 4 ou 8 *)

end (* of module PROXIMAE *)
(******************************* MODULE ESPACE : FONCTIONS DE LECTURE *****************************************)
                                                              
type t =
  {
  forme : forme;
  resolution : resolution;
  proximaeArray : Proximae.t Rid.Array.t;
  }
(** Espace.t *)


let forme          e = e.forme
let resolution     e = e.resolution
let proximaeArray  e = e.proximaeArray
(** tableau des proximae *)


let wir  e = match e.forme with
| Cylinder _ -> Cylinder.wir (resolution e)
| Sphere     -> raise (Failure "Sphere.wir is undefined")

let hir e = match e.forme with
| Cylinder _ -> Cylinder.hir (resolution e)
| Sphere     -> raise (Failure "Sphere.hir is undefined")

let sir   e = match e.forme with
| Cylinder _ -> Cylinder.sir (resolution e)
| Sphere     -> Sphere.sir (resolution e)

let dimir  e = sir e, wir e, hir e     (* dimension in regiones *)

let centerRid e = match e.forme with
| Cylinder _ -> Cylinder.rid_of_ryx (resolution e) ((hir e)/2) ((wir e)/2)
| Sphere     -> Rid.first

let distance (lat,lon) (lau, loo) =
  let open Tfloat in
  let latDif = lat - lau in 
  let lonDif = 
    let a = abs (lon - loo) in 
    if a<lonMax then a else (wid - a) in (* une différence en longitude ne peut excéder 180 *)
  adik * ( (latDif * latDif + lonDif * lonDif) ** 0.5 )
(* une approximation de la distance en km entre deux points définis par latitude et longitude *)
(* mieux : distance = R*acos(cos(a)*cos(b)*cos(c-d)+sin(a)*sin(b)) *)


(************** MODULE ESPACE : propriétés génériques des regiones de l'espace *)

let rs ?(lat=0.) e = match e.forme with
| Sphere     -> sik /. (foi (sir e))       (* regio superficie en km2 = superficie de la Terre / nb de regiones *)
| Cylinder _ -> (Cylinder.regioWik (resolution e) lat *. Cylinder.regioHik (resolution e))

let rw ?(lat=0.) e = match e.forme with
| Sphere     -> (rs e) ** 0.5       (* regio width en km *)
| Cylinder _ -> Cylinder.regioWik (resolution e) lat

let rh           e = match e.forme with
| Cylinder _ -> Cylinder.regioHik (resolution e)
| Sphere     -> rw e

let rdim ?(lat=0.) e = rs ~lat e, rw ~lat e, rh e

let rsid   e = match e.forme with
| Cylinder _ -> Cylinder.regioSid (resolution e)
| Sphere     -> 1. (*Sphere.regioSid (resolution e)*)

(*************** MODULE ESPACE.REGIO : propriétés d'une regio appelée par son id *************)

module Regio = struct
  
  let latitude e (rid:Rid.t) = match e.forme with
  | Cylinder _ -> Cylinder.lat (resolution e) rid
  | Sphere     -> assert false
  
  let longitude e (rid:Rid.t) = match e.forme with
  | Cylinder _ -> Cylinder.lon (resolution e) rid
  | Sphere     -> assert false
  
  let coords e (rid:Rid.t) = latitude e rid, longitude e rid
   
  let superficie    e rid = match e.forme with
  | Cylinder _ -> rs ~lat:(latitude e rid) e
  | Sphere     -> rs e
  (** superficie d'une regio en km2 *)

  let proximaeRecord e rid = Rid.Array.get (proximaeArray e) rid
  let lesQuatre      e rid = Proximae.lesQuatre (proximaeRecord  e rid)
  let lesHuit        e rid = Proximae.lesHuit   (proximaeRecord  e rid)
  let proximae       e rid = Proximae.toutes    (proximaeRecord  e rid)
  (** liste des proximae d’une regio *)
  let random_fold_left e rid s f rate = Proximae.random_fold_left (proximaeRecord e rid) s f rate

  
  let proxima   e rid dir = Proximae.proxima (forme e) (proximaeRecord e rid) dir
  (** trouver une proxima en particulier en fonction de sa position, dans une liste de 4 ou 8 *)

  let distance e rid rie = distance (coords e rid) (coords e rie)
  (** distance en km entre deux regiones définies par leur rid *)
end




(******************************* MODULE ESPACE : FONCTIONS DE CREATION/TRANSFORMATION ****************************)

let create forme res =
  let e = 
    {
    forme = forme;
    resolution = res;
    proximaeArray = Rid.Array.empty;
    } in
  { 
  e with
  proximaeArray = match forme with
  | Cylinder web -> Rid.Array.init (sir e) (Proximae.Cylinder.proximaeCreate web (resolution e) )
  | Sphere       -> Rid.Array.init (sir e) (Proximae.Sphere.proximaeCreate)
  }
(* création d’un espace vide *)

let resolutionList = [ Agglo ; Lower; Low; High; Higher ]

let projNb initRes finalRes = 
  let l = [ Agglo ; Lower; Low; High; Higher ] in
  Tlist.mem_n finalRes l - Tlist.mem_n initRes l
(*def° du nb de projection en fonction de la taille initiale de la génération et de la taille finale de carte demandée*)

let dilate e forme = create forme (Tlist.following e.resolution resolutionList)
(*dilatation de l’univers*)



