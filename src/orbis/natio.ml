(*

 ****************************** Natio.ml ******************************


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

module Nil = Nid.Nil
module P   = Partitio
module PR  = Partitio.Record
module Pa  = Politeia
module Gn  = G.Natio
module K   = Aedificium

type key =
  | Facultas     
  | Plebs        
  | Hospitalitas 
  | Instrumentum 
  | Efficientia  
  | Famine       
  | Copia        
  | Tfg          
  | Isf          
  | DxVar        
  | Alimonium_ratio 
  | Facultas_ratio  
  | Sophia       
  | Fides        
  | Libertas     
  | AgriCopia    
  | Densitas     


type t = {
  nid : Nid.t;
  active: bool;
  origo : Rid.t*Date.t; (* date de naissance *)
  urbs  : bool*Rid.t;   (* caractérise la natio au tour(n) *)
  artes : Ars.t list; (* le tour *)
  g  : G.Natio.t; (* geographie nationale *)
  junctiones : Junctiones.Natio.t; (* junctiones relatives à une natio (vision nationale) *)
  k  : K.t;      (* idem *)
  pa : Politeia.t;     
  p  : Partitio.Record.t;       (* caractérise la période tour(n-1) à tour(n) *)
  luc: Partitio.t;   
  d  : Dx.t; (* le tour (pyramid), la période(famine, tfg) *)
  fd : Dx.t; (* le tour (n+1) (pyramid projetée), la période suivant le tour n (famine, tfg) *)
  }

let null =
  {
  nid = Nid.none;
  active=false;
  origo = Rid.none,Date.Unknown;
  urbs  =(false,Rid.none);
  artes =[];
  g  = G.Natio.null;
  junctiones = Junctiones.Natio.null;
  k  = K.null;
  pa = Pa.anarchy;
  p  = P.Record.null;
  luc= P.null;
  d  = Dx.null;
  fd = Dx.null;
  }

(*let populatioFun plebs facultas = squot 2. plebs facultas*)
(* *population (degré de peuplement) de la natio n *)

let agriCopiaFun facultas pr =
  let factum = PR.factum pr in
  let cibus = P.cibus factum in
  let luxus = P.lux factum in
  (squot 0. (facultas) (cibus+luxus) )
(* abondance en ager *)
(* vaut normalement plus que u. vaut moins quand la terre et l’instrumentum sont insuffisants pour assurer 
la production matérielle (cibus (dont pillage/tribut) et luxe) *)


let efficientiaFun = Rv.Fun.efficientia
(* productivité du travail *)


let rec artesFun = function
| [] -> []
| (ars,i)::q -> if i=1. then ars::artesFun q else artesFun q
(* artes maîtrisés *)


(************************************ fonctions de lecture ********************************************)

let nid          n = n.nid
let is_active    n = n.active
let origo        n = n.origo
let has_urbs     n = fst       n.urbs 
let urbsRid      n = snd       n.urbs 
let artes        n = n.artes
let has_nav      n = List.mem Ars.NAV (artes n)
(*let ter_rayon    n = Gn.instrumentum n.g * 20.*)
(*let mer_rayon    n = if has_nav n then Gn.instrumentum n.g * 100. else 0.*)
let ter_rayon    n = Gn.instrumentum n.g * 100.
let mer_rayon    n = if has_nav n then Gn.instrumentum n.g * 200. else 0.
let partitio     n = P.Record.attrib n.p (*les attributiones*)
let lucrum       n = n.luc (*les revenus*)
let kapital      n = n.k
let politeia     n = n.pa
let geographia   n = n.g
let junctiones   n = n.junctiones
let seditio      n = K.seditio       n.k
let vis          n = K.vis           n.k
let imperium     n = Gn.imperium     n.g
let chora        n = Gn.choraAmp     n.g
let facultas     n = Gn.facultas     n.g
let plebs        n = Gn.plebs        n.g
let hospitalitas n = Gn.hospitalitas n.g
let instrumentum n = Gn.instrumentum n.g
let latifundium  n = Gn.latifundium  n.g
let pil          n = Gn.pil          n.g
let efficientia  n = efficientiaFun (hospitalitas n) (instrumentum n)
let famine       n = Dx.famine       n.d
let copia        n = Dx.copia        n.d
let tfg          n = Dx.tfg          n.d
let isf          n = fst(Dx.isf      n.d)
let dfn          n = snd(Dx.isf      n.d)
let dxVar        n = Dx.var          n.d
let alimonium_ratio n = Dx.alimonium_ratio n.d
let facultas_ratio  n = Dx.facultas_ratio  n.d
let pyramid      n = Dx.pyramid      n.d
let sophia       n = K.sophia        n.k
let fides        n = K.fides         n.k
let libertas     n = K.libertas      n.k
let ususList     n = K.ususList      n.k
let agriCopia    n = agriCopiaFun (facultas n) (n.p)
let densitas     n = Tfloat.squot 0. (plebs n) (chora n)
(* densité de population en hab/km2 (sic) *)


let value n = function
  | Facultas       -> facultas n      
  | Plebs          -> plebs n      
  | Hospitalitas   -> hospitalitas n      
  | Instrumentum   -> instrumentum n      
  | Efficientia    -> efficientia n      
  | Famine         -> famine n      
  | Copia          -> copia n      
  | Tfg            -> tfg n      
  | Isf            -> isf n      
  | DxVar          -> dxVar n            
  | Alimonium_ratio-> alimonium_ratio n         
  | Facultas_ratio -> facultas_ratio n         
  | Sophia         -> sophia n      
  | Fides          -> fides n      
  | Libertas       -> libertas n      
  | AgriCopia      -> agriCopia n      
  | Densitas       -> densitas n      




let kill n =
  {
  null with 
  origo = n.origo
  }

(*************** fonctions de lecture spécialisées pour les modules antérieurs ****************)


let aeNatio n cArtes pr luc = ( {
  K.k = kapital n;
  K.g = geographia n;
  K.plebs = plebs n;
  K.pArtes = artes n;
  K.cArtes = cArtes;
  K.pp = partitio n; (* previous partitio *)
  K.pr = pr; (*current partitio record*)
  K.luc = luc; (*current lucrum partitio *)
  } : Aedificium.natio)


let imNatio n = ( {
  Im.centralized = Pa.is_centralized (politeia n);
  Im.nav = has_nav n;
  Im.pop = 0.; (*populatio n;*)
  Im.densitas = densitas n;
  Im.artes = artes n; 
  Im.sophia = sophia n; 
  Im.fides = fides n; 
  Im.humanitas = 0. (*P.humanitas (P.alter_all n.p [PYRAMID(Dx.pyramid n.d,(iof n.k.plebs));
  PRODUCT(n.k.usus,(iof n.c.efficientia),n.artes)] )*);
  Im.instrumentum =  instrumentum n;
  Im.luxus = P.lux (lucrum n);
  Im.plebs = plebs n;
  Im.facultas = facultas n;
  Im.plebsVar = Dx.var n.fd; (* les regiones vont évoluer en fonction de la variation projetée (fd) *)
  Im.alimonium_ratio = alimonium_ratio n; (* pour calculer les besoins en nourriture des regiones *)
  Im.agriCopia = agriCopia n;
  Im.vis = vis n;
  } : Im.natio )
(* infos nationales dont Im a besoin*)


let jNatio n = {
  Junctiones.chora = chora n;
  Junctiones.imperium = imperium n;
  Junctiones.vis = vis n;
  }
(* infos nationales dont J a besoin*)

let pNatio n =
  {
  P.g = geographia n;
  P.pyramid = pyramid n;
  P.pp = partitio n;
  P.artes = artes n;
  P.politeia = politeia n;
  P.plebs = plebs n;
  P.fides = fides n;
  P.libertas = libertas n;
  P.efficientia = efficientia n;
  P.latifundium = latifundium n;
  P.facultas = facultas n;
  P.agriCopia = agriCopia n;
  P.ususList = ususList n;
  }


let paNatio cl n = ( {
  Politeia.p          = politeia n;
  Politeia.poleis     = (match cl with e::q -> true | [] -> false) ;
  Politeia.writing    = List.mem Ars.WRI (artes n);
  Politeia.metallurgy = List.mem Ars.MET (artes n);
  Politeia.agriCopia  = agriCopia n;
  Politeia.sophia     = sophia n;
  Politeia.pil        = pil n;
  } : Politeia.natio )
(* Politeia natio *)


(************************************* BROUILLARD DE GUERRE ***************************************************)

type regio =
| Cognita
| Terra_incognita
| Incognita

let regio n e lat lon alt coast brouillards = 
  if Rv.Brouillards.read brouillards (nid n) then Cognita 
  else
    let urbsRid = urbsRid n in
    let urbsLat = Espace.Regio.latitude  e urbsRid in
    let urbsLon = Espace.Regio.longitude e urbsRid in
    let d = Espace.distance (lat,lon) (urbsLat, urbsLon) in
         if d< ter_rayon n        && alt >= (-1) then Cognita
    else if has_nav n && d<mer_rayon n then (if alt < 0 || coast then Cognita else Terra_incognita)
(*  else if d<(ter_rayon n * 1.33) && alt >= ( 0) then Terra_incognita*)
    else Incognita
(* la regio (lat,lon,alt,coast) est-elle connue de la natio n *)

(******************************************* NATIO UPDATE ***************************************************)

let n_artes pa(*previous artes*) politeia sophia inst plebs e_sapientia proxArtes =
  let rand x = Random.int (max 1 (iof x)) = 0 in
  let v = 4. in (* vitesse de découverte *)
  let f ars =
          let lev x = foi (Ars.level ars) * (10. ** x)  in
          List.mem ars pa
  || (    sophia * 100. > lev 0.
       && (      ars =Ars.MET (* pas de prérequis pour découvrir la métallurgie *)
            || ( ars =Ars.WRI && Politeia.is_civilized politeia ) 
            || ( ars!=Ars.WRI && List.mem Ars.MET pa ) )
       && ( 
                 (rand (lev (7. - v) / e_sapientia / log plebs)) (*invention*)
            || (List.mem ars proxArtes && rand (lev (6. - v) / e_sapientia / inst)) (*diffusion*)
               ) ) 
  in List.filter f Ars.list
(* techniques découvertes *)


let update gn jn cl n(*natio*) pr luc p(*partitio*) pArtes = 
  match G.Natio.choraAmp gn with
  | 0. -> kill n
  | _ -> 
  let d  = Dx.update  n.fd (G.Natio.plebs gn) in
  (* la nouvelle démographie est le « produit » de la forward demographie de l’année précédente et de la géographie nationale présente (perte de regiones ?) *)
  let fd = Dx.preview d  (P.cibus luc) (sophia n) (fides n) (facultas n) (P.mil p) 0. in
  let artes = n_artes (artes n) (politeia n) (sophia n) (instrumentum n) (plebs n) (P.sap (P.Record.fructus pr)) pArtes in
(*  let artes = (artes n) in*)
  let k  = Aedificium.update (aeNatio n artes pr luc) in
  { 
  n with
  g = gn;
  junctiones = jn;
  artes = artes;
  k   = k;
  pa  = Pa.update (paNatio cl n);
  p   = pr;
  luc = luc;
  d   = d;
  fd  = fd;
  }
  

(*let update (rm, rs, j, im, g, fr) id n(*natio*) p(*partitio*) prl = n
(* revenus effectifs de l'année passée *)
  and lostRatio = G.lostRatio g j id
(* proportion de la chora occupée par des nationes ennemies *)
  and warRatio = G.warRatio g j id in
(* proportion de nationes ennemies parmi les proximae *)*)
(*  let d = Dx.v n.d e.labor n.k.sophia n.k.fides n.c.copia (iof n.k.plebs) p.militaria warRatio lostRatio in*)
(* situation démog au tour n, fonction des naissances et décès de l'année passée *)
(*  let artes = artes (n.artes, n.k.sophia, (iof n.k.instrumentum), e.sapientia, (iof n.k.plebs), prl) in*)
(* artes au tour n, fonction des découvertes faites l'année passée *)
(*  let k = K.v (n.k, n.p, n.artes) (p, e) (Dx.pyramid d, artes, lostRatio) in*)
(* capitaux au tour n *)
(*  let c = computation rm rs im g id k.instrumentum e.militaria (iof k.plebs) in*)
(* données supplémentaires *)
(*  {
  n with
  p = p;
  d = d;
  artes = artes;
  k = k;
  c = c;
  }*)
(* Ordre des calculs : 

1. Productions effectives de l'année précédente, en fonction des capitaux et technologies disponibles au tour (n-1), et de la partitio réalisée durant cette année.

2. Naissances et Décès de l'année précédente, en fonction des productions effectives réalisées, et du capital sophia du tour (n-1)

3. Découvertes (pas d'impact des découvertes faites pendant l'année précédente sur les productions de l'année précédente)

4. Réactualisation des capitaux, en fonction des capitaux du tour précédent, des productions de l'année précédente, des naissances et décès de l'année précédente (calcul de plebs), et des découvertes faites l'année précédente (impact sur l'expérience)

5. Calcul de la chora et de l'imperium, en fonction des invasions de l'année précédente. On note que les invasions n'ont pas d'impact sur les productions l'année où elles ont lieo. (Le moment exact de l'invasion dans l'année étant inconnu, l'impact ou le non-impact sont équiprobables. Le non-impact est préféré pour des raisons de prévisibilité/jouabilité et simplicité du code).

Calcul de vis en fonction de l'imperium du tour présent, de militaria de l'année passée, et de la population du tour présent.

*)

(******************************* NATIO CREATE *******************************************)

let create rm im g nid civitas =
  let g = G.Natio.make g [] nid in
(*  let plebs = G.plebs g nid in (* Depr. let plebs = physis * Dx.npc * 141/100 in*)*)
  let efficientia = efficientiaFun (Gn.hospitalitas g) (Gn.instrumentum g) in
  let sophia = u - 0.92 / efficientia in (* constaté réaliste *)
  let d =Dx.create sophia (Gn.plebs g) in
  let fd=d in
(*  let d = Dx.update n.d (G.Natio.plebs gn) (P.lab e) (sophia n) (fides n) (facultas n) (P.mil p) 0. in*)
  let labor = min u (u - sophia + 0.1) in 
(*  let mil = (quot plebs (physis*Dx.npc) 0)/50 in*)
  let mil = 0. in
  let sap = 1. - labor - mil in
  let aff=P.make
        ~labor: labor
        ~sapientia: sap
        ~religio: 0.
        ~militaria: mil
        ~oppressio: 0.
        ~luxus: 0.
        ~otium: 0.
         in
  let k=K.make 
        ~seditio: 0.
        ~fides: 0.
        ~sophia: sophia
        ~ususList: Partitio.UsusList.null
        ~vis: 1. (*approximation*)
         in
  let artes = Ars.beginList in
  let natio =
    {
      nid = nid;
      active = true;
      origo = (Civitas.rid civitas, Civitas.origo civitas);
      urbs = (true, Civitas.rid civitas);
      artes = artes;
      g = g;
      junctiones = Junctiones.Natio.null;
      k = k;
      pa= Pa.anarchy;
      p = Partitio.Record.null;
      luc = Partitio.null;
      d = d;
      fd=fd;
    } in
    {
      natio
      with
      p = Partitio.Record.compute aff (pNatio natio)
    }

(* EOF *)
