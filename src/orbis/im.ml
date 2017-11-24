(*

 ****************************** Imperium_map.ml ******************************


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

module E  = Espace
module ER = Espace.Regio
module J  = Junctiones
module Rvi= Rv.Incola

type nid = Nid.t

type t = Rv.t Rid.Array.t

type natio = {
  centralized : bool;
  nav : bool;
  pop : float; (**undef*)
  artes : Ars.t list;
  sophia: float;
  fides : float;
  densitas : float; (** densité en hab/km2 *)
  humanitas :float;
  instrumentum : float;
  plebs : float;
  facultas : float;
  plebsVar : float;
  alimonium_ratio : float;
  agriCopia : float;
  vis : float;
  }
(* ce que Im.update a besoin de savoir des nationes *)

module Ria = Rid.Array
module Nia = Nid.Nia
(*module Nix = Nid.Nix*)

let centralized   na i = (Nia.get na i).centralized
let nav           na i = (Nia.get na i).nav
let pop           na i = (Nia.get na i).pop
let artes         na i = (Nia.get na i).artes
let sophia        na i = (Nia.get na i).sophia
let fides         na i = (Nia.get na i).fides
let densitas      na i = (Nia.get na i).densitas
let humanitas     na i = (Nia.get na i).humanitas
let instrumentum  na i = (Nia.get na i).instrumentum
let natioPlebs    na i = (Nia.get na i).plebs
let natioFacultas na i = (Nia.get na i).facultas
let plebsVar      na i = (Nia.get na i).plebsVar
let alim_ratio    na i = (Nia.get na i).alimonium_ratio
let agriCopia     na i = (Nia.get na i).agriCopia
let vis           na i = (Nia.get na i).vis
 

let length (im:t) = Ria.length(im) 

let nIter f rm = Ria.nIter f rm

let get         im (i:Rid.t) = Ria.get im i
let incola      im (i:Rid.t) = Rv.incola (get im i)
let dominus     im (i:Rid.t) = Rv.dominus (get im i)
let facultas rm im (i:Rid.t) = Rv.facultas (Rm.get rm i) (get im i)


let is_competitor pass j na dom (i:nid) =
  match J.is_attacking j i dom with (* dont worry : le compilateur gère ça mieux que toi (testé) *)
  | Some offensive -> pass == Rv.Passable || ( pass = Rv.Navigable && nav na i )
  | None -> false
(* L'empire n°i veut et peut-il conquérir la régio r ? *)


let regio_colonizability e r rv =
  if R.alt r >=0
  && Rv.incola rv = None
  && Rv.is_farmable r
  then iof(80. * (R.hospitalitas r) / (E.rh e))
  else 0
(* donnée valable pour tous les colonizateurs *)

let is_natio_colonizatrice na nid r regioDominus =
  let inst      = instrumentum na nid
  and agriCopia = agriCopia    na nid in
  ( regioDominus = Nid.none || regioDominus = nid )
  && let cho = Rv.Fun.chorability ~inst (Rv.tegmen r) in
  Random.int 100 < iof(cho / agriCopia) 
(* est-ce que la regio intéresse cette natio en particulier (pour une colonization) *)

let local_colonization na nid r rv rcy =
  Random.int 100 < rcy 
  && is_natio_colonizatrice na nid r (Rv.dominus rv)
(* colonization d’une terre dont on est déjà propriétaire *)

let competitores e rm im j na (rid:Rid.t) rcy =
  let dom = dominus im rid in
  let r  = Rm.get rm rid in
  let rv =    get im rid in
  let is_colonizable = Random.int 1000 < rcy in
  let is_passable    = Rv.is_passable r rv in
  (* colonization progressive des regiones adjacentes : 10 fois plus lente que la local_colonization *)
  let rec f = function
  | []    ,il -> il
  | pid::q,il -> let i=dominus im pid in (* propriétaire de la regio voisine *)
    if i<>Nid.none
    && i<>dom
    && List.mem_assoc i il=false 
    then (      if is_competitor is_passable j na dom i = true   
                then f(q,((i,false)::il)) (* conquete militaire *)
           else if is_colonizable
                && ( match incola im pid with Some inc -> Rvi.nid inc = i | _ -> false )
                (* empire i a au préalable colonisé la regio voisine *)
                && is_natio_colonizatrice na i r dom
                then f(q,((i,true )::il)) (* colonization anarchique *)
           else f(q,il) )
    else f(q,il) in
  let proximae_competitores = f(ER.lesQuatre e rid,[]) in
  if dom == Nid.none then proximae_competitores 
  else (dom,false) :: proximae_competitores
(* Liste des imperium en compétition pour être le dominus au tour à venir *)
(*(* Liste des imperium en mesure d'absorber la régio r *)*)
(* on précise s'il y a colonisation agricole simultanée (true) ou non (false) *)
(*
let deminutio e rm im j r =
  let dom = dominus im r
  and inc = incola im r
  and prox = ER.lesQuatre e r in
  let rec is_finium = function
  | [] -> false
  | e::q -> (dominus im e <> dom) || is_finium q in
  let mutatio,proba = J.mutatio j dom inc in
     (mutatio=J.Deminutio) (*retraite des terres de la natio rn*)
  && (proba = 100 || (Random.int 100)<proba)
  && (is_finium prox) (*regio frontalière*)
(* abandon de la regio par son dominus *)
*)


let neo_dominus e rm im j na r rcy =
  let cs = competitores e rm im j na r rcy in
  let maximus (i,f) (j,g) = if vis na i > vis na j then (i,f) else (j,g) in (*le plus fort*)
  List.fold_left maximus (Nid.none,false) cs 
(* en l’absence de competitor, le neo_dominus est Nid.none *)
(* avec 1 competitor, il l’emporte si sa vis est supérieure à celle de Nid.none, soit 0 *)
(* le booléen est le flag de colonisation *)
(* (nouveau?) Propriétaire de la régio r *)



(*
let apmx j na =
  let f i d = 
  match J.relatio j i d with
  | J.Bellum -> min 500 ((fides na d/100)/(densitas na i*10))
  | J.Pax    -> min 500 (10*(humanitas na d)/(humanitas na i) - 10)
  | _        -> 0 in
  Nix.init f
(* matrices évaluant la proba de dispersion ou d’assimilation de i par d *)
(* i=ordonnée, d=abscisse *)

*)

let nea_plebs rs p (*regio plebs*) f (*regio facultas*) na(*natioArray*) i(*regio incola*) colonize = 
  if colonize then f * ((max u (u / agriCopia na i + 0.1)) ** 2.) 
  else
  let dxVar = p * (plebsVar na i - u) in
  (* acroissement démographique de la regio (en nb de personnes), au taux national *)
  let miVar =
    let np  = natioPlebs    na i in
    let nf  = natioFacultas na i in
    let nr  = np / nf in (* national ratio = point de rupture entre les regiones d’immigration et d’émigration au sein de la chora *)
    let rr  =  p /  f in (* regional ratio *)
    (rs) (* la regio plus vaste génère de plus grosse migration / échelle du jeu *)
    * (densitas na i)  (* la natio plus peuplée génère de plus grosse migration *)
    * 0.012  (* constante à fixer librement *)
    * (nr - rr) in (* plus la regio est vide et offre des opportunités, plus elle reçoit, et inversement *)
    (* accroissement migratoire (positif ou négatif) *)
  p + dxVar + miVar
(* calcul de la nouvelle plèbe régionale *)


let update e rm im j (na:natio Nia.t) =
  let s = Espace.sir e in
  let vlr = ref [] in
(*  let apmx = apmx j na in*)
  let f (rid:Rid.t) =
    let rv =    get im rid in
    let r  = Rm.get rm rid in
    let rcy= regio_colonizability e r rv in
    let dom,colonize = neo_dominus e rm im j na rid rcy in
    let colonize     = colonize || local_colonization na dom r rv rcy in
(*    let ina = isNeoAger r (Rv.tegmen rv) riv dom na in*)
    let hum = match Rv.contents rv with
    | Rv.Desertum_for n when colonize -> Rv.Incol (Rvi.colonus dom (Rv.facultas r rv) (agriCopia na dom)) 
    | Rv.Desertum_for n -> Rv.Desertum_for (n++1) 
    | Rv.Incol incola ->  
      let i     = Rvi.nid incola in
      let p_oik = Rvi.oikos incola in
      let plebs = nea_plebs (R.area r) (Rvi.plebs incola) (Rv.facultas r rv) na i colonize in
      let ins   = Rvi.Next.instrumentum (Rv.instrumentum rv) (artes na i) (sophia na i) in
      let oikos, new_vicus = Rvi.Next.oikos r (p_oik) (Rv.tegmen r ~rv)  plebs ins (agriCopia na i) in
      let dominium = Rvi.dominium incola in
      let incola = Rvi.make plebs i oikos dominium ins in
      if new_vicus then vlr := ( (rid,incola)::(!vlr) ) ;
      Rv.Incol (incola) in
    Rv.update rv dom hum in
  Ria.init (s) (f), (!vlr)
(*  Ria.update f im; im : ne fonctionne pas correctemment : il faut différencier la carte de lecture et celle d’écriture*)
(* mise à jour de l'imperiumMap *)


let create e rm cm(*civilizations map*) ccm = 
  let s = Espace.sir e (*Ria.length cm*) in
  let f (i:Rid.t) =
    let civ,origo = Ria.get cm i in (*année de colonisation agricole*)
    if civ=Nid.none then Rv.null (*regio inhabitée*)
      else
      let age = Date.distance origo Date.beginning in
      if age=0 then Rv.null (* notamment à cause des log age qui suivent*)
        else
        let dominus = civ in
        let r       = Rm.get rm i in
(*        let oikos   = if civ=Ria.get ccm i then Rvi.Urbs else Rvi.Saltus in*)
        let oikos   = Rvi.Saltus in
        let incola  = Rv.Incola.create r civ oikos age in
        Rv.make dominus (Rv.Incol incola) in
  Ria.init s f (*(fun i -> Rv.null)*)
(* étape finale de la génération de carte *)

(*let n_humanitas_fun r human tegmen plebs inst =*)
