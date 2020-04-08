
(*

 ****************************** flumenList.ml ******************************


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

module E = Espace
module ER= Espace.Regio
module ED= Espace.Direction
module F = Flumen
module Ria = Rid.Array

type t = Flumen.t list

let (%) = Ext.modulo

let dir o = match (o%4) with 0->ED.nord |1->ED.east |2->ED.sud |_->ED.west
(*les dirs doivent se « suivre », pour que les virages fonctionnent*)


let insert flumenList affluent =
  let rec g vus a_voir = match a_voir with
  | []   -> []
  | f::q when f.F.fid = affluent.F.fid -> (Flumen.cat f affluent)::(vus^^q)
  | f::q -> g (f::vus) q in
  g [] flumenList
(* remplace dans la flumenList le flumen d’id fid par la concaténation de lui et de son nvel affluent *)

let orientation pa pr i a mo o h vp =
           if pa i o>=a && pa i (o-1)<a then let o=(o-1)%4 in o,o,0 (*descente trouvee*)
      else if pa i o>=a && pa i (o+1)<a then let o=(o+1)%4 in o,o,0 (*idem*)
      else if pa i o>a  && o=mo  && pa i (o-h)=a then mo,(o-h)%4,2 (*contournement trouve*)
      else if pa i o>a  && o=mo  && pa i (o+h)=a then mo,(o+h)%4,2 (*idem*)
      else if pa i o>a  && o<>mo && pa i (mo) =a then mo,mo,0      (*idem*)
      else if pa i o=a  && pr i o=0 && o=mo  && pa i (o-1)=a && pa i (o+1)=a && Random.int 4<vp 
           then mo,(o+h)%4,0 (*hazard*)
      else if pa i o=a  && pr i o=0 && o<>mo && pa i (mo) =a && Random.int 4<vp then mo,mo,0 (*hazard*)
      else mo,o,vp+1
(*définition des new main orientation, orientation, virage proba*)

let flumenCreate aa wa p pa pr pt i pi mo o vp flumen = 
  let rec f i pi mo o vp flumen =
    (*id, previous id, main orientation, orientation, virage proba*)
    let a = Ria.get aa i in (*alt*)
    let _ = if (*a>=1 &&*) (Ria.get aa pi=a) && (pa i (o-1)<a) && (pa i (o+1)<a) && (pa i o<=a) 
            then Ria.set aa i (a-1) (*ravinement*) in
    let a = Ria.get aa i in
    let h = 1 - 2*Random.int 2 in (*sens (gauche,droite) des premiers essais, pour ce tour*)
    let nmo,no,nvp = orientation pa pr i a mo o h vp in  (*new main orienation etc.*)
         if pa i no>a    then F.null (*cul de sac =>cancel*)
    else if a<0 then F.setEstuaire flumen i 
    else if pt i no>= 75 then F.null (*latitude inlandsis =>cancel*)
    else if Ria.get wa i>0 then F.setConfluent flumen i (Ria.get wa i)
    else if no<>o && pa i no<a && (pr i o>0 || pr i (no-2)>0) then F.null (*descentes parallèles=>cancel*)
    else if pa i no<a (*&& no=nmo*) && pa i (no-1)<a && (pa (Rid.add (p pi no)(-1)) no-1)>=a 
         then f (Rid.add(p pi no)(-1)) pi nmo (no-1) nvp (flumen) (*recul pour virage*)
    else if pa i no<a (*&& no=nmo*) && pa i (no+1)<a && (pa (Rid.add (p pi no)(-1)) no-1)>=a 
         then f (Rid.add(p pi no)  1 ) pi nmo (no+1) nvp (flumen) (*recul pour virage*)
(*    else if pa i no<a && ((pa i (nmo-1)<a) ||| (pa i (nmo+1)<a)) then F.null (* problème graphique *)*)
    else if (pr i (o-1)>0 && pa i (o-1)<=a)  (* virage pour confluent *)
         then (if (pa i o)<a then F.null else let no=o-1 in f (p i no) i nmo no nvp (F.add flumen i (dir no) (p i no) )) 
    else if (pr i (o+1)>0 && pa i (o+1)<=a)
         then (if (pa i o)<a then F.null else let no=o+1 in f (p i no) i nmo no nvp (F.add flumen i (dir no) (p i no) ))
    else if (pr i (o-1)>0 || pr i (o+1)>0) then F.null (*confluent en remontant =>cancel*)
    else if a<=Ria.get aa pi then f (p i no) i nmo no nvp (F.add flumen i (dir no) (p i no) ) (*cours normal*)
    else F.null in
  let flumen = f i pi mo o vp flumen in
  if Flumen.len flumen<=4 then F.null else flumen
(* mise à jour du flumen en construction après être « avancé » d’une regio *)

let create (e:E.t) aa(*altitude array*) tla (*temp lat array*) =
  let s,mw,_mh = E.dimir e in
  let wa = Ria.make s 0 in (*river array : pour les flumenID*)
  let p  rid o = List.nth (ER.lesQuatre e rid) (o%4) in (*proxima, écrit ainsi pour l’efficacité*)
  let pa rid o = Ria.get aa  (p rid o) in (*proxima altitude*)
  let pr rid o = Ria.get wa  (p rid o) in (*proxima river*)
  let pt rid o = Ria.get tla (p rid o) in (*proxima temp*)
  let rec flumenListCreate fleuveID flumenList =  
    if fleuveID >= mw lsl 8 then flumenList
    else 
      let i = E.Cylinder.randomRid ~polarExclusion:(max 0.10 (Continentes.polarExclusion +. 0.03)) (E.resolution e) in
      let a = Ria.get aa i in
      if a >= 1 (*les sources sont sur un flanc de relief*)
      && Ria.get wa i=0 (*pas déjà un fleuve*)
      && List.for_all (fun pid->(Ria.get wa pid=0)) (ER.lesQuatre e i)(*ni autour*) 
      then
        let rec g n o = match n,o with (*recherche d’une orientation adéquate*) 
        | 4,_ -> (-1)
        | n,o -> if pa i o < a (*ça descend*)
                 && pa i (o+1)>=a && pa i (o+2)>=a && pa i (o+3)>=a (*ça n’aurait pas pu descendre ailleurs*)
            	 then o else g (n+1) ((o+1)%4) in
        let mo=g 0 (Random.int 4) in
        if mo>=0
        then let flumen = flumenCreate aa wa p pa pr pt (p i mo) i mo mo 0 (F.make fleuveID i (dir mo) (p i mo) ) in 
        let rewriteWa flumen = Tlist.iteri (fun _n regio ->(Ria.set wa regio.F.rid (flumen.F.fid))) flumen.F.cours in
        let flumenList = 
          if flumen<>F.null 
          then match flumen.F.fin with 
          | F.Estuaire  _    -> rewriteWa flumen ; flumen::flumenList
          | F.Confluent _rid -> rewriteWa flumen ; insert flumenList flumen
          | _ -> raise (Failure "FlumenList.create")
          else flumenList in flumenListCreate (fleuveID+1) flumenList
        else flumenListCreate (fleuveID+1) flumenList
      else flumenListCreate (fleuveID+1) flumenList in
  let flumenList = flumenListCreate 0 [] in
  flumenList, aa
(* génération d’une liste de fleuves + une carte d’altitude un peu modifiée (ravinement) *)


(* EOF *)
