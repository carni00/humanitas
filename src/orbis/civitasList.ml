(*


 ****************************** CivitasList.ml ******************************


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
module Til = Tid.Til

type t = (Civitas.t Tid.t*Civitas.t) list
(* == Civitas.t Til.t, à ceci près que on a besoin du détail pour runner la fonction add *)

let get   t cid = Til.nth t cid



let search t rid = Til.search (fun cvt -> Civitas.rid cvt == rid) t
let filter t nid = Til.filter (fun cvt -> Civitas.incola cvt == nid) t




let iter f (cl:t) = Til.iter f cl


let test_vicus e (cl:t) rid inc =
    let nid= Rv.Incola.nid inc in
    let rec iter n = function
    | []        -> n
    | (i,c)::cq -> let n = n+iob(Civitas.civ c==nid) in
               let d = Espace.Regio.distance e rid (Civitas.rid c) in
               if n<4 && d>1500. then iter n cq else 4 in
    iter 0 (cl :> ('a*Civitas.t) list)
    (* tester si le vicus doit devenir ou non une cité *)

type origo =
| Date of Date.t
| OrigoList of ((Rid.t * Date.t) Nid.Nil.t)

let rec add e origo (cl:t) vl = match vl with
  | []            -> cl
  | (rid,inc)::vq -> let n=test_vicus e cl rid inc in
                     if  n==4 then add e origo cl vq (*echec*)
                     else
                     let o = match origo with 
                     | OrigoList ol -> ( match Nid.Nil.nth ol (Rv.Incola.nid inc) with rid,date -> Civitas.Post date )
                     | Date date -> Civitas.Equal date in
                     let c=Civitas.create rid (Espace.Regio.superficie e rid) inc n o in add e origo (Til.add cl c) vq
    (* ajoute des civitas à cl à partir d’une liste de vicus candidat *)


let create e  ol  vl = (add e (OrigoList ol) Til.empty vl)



let update e turn im cl vl =
  let f cvt =
    let rid = Civitas.rid cvt in match Im.incola im rid with 
    | None        -> print_endline "CivitasList.update : une cité sans incola ???"; cvt
    | Some incola -> Civitas.update cvt (Espace.Regio.superficie e rid) incola in
  let cl = Til.map f cl in (* mise à jour cités existantes *)
  let cl = add e (Date turn) cl vl in (* ajout nouvelles cités *)
  Im.set_oikos_urbs im (List.map (fun (cid,c) -> Civitas.rid c) cl) ; cl

(******************************************************************************************************************************)


let create_nil coa nil =
  let module Nil = Nid.Nil in
  let f (nid:Nid.t) = 
    let r,o =  Nid.Nia.get coa nid in
    Civitas.make_from_imd o r in
  Nil.init f nil 
(** lors de la création du monde, création artificielle de toutes les cités pour transmettre à la natio son origine (emplacement et date *)
 


