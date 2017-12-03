(*

 ****************************** Lucrum.ml ******************************


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

module Nil = Nid.Nil
module Nim = Nid.Nim
module P   = Partitio

type t = {
  factumList  : Partitio.t Nid.Nil.t; (* produits réels des nationes *) 
  damnumMap   : Partitio.t Nid.Nim.t;  (* pertes et tributs donnés, par incola(donneur), puis dominus(receveur) *)
  damnumList  : Partitio.t Nid.Nil.t; (* pertes et tributs donnés, par incola *)
  tributumMap : Partitio.t Nid.Nim.t;  (* tributs réels, par dominus, puis incola *)
  tributumList: Partitio.t Nid.Nil.t; (* tributs réels, par dominus *)
  lucrumList  : Partitio.t Nid.Nil.t; (* revenus réels *)
  }

let null = {
  factumList  = Nil.empty; 
  damnumMap   = Nim.empty;
  damnumList  = Nil.empty; 
  tributumMap = Nim.empty;
  tributumList= Nil.empty; 
  lucrumList  = Nil.empty;
  }
  


let compute g j nil fl (*pl nl*) =
(*  let factumList = Nil.map2 (fun nid p n -> Partitio.factum p n) pl nl in*)
  let factumList = Nil.mapi (fun nid fru -> Partitio.Record.factum fru) fl in
  
  let damnumFun inc dom =
    let chora = G.chora g inc in
    let funus = G.fines g inc dom in (* la zone occupée de *inc, par *dom *)
    let relatio = Junctiones.relatio j inc dom in
    P.damnum_of_factum (Nil.nth factumList inc) (funus, chora) relatio in
    (* le dommage (partitio) subi par *inc du fait de *dom *)

  let damnumMap = Nim.init (fun i j -> damnumFun i j) nil in
  let damnumList= Nil.mapi (fun y l -> P.listSum l) (Nim.to_ll damnumMap) in
(*  let tributumMap = Nim.sym (Nim.nfilter (fun (y,x)->(Junctiones.relatio j y x=Junctiones.Pax)) damnumMap) in*)
  
  let tributumFun inc dom damnum =
    let relatio = Junctiones.relatio j inc dom in
    (P.tributum_of_damnum damnum relatio)  in
(* le tribut prélevé par l’occupant est une partie du damnum dépendante de la situation de paix/guerre *)

  let tributumMap = Nim.sym_mapi tributumFun damnumMap in
(* la map des tributs est la map symétrique des dommages, corrigée par la tributumFun *)


  let tributumList= Nil.init (fun y -> (P.listSum (Nim.line tributumMap y))) nil in

  let lucrumList = Nil.map3 (fun fac dam tri -> P.alter fac [ P.SBS_P dam ; P.ADD_P tri ] ) factumList damnumList tributumList in
  {
  factumList  = factumList; 
  damnumMap   = damnumMap;
  tributumMap = tributumMap;
  damnumList  = damnumList; 
  tributumList= tributumList; 
  lucrumList  = lucrumList
  }


let damnum f inc dom = Nim.nth  f.damnumMap  inc dom
(* fonctionnel, snth non nécessaire puisqu’on interroge des nations existantes dans la map *)

let damSum f inc     = Nil.nth  f.damnumList inc 
(* déclenche un plantage à la sélection de pov national au 1er tour, parce que fructus pas créé *)
(*let damSum f inc     = Nil.snth Partitio.null f.damnumList inc*)

let tributum f inc dom = Nim.snth Partitio.null f.tributumMap  inc dom
let tribuSum f inc     = Nil.snth Partitio.null f.tributumList inc 

let factum   f inc = Nil.nth f.factumList inc
let lucrum   f inc = Nil.nth f.lucrumList inc

