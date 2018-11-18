(*

 ****************************** Natio_list.ml ******************************


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
(*open Pseudo_float*)

type nid = Nid.t
type natio = Natio.t
module N = Natio

type t = natio Nid.Nil.t

module Nil = Nid.Nil
module Nia = Nid.Nia

let get nl i = Nil.nth nl i 
(* nth et non snth = toutes les nationes, même inactives, sont présentes *)
let optGet nl i = if i == Nid.none then None else Some (get nl i)

let iter f (nl:t) = Nid.Nil.iter f nl

let nf f nl id = f(get nl id)
(*apelle une Natio function sur la natio déterminée par une nl et un nid*)

let length nl = List.length nl

let nil nl = fst (List.split nl)
(* associe à la liste des natio la liste des Natio_Id *)

let create rm im g cl = Nil.mapi (N.create rm im g) cl
(*creation de la nl à partir de la cl*)

(*let proximaList g nl i =*)
(*  Nil.init (fun i->nf N.proxima nl i) pil*)
(* ce que l’on sait des proximae *)


let update gnl jnl cl nl fl luc pl inl =
  let f g j n p pa =
    let nid = Natio.nid n in
    let ncl = CivitasList.filter cl nid in
    N.update g j ncl n (Nid.Nil.nth fl nid) (Lucrum.lucrum luc nid) p pa in
  Nil.map5 f gnl jnl nl pl inl


let politeia = nf N.politeia
let artes    = nf N.artes
let vis      = nf N.vis
let pNatio   = nf N.pNatio
let origo    = nf N.origo
let urbsRid  = nf N.urbsRid

let pNatioList nl   = Nil.map N.pNatio nl
let jNatioList nl   = Nil.map N.jNatio nl
let origoList nl    = Nil.map N.origo nl
let imNatioArray nl = Nia.map_of_nil N.null N.imNatio nl
let instArray nl    = Nia.map_of_nil N.null N.instrumentum nl

(*let map  x = Nil.smap2 N.null N.null x*)

(*let map2 x = Nil.smap2 N.null N.null x*)
(* permet aux autres modules de parcourir deux t comme deux natio list *)
(* used par Vetera.politics et Vetera.inventiones *)

(* stats mondiales *)
open Tfloat
let sum  nf nl = Nil.fold_left (fun s n -> s + nf n) 0. nl
let mean nf nl = sum nf nl / (foi (Nil.len nl))

let plebs          nl = sum  N.plebs nl
let instrumentum   nl = mean N.instrumentum nl
let sophia         nl = mean N.sophia      nl
let fides          nl = mean N.fides       nl

(*eof*)
