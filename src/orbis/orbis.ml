(*

 ****************************** orbis.ml ******************************


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

(* orbis : un monde à un tour donné *)

open Std
(*open Abbrev.Orbis*)

module Nim = Nid.Nim
module Nil = Nid.Nil
module Nia = Nid.Nia
module E   = Espace
module SD  = Strategica.Data

type t = (* monde : ne change qu'en cas de end_of_turn *)
  {
  espace : E.t;
  regioMap : Rm.t;
(* données constantes du monde *)
  turn : Date.t;
  imperiumMap : Im.t;
  geographia : G.t;
  lucrum : Lucrum.t;
  natioIdList : Nid.t list;
  civitasList : CivitasList.t;
  natioList : NatioList.t;
  proximaeList : Proxima.t Nil.t Nil.t;
  junctiones : Junctiones.t;
  sd         : Strategica.Data.d;
  flexuraeList : Flexurae.t Nil.t;
(*  vetera : Vetera.t;*)
(* données variable du monde *)
  }

let espace   o = o.espace
let regioMap o = o.regioMap


let update orbis (sl:StrategicaList.t) =
  let o  = orbis in
  let rm = o.regioMap in
  let e  = o.espace in
  let j  = o.junctiones in
  let im = o.imperiumMap in
  let nil= o.natioIdList in
  let nl = o.natioList in
  let n_turn = Date.inc o.turn in
  let sd   = SD.make (sl:> Strategica.t Nid.Nil.t) in
  let n_j  = Junctiones.update j (NatioList.jNatioList nl) (SD.jStrategies sd) in
  let n_im, vl = Im.update e rm im n_j (NatioList.imNatioArray nl) in
  let cl   = CivitasList.update e n_turn n_im (o.civitasList) vl in
  (** Attention : l’im est mise à jour par effet de bord (regiones urbs) par CivitasList.update *)
  let n_g  = G.create  e rm n_im in
  let gnl  = G.natioList n_g nil in
  let jnl  = Junctiones.natioList n_j nil in
  let pnl  = NatioList.pNatioList nl in
  let pl   = Nil.map2 Partitio.compute pnl (Nil.map Strategica.stratiotikon (sl:> Strategica.t Nid.Nil.t)) in
  let rl   = Nil.map2 Partitio.Record.compute pl pnl in
  let luc  = Lucrum.compute n_g n_j nil rl in
  let pal  = Nil.map Proxima.proxArtes o.proximaeList in
  let inl  = Nil.map3 Natio.inventiones (nl :> Natio.t Nid.Nil.t) rl pal in
  let n_nl = NatioList.update gnl jnl cl nl rl luc pl inl in
  let n_pl = Nid.Nil.init (fun i -> Proxima.proximae n_nl (G.pil nil n_g i)) nil in
    {
    orbis with
    turn        = n_turn;
    junctiones  = n_j;
    imperiumMap = n_im;
    natioList   = n_nl;
    proximaeList= n_pl;
    geographia  = n_g;
    lucrum      = luc;
    civitasList = cl;
    sd          = sd;
    flexuraeList = Nid.Nil.map2 (Flexurae.update) (n_nl :> Natio.t Nil.t) o.flexuraeList ;
(*  vetera = Vetera.cat o.vetera n_vetera;*)
    }
(* mise à jour de l'humanité résultant de l'écoulement d'une année *)

let create forme size =
  let e  = E.create (E.Cylinder E.Octal) size in
  let rm = Rm.create  e in
  let imd= Imd.create e rm in
  let nil= imd.Imd.natioIdList in
  let nil= Nid.sort nil in
  let im = Im.create  e rm (imd.Imd.civMap) (imd.Imd.civCentersMap) in
  let g  = G.create e rm im in
  let cl_nil = CivitasList.create_nil (imd.Imd.civOrigoArray) nil in
  let nl = NatioList.create rm im g cl_nil in
  let j  = Junctiones.make in
  let im, vl = Im.update e rm im j (NatioList.imNatioArray nl) in
  let g    = G.create e rm im in
  let gnl  = G.natioList g nil in
  let jnl  = Junctiones.natioList j nil in
  let pnl  = NatioList.pNatioList nl in
  let pl   = Nil.mapi (fun nid n -> Partitio.primary          n)  pnl    in
  let rl   = Nil.map2 (fun n p   -> Partitio.Record.compute p n)  pnl pl in
  let luc  = Lucrum.compute g j nil rl in
  let cl   = CivitasList.create e (NatioList.origoList nl) vl in
  let nl   = NatioList.update gnl jnl cl nl rl luc pl (Nil.init (fun i -> []) nil) in
  let pl   = Nid.Nil.init (fun i -> Proxima.proximae nl (G.pil nil g i)) nil in
  let orbis = {
    espace      = e;
    regioMap    = rm;
    turn        = Date.beginning;
    imperiumMap = im;
    geographia  = g;
    lucrum      = luc;
    civitasList = cl;
    natioList   = nl;
    proximaeList= pl;
    junctiones  = j;
    natioIdList = nil;
    sd = SD.of_nil nil;
    flexuraeList = Nid.Nil.init (fun i -> Flexurae.make) nil ;
(*    vetera = [];*)
    }
  in orbis
(* creation ex nihilo d'un orbis *)
