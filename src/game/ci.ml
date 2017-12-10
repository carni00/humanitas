(*

 ****************************** Color Identity.ml ******************************


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

(*module W   = WindowID*)
(*module Ws  = Windows*)
module Co  = Color
module N   = Color.Nuance
module Rvi = Rv.Incola

(* couleurs des différents éléments du jeu *)
(* ce module les calcule une fois pour toutes, et permet d'y accéder facilement *)

(*let nil (n,i,l) = Co.Nil.make (n,i,l) (* préférer cette fonction, la couleur sous forme de nil peut être alors*)
(*modulé avant l’affichage *)*)
(*let rvb nil = Co.Rvb.of_nil (Co.Nil.make nil)*)
(*let co  nil = Co.of_nil(Co.Nil.make nil)*)

let nil = Co.nil

(************************* HUMANITAS COLORS ***************************)

let dominium rv = match Rv.contents rv with
  | Rv.Desertum_for n -> N.celadon
  | Rv.Incol incola -> match Rv.Incola.dominium incola with
    | Rvi.Mir          -> N.vert
    | Rvi.Latifundium  -> N.jaune
    | Rvi.Minifundium  -> N.orange



let politeia (p:Politeia.t) = 
  let open Politeia in
  match dominus p, arkhe p with
  | Demos, Anarchy   -> N.add N.rouge (-0.5)
  | Demos, Council   -> N.add N.rouge ( 0.5)
  | Aristoi, Anarchy -> N.violet
  | Aristoi, Council -> N.bleu
  | Aristoi, Monarch -> N.turquoise
  | _                -> N.vert

(*
let ars ars =
  let list = [
  (Ars.MET, N.rouge);  
  (Ars.WRI, N.magenta); 
  (Ars.GUN, N.violet); 
  (Ars.NAV, N.bleu); 
  (Ars.N_ARS, N.ocean); 
  (Ars.STE, N.celadon); 
  (Ars.CMB, N.emeraude); 
  (Ars.ELE, N.jaune);  ] in List.assoc ars list
*)

let natio (nid:Nid.t) = 
  let tab = [|
    Color.Nuance.celadon  ; (* 0:None     *)           
    Color.Nuance.jaune    ; (* 1:Egyptian *)             
    Color.Nuance.vert     ; (* 2:Akkadian *)             
    Color.Nuance.indigo   ; (* 3:Indian   *)           
    Color.Nuance.corail   ; (* 4:Assyrian *)             
    Color.Nuance.magenta  ; (* 5:Greek    *)          
    Color.Nuance.ocean    ; (* 6:Persian  *)            
    Color.Nuance.orange   ; (* 7:Punic    *)          
    Color.Nuance.amande   ; (* 8:Celt     *)         
    Color.Nuance.cerise   ; (* 9:Roman    *)          
    Color.Nuance.rouge    ; (*10:Han      *)        
    Color.Nuance.violet   ; (*11:Ethiopian*)              
    Color.Nuance.ble      ; (*12:Maya     *)         
    Color.Nuance.bleu     ; (*13:Viking   *)           
    Color.Nuance.cyan     ; (*14:Khmer    *)          
    Color.Nuance.lime     ; (*15:Songhai  *)            
    Color.Nuance.turquoise; (*16:Uzbek    *)          
    Color.Nuance.ambre    ; (*17:Aztec    *)          
    Color.Nuance.violine  ; (*18:Inca     *)         
    Color.Nuance.pomme    ; (*19:Zoulou   *)           
  |] in Array.get tab (Nid.ti nid)

(* 20 nationes = 20 nuances; *)
(*let natio (nid:Nid.t) = N.od(nid:>int)*)
(*let natio (nid:Nid.t) = Co.Nuance.custom(Nid.tf nid)*)


let affectatio ?(i=600) ?(lum=600) aff = match aff with
  | Partitio.LAB -> Color.nil  N.corail       i lum                 
  | Partitio.SAP -> Color.nil  N.turquoise    i lum                 
  | Partitio.MIL -> Color.nil  N.cerise       i lum             
  | Partitio.OPP -> Color.nil  N.indigo       i lum             
  | Partitio.REL -> Color.nil  N.pomme        i lum             
  | Partitio.LUX -> Color.nil  N.ble          i lum             
  | Partitio.OTI -> Color.nil  N.none         0 lum             


let natioKey ?(i=900) ?(lum=500) (key : Natio.key) = match key with
  | Natio.Facultas        -> Color.nil  N.jaune         i lum
  | Natio.Plebs           -> Color.nil  N.orange        i lum
  | Natio.Hospitalitas    -> Color.nil  N.vert          i lum
  | Natio.Instrumentum    -> Color.nil  N.corail        i lum
  | Natio.Efficientia     -> Color.nil  N.jaune         i lum
  | Natio.Famine          -> Color.nil  N.none          0 lum
  | Natio.Copia           -> Color.nil  N.ble           i lum
  | Natio.Tfg             -> Color.nil  N.magenta       i lum
  | Natio.Isf             -> Color.nil  N.violet        i lum
  | Natio.DxVar           -> Color.nil  N.rouge         i lum
  | Natio.Sophia          -> Color.nil  N.turquoise     i lum
  | Natio.Fides           -> Color.nil  N.pomme         i lum
  | _                     -> Color.nil  N.none          0 lum
 



(************************* GAME COLORS ***************************)
let font             = nil N.ocean      200  720 
let focus            = nil N.jaune      900  700 

(*let wBackground wStatus =
  match wStatus with
  | W.Active -> co(N.turquoise,    700, 232)
  | _        -> co(N.custom(2.3), 1000, 240)
*)
let wsb = nil  N.turquoise     700  232  (*window std background*)
(*let wgb = nil  N.celadon         0  232  (*window gray background*)*)
let wib = nil (N.custom 2.3)  1000  240  (*window inactive bg *)

let bsl = nil  N.pomme        800  400  (*button std limit*)
let bpl = nil  N.vert         800  500  (*button pressed limit*)
let bsb = nil  N.pomme        800  250  (*button std background*)
let bpb = nil  N.vert         800  180  (*button pressed background*)


(*let wScrollBar  = co(N.ocean,  1000, 340)*)

(*let bBackground alert ld =
  let n = match alert with
  | Ws.Awake -> N.amande
  | Ws.Numb  -> N.celadon
  | Ws.Asleep-> N.ocean in
  co(n, 600, 200+ld)*)

(*let natioColumn nid = nil(natio nid, 500, 500)*)


