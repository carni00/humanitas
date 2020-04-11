(*

 ****************************** geoRect.ml ******************************


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
open Humanitas_physis
(*open Std*)
module E = Espace
 
type t =
  {
  lonMin : float;
  lonMax : float;
  latMin : float;
  latMax : float;
  }

(*
let make (crLat, crLon) scene screen =
  let open Tfloat in
  let module S = Scene in
  let d  = S.dim scene (Screen.ratio screen) in
  {
  lonMin = crLon - d.S.wid/2. ;
  lonMax = crLon + d.S.wid/2. ;
  latMin = crLat - d.S.hid/2. ;
  latMax = crLat + d.S.hid/2. ;
  }
*)
(* "rectangle" terrestre, en degrés de lon/lat, à afficher sur la surface-écran disponible *)


let is_visible gr (lat,lon) = lon>gr.lonMin && lon<gr.lonMax && lat >gr.latMin && lat<gr.latMax 


let visibleCoords gr (lat,lon) =
  let open Tfloat in
  let f p min max x =
    let nx = if x<min then x+p else if x>max then x-p else x in
    if nx>min && nx<max then Some nx else None in
  match (f (E.hid) gr.latMin gr.latMax lat) , (f (E.wid) gr.lonMin gr.lonMax lon) with
  | Some lat, Some lon -> Some (lat,lon)
  | _ -> None
(* s’assure que la position donnée se trouve dans le rectangle à afficher ; réalise les modulos nécessaires pour l’aspect cylindrique ; renvoie None si la regio n’est pas dans le rectangle à afficher *)

 
