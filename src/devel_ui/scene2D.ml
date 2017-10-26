(*

 ****************************** Scene2D.ml ******************************


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
module T  = Tabula
module E  = Espace
module ER = Espace.Regio
module RS = React.S
module Co = Color
module Ria = Rid.Array
module G = Scene.GeoRect

let latitudes =
  let nordLat = [
  (-80.  , 0.30);
  (-67.56, 0.30);
  (-60.  , 0.00);
  (-50.  ,-0.40);
  (-40.  , 0.10);
  (-30.  ,-0.50);
  (-23.44, 0.30);
  (-10.  ,-0.20);
  ]  in
  let sudLat = List.map (fun (lat,a) -> (-. lat,a)) nordLat in
  (  0.  , 0.40)  :: ( nordLat ^^ sudLat )


let yxrh = (* y/x ratio height *)
  let open Tfloat in
  (fun lat -> Espace.yxr (iof(abs lat) ++1 ))
(* (++1) : on arrondit au dessus le choix de la height de la regio pour éviter les vides dans l'affichage *)
(* les regiones ne sont pas carrées, sauf à l’équateur *)


let visibleCoords_of_v2 gr v2 = 
  let lat,lon = v2.V2.y, v2.V2.x in
  if G.is_visible gr (lat,lon) 
  then Some (lat,lon)
  else None
(* renvoie la latitude et la longitude pour un v2, si celui-ci est bien dans la zone à afficher *)


let pos_of_v2 gr pos v2 = match visibleCoords_of_v2 gr v2 with
  | None           -> None
  | Some (lat,lon) -> let x,y = pos v2 in Some (lat, lon, x, y)
(* pour un v2 donné, renvoie la latitude et la position x,y à l’écran, si le v2 est « dans le cadre » *)


let pos_of_rid e ria gr pos rid =
  let regio   = Rid.Array.get ria rid in 
  let lat,lon = ER.coords e regio.Tabula.rid in
  match G.visibleCoords gr (lat,lon) with (*visibles = contenues dans la scène*)
  | None -> None
  | Some (vlat,vlon) -> let x,y = pos (V2.make vlon vlat) in Some (lat, lon, x, y)
(* pour un rid donné, renvoie la latitude et la position x,y à l’écran, si la regio est « dans le cadre » *)


let qiter qtree gr f = 
  let ul = Some(V2.make gr.G.lonMin gr.G.latMin)
  and lr = Some(V2.make gr.G.lonMax gr.G.latMax) in
  Qtree.set_iter_rect ~ul ~lr f (qtree:> Tabula.regio Qtree.set)


let vorid e povOpt ria qiter =
  let open Tabula in
  let default = match povOpt with None -> Natio.Cognita | Some n -> Natio.Incognita in
  let ba   = Ria.make (Espace.sir e) default in
  let f n v2 regio = 
    let lat,lon     = v2.V2.y, v2.V2.x in 
    match Natio.regio n e lat lon regio.alt regio.coast regio.brouillards with
                | Natio.Incognita -> ()
                | Natio.Terra_incognita -> Ria.set ba regio.rid Natio.Terra_incognita
                | Natio.Cognita         -> Ria.set ba regio.rid Natio.Cognita in
  let _ = match povOpt with
  | None   -> ()
  | Some n -> qiter (fun v2 regio -> f n v2 regio) in
  (fun rid -> Ria.get ba rid )
(* caractère connu ou non d’une regio appelée par son rid *)
(* qiter = parcours de toutes les regiones à afficher, stockage des données calculées dans un array *)


let extremi_in_earth_mode rw sw earthMode rx px =
  let open Tfloat in
  let dw = 1.5 * rw in
  earthMode && (     ( rx < dw && px > (sw-dw) )
                  || ( px < dw && rx > (sw-dw) )  )  
(* contrôle si deux regiones sont au bord de l’écran et en earthMode *)


  
module Display = functor (Draw : Video.Draw) -> struct

module D = Draw
open Tfloat
let rsm  = RS.map
let rsv  = RS.value
let rsf  = rsv |- foi
let h  x = x * 0.5
let ip x = iof (arrondi x)  (*integer value position*)
let iw x = iof x ++ 1    (*integer value width*)

let forest   = D.load_bmp "forest"
let grass    = D.load_bmp "grass"
let mountain = D.load_bmp "mountain"
let wood     = D.load_bmp "wood"
let field    = D.load_bmp "field"
let civitas  = D.load_bmp "civitas"
let field_and_grass = D.load_bmp "field_and_grass2"
let field_and_wood  = D.load_bmp "field_and_wood2"


let regio_of_pos e scene gr (x,y) =
  let earthMode    = Scene.earthMode  scene in
  let crLat, crLon = ER.coords e (Scene.cr ~e scene) in
(*  let gr  = G.make (crLat, crLon) scene (rsv (D.screen_geometry)) in*)
  let wdip= rsf D.swip / (gr.G.lonMax - gr.G.lonMin) in (* largeur d’un degre de longitude en pixels *)
  let hdip= wdip * (rsv D.ycorratio) in (*** FIXME * ***)
  let res = E.resolution e in
  let wr  = let rw = E.Cylinder.regioWid res * wdip in foi (iw rw) / rw in
  let hswip = div (rsv D.swip) 2 in
  let rx  = 
    let rx = foi (x -- hswip) in
    if earthMode then rx else (rx - 0.5) / wr
  and ry  = foi y - h(rsf D.ship) in
  let lon = crLon + rx / wdip
  and aLat= crLat + ry / hdip in
  let lat = G.fittingLat crLat aLat aLat in (* ce qui permet de gérer le fait que les lat sont de hauteur variable en pixels *)
  let module C = Espace.Cylinder in
  let ix   = iof( (lon - E.lonMin) / C.regioWid res )
  and iy   = iof( (lat - E.latMin) / C.regioHid res ) in 
  let ix   = if x < hswip && E.legalLon lon > 0. && E.legalLon crLon < 0. then (print_endline "coucou"; ix -- 1) else ix in
  Some (C.srid_of_ryx res iy ix )
(** renvoie la regio située à la position (x,y) sur l'écran *)

 
let draw_lat laty aos lat a =
  let y  = iof(laty lat) in
  let co = Color.nil Color.Nuance.cyan 100 in
  let s  = iw (rsv D.ehip * 0.4)  in
  let a  = cut 0. 0.6 (a+aos) in
  if  a >= 0.2 then (
    D.line ~a:(cut 0. 0.4 (a+aos)) (co 320) 0 y (rsv D.swip) y ;
    D.strn ~size:s ~co:(co 640) ~xAlign:D.Left ~yAlign:D.Top (Strn.latitude lat) 0 y
    )
(* dessine une latitude pour un float (lat) et un float (alpha) *)

let draw_pays_names vorid por rw rh scene nl = 
  let f natio =
    let rid = fst (Natio.origo natio) in
    if vorid rid = Natio.Cognita then
    match por rid with None -> () | Some (lat,lon,x,y) -> 
      let nid = Natio.nid natio in
      let co = Color.nil (Ci.natio nid) 500 200 in
      let s  = iw (rsv D.ehip * (0.5 + 0.6 * Scene.ascale scene )) in
      D.strn ~size:s ~co:co  ~xAlign:D.Middle ~yAlign:D.Medium (Si.natioRegio nid) (ip x) (ip y) in
  NatioList.iter f nl
(* affichage des noms des pays *)

let draw_civitates vorid por rw rh scene cl = 
  let f civitas =
    let rid = Civitas.rid civitas in
    if vorid rid = Natio.Cognita then
    match por rid with None -> () | Some (lat,lon,x,y) -> 
      let inw = iw (rw / 12.) in (* largeur insula *)
      let ew  = ip (rw / 24.) in (* espace entre centre regio et insula *)
      let dw  = inw ++ ew in
      let nid = Civitas.civ civitas in
      let co = Color.nil (Ci.natio nid) 500 800 in
      let fill_rect = Draw.fill_rect ~a:0.8 co inw inw in
      let _ =
      fill_rect (iof x -- dw) (iof y -- dw) ;
      fill_rect (iof x -- dw) (iof y ++ ew) ;
      fill_rect (iof x ++ ew) (iof y -- dw) ;
      fill_rect (iof x ++ ew) (iof y ++ ew) in
      (* quatre insulae *)
      let s  = iw (rsv D.ehip * (0.4 + 0.4 * Scene.ascale scene )) in
      let name = Si.civitas (nid) (Civitas.nth civitas) in
      D.strn ~size:s ~co:co ~xAlign:D.Left ~yAlign:D.Top name (ip (x-h(rw))) (ip y ++ inw) in
      (* affichage des noms des cités *)
  CivitasList.iter f cl


let draw_selection por rw rh scene =
  match (Scene.sr scene) with
  | None -> ()
  | Some rid -> match por rid with None -> () | Some (lat,lon,x,y) ->
    Draw.rect ~a:0.6 Color.yellow 2 (iw rw) (iw (rh lat)) (ip (x-h(rw))) (ip (y-h(rh(abs lat / 1.1))))
  (* cadre lumineux désignant la regio sélectionnée *)


let texture ?(aMin=0.) a t x y i m rw rh aos =
  let d  = if m=0 then 0 else (((mult i i)mod m) -- (div m 2)) in
  let tw = 128 (*texture_width*) in
  let rw, rh = iw rw, iw rh in
  let sx = (div (tw--rw) 2) ++ d in
  let sy = (div (tw--rh) 2) ++ d in
  let fa = max aMin (aos * a) in
  if fa >= 0.05 (* pas de perte de temps à afficher quelque chose d'invisible *) 
  then Draw.fill_bmp ~a:fa ~sw:rw ~sh:rh ~sx ~sy t x y


let draw_terra_incognita rw rh x y =
  let open Tabula in
  let xMin, yMin  = ip(x - h rw), ip(y - h rh) in
  Draw.fill_rect   Tabula.Color.terra_incognita (iw rw) (iw rh) xMin yMin


let draw_cognita scene rw rh x y aos regio = 
  let open Tabula in
  let xMin, yMin  = ip(x - h rw), ip(y - h rh) in
  let texture ?aMin a t m = texture ~aMin:0. a t xMin yMin (regio.rid:>int) m rw rh aos in
  let filter = Scene.filter scene in
  let color = match filter, Scene.nations scene, Scene.altitude scene with
  |  _        , true , true-> regio.color.natAlt
  | `populatio, true , _   -> regio.color.popNat
  | `populatio, _    , true-> regio.color.popAlt
  | `populatio, _    , _   -> regio.color.populatio 
  | `montes   , true , _   -> regio.color.natAlt
  | `montes   , _    , _   -> regio.color.montes
  |  _        , true , _   -> regio.color.nationes
  | `politeia , _    , true-> regio.color.polAlt 
  | `politeia , _    , _   -> regio.color.politeia 
  | `imperii  , _    , true-> regio.color.impAlt
  | `imperii  , _    , _   -> regio.color.imperii
  | `nationes , _    , true-> regio.color.natAlt
  | `nationes , _    , _   -> regio.color.nationes
  | `tegmen   , _    , true-> regio.color.tegAlt
  | `tegmen   , _    , _   -> regio.color.tegmenF 
  | `artes    , _    , _   -> regio.color.artes 
  | `vis      , _    , true-> regio.color.visAlt 
  | `vis      , _    , _   -> regio.color.vis 
  in
  (
  let irw, irh = iw rw, iw rh in
  Draw.fill_rect color (irw) (irh) xMin yMin ;
  if filter = `tegmen && Scene.nations scene == false && Scene.altitude scene == false then 
    ( match regio.tegmen with
    | Rv.Turbs              
    | Rv.Irrigation 
    | Rv.Fields             ->  texture 0.15 field           13
    | Rv.Fields_and_pasture ->  texture 0.15 field_and_grass 19
    | Rv.Fields_and_woods   ->  texture 0.20 field_and_wood  19
    | Rv.Desertum _ -> (
    if regio.silva >= 58
      then texture  (min 0.60 (foi regio.silva / 100. * 0.60 )) forest 11
      else if regio.silva >=45
        then texture  (min 0.60 (foi regio.silva / 100. * 0.60 )) wood 11
      else if regio.silva >=13
        then texture  (min 0.20 (foi regio.silva /  30. * 0.25 )) grass  11;
    if regio.alt>=0 && regio.thermos < 0 then
        let a = min 0.4 (0.01 * foi(- regio.thermos)) in
        Draw.fill_rect ~a Color.snow (irw) (irh) xMin yMin )
    | _ -> () 
    ) ;
  if regio.mountain 
      then texture ~aMin:0.08 (foi regio.alt * 0.2 / 6.) mountain 0;
  ) 
(* draw_cognita *)

let draw_regio vor pow rw rh scene aos v2 regio =
  match pow v2 with None -> () | Some (lat, lon, x, y) ->
  let open Tabula in
  let rh          = rh lat in
  match vor regio with
              | Natio.Incognita -> ()
              | Natio.Terra_incognita -> draw_terra_incognita rw rh x y
              | Natio.Cognita         -> draw_cognita scene rw rh x y aos regio


let draw_flumen vor pow por rw exiem scene res v2 regio = 
  let open Tabula in
  match regio.hydros, vor regio with R.River river, Natio.Cognita -> (
      match pow v2 with None -> () | Some (lat, lon, rx, ry) ->
      match por (river.R.dest) with None -> () | Some (plat, plon, px, py) -> 
      if not (exiem rx px) then
      let w = Scene.fluwip scene (foi river.R.fluxus) in
      let a = min 1. (w * 0.15) in
(*      Draw.thick_line ~a (Co.yellow) (iof w) (ip rx) (ip ry) (ip px) (ip py)  )*)
      Draw.thick_line ~a (Tabula.Color.flumen) (iof w) (ip rx) (ip ry) (ip px) (ip py)  )
  | _ -> ()


let draw_borders vor pow por rw rh exiem v2 regio =
  match vor regio with Natio.Cognita -> (
      match pow v2 with None -> () | Some (lat, lon, rx, ry) ->
      let open Tabula in
      let hrw     = 0.5 *  rw in
      let hrh lat = 0.5 * (rh lat) in
      let draw_border prid =
        match por prid with None -> () | Some (plat, plon, px, py) -> 
           if not (exiem rx px) then
             if px=rx then let y = ip(barycenter ry py) in  Draw.line ~a:0.6 Color.border (ip(rx-hrw)) y (ip(rx+hrw)) y
        else if py=ry then let x = ip(barycenter rx px) in  Draw.line ~a:0.6 Color.border x (ip(ry-hrh plat+0.5)) x (ip(ry+hrh plat-0.5)) in
      List.iter draw_border regio.borders )
  | _ -> ()
(* frontières-lignes blanches des empires *)


 

let display_scene scene gr game player (ria, qtree) =
  let orbis = Game.orbis game in
  let e     = orbis.Orbis.espace in
  let povOpt  = NatioList.optGet (orbis.Orbis.natioList) (Game.Player.pov player) in
  let earthMode    = Scene.earthMode  scene in
  let crLat, crLon = ER.coords e (Scene.cr ~e scene) in
(*  let gr      = G.make (crLat, crLon) scene (rsv (D.screen_geometry)) in*)
  let wdip    = rsf D.swip / (gr.G.lonMax - gr.G.lonMin) in (* largeur d’un degre de longitude en pixels *)
  let hdip lat= wdip * (rsv D.ycorratio)  * (yxrh lat) in
  let res     = E.resolution e in
  let rw      = E.Cylinder.regioWid res *  wdip      in
  let rh lat  = E.Cylinder.regioHid res * (hdip lat) in (* hauteur exacte (float) de la regio en pixels *)
    (* en fullscreen, on affiche pile le bon nombre de regio à l’écran (c’est le but), quitte à ce que les regiones réelles se chevauchent (arrondi à l’int supérieurs); tandis que pour les autres zooms, on affiche des regiones de même taille, quitte à afficher un peu moins de regiones dans tout l’écran que prévu *)
  let aos = Scene.ascale scene in (* alpha of scale *)
  let laty lat = h(rsf D.ship) + wdip * (rsv D.ycorratio) * Espace.latdid crLat lat in (*** FIXME  ***)
(*    let laty lat = h(rsf D.ship) + wdip * Espace.latdid crLat lat in (*** FIXME  ***)*)
(*    let laty lat = h(rsf D.ship) + (wdip) * (rsv D.ehip) / (rsv D.ewip) * Espace.latdid crLat lat in (*** FIXME  ***)*)
(*    let laty lat = h(rsf D.ship) + (wdip) * Espace.latdid crLat lat in*)
    (* position en y sur l'écran d'une latitude *)
  let lonx lon =
    let wr = foi (iw rw) / rw in
    (* fonction permettant une égalité parfaite des largeurs des regiones (excepté en earthMode ou l'on privilegie la coincidence parfaite des largeurs de la terre et de l'écran *)
    let xr = (fun x -> if earthMode then x else x * wr + 0.5)  in
    (* le + 0.5 pixel nécessaire est à mon avis lié à l'imperfection de l'affichage des rectangles au pixel entier près *)
    (* pas de fonction similaire pour l'ordonnée vu que les regiones ont de tte façon des hauteurs var en fn de la latitude *)
      h(rsf D.swip) + xr( (lon - crLon) * wdip) in
    (* + wdip*0.5 parce que crLon n’est pas exactement au milieu de l’écran (puisque le nombre de regio est pair) *)
    (* position en x sur l’écran d'une longitude *)
  let pos v2 (*regio *) = lonx v2.V2.x, laty v2.V2.y in
    (* position exacte d’une regio en pixels (position du centre de la regio *)
    (* on utilise de préférence v2 et non regio, parce que v2 contient des longitudes "légales" c-a-d variables de -360° à +360° et en l’occurrence les bonnes : celles qui sont « réellement » affichées, tandis que la regio ne connait que ses coords absolues (-180° à +180°) *)
  (********************************* Applications partielles  en fonction des spécificités de la carte *********************************)
  let qiter = qiter qtree gr in
  let pow   = pos_of_v2 gr pos in
  let por   = pos_of_rid e ria gr pos in
  let vorid = vorid e povOpt ria qiter in (* visibility level of regio (cognita, etc.) *)
  let vor   = fun regio -> vorid (regio.Tabula.rid) in
  let exiem = extremi_in_earth_mode rw (rsf D.swip) earthMode in
  (************************************************************** AFFICHAGE *************************************************************)
  let _ = qiter (draw_regio  vor pow rw rh scene aos) in
  let _ = qiter (draw_flumen vor pow por rw exiem scene res) in
  let _ = if Scene.borders scene then
          qiter (draw_borders vor pow por rw rh exiem) in
  let _ = List.iter (fun (lat,a) -> draw_lat laty aos lat a) latitudes  in
  let _ = draw_pays_names vorid por rw rh scene orbis.Orbis.natioList in
  let _ = draw_civitates  vorid por rw rh scene orbis.Orbis.civitasList in
  let _ = draw_selection  por rw rh scene in
  let scale = Strn.percent (0) (Scene.ascale scene) in
  let wik   = Strn.float   (2) (Scene.wik    scene) in
  let _ = D.antiStrn ~xAlign:D.Right ~yAlign:D.Top ("wik="^wik^"("^scale^")") (rsv D.swip) (iof(rsv D.ehip)) in
   ()

end

(*EOF*)
