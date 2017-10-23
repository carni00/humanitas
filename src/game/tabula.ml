(*

 ****************************** tabula.ml ******************************


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
module E=Espace
module N=Color.Nuance
module Rvi=Rv.Incola

type filter = [
| `imperii
| `artes
| `montes
| `nationes
| `politeia
| `populatio
| `tegmen
| `vis
  ]

type color = {
  tegmenF  : Color.t;
  tegAlt   : Color.t;
  imperii  : Color.t;
  impAlt   : Color.t;
  nationes : Color.t;
  natAlt   : Color.t;
  politeia : Color.t;
  polAlt   : Color.t;
  populatio: Color.t;
  popAlt   : Color.t;
  popNat   : Color.t;
  vis      : Color.t;
  visAlt   : Color.t;
  montes   : Color.t;
  artes    : Color.t;
  }


type regio = {
  rid : Rid.t;
  alt : int;
  coast : bool;
  thermos : int;
  hydros : R.hydros;
  silva  : int;
(*  gAlt : float ;*)
(*  pAlt : float ;*)
  mountain : bool ;
  tegmen : Rv.tegmen ;
  color : color ;
  borders : Rid.t list; (*liste des regiones entre lesquelles et la présente regio il y a une frontière interimpériale *)
  brouillards : Rv.Brouillards.t;
}

type t = 
| Qtree  of ((regio Rid.Array.t) *  (regio Qtree.set) )
(*| Octree of (regio Octree.set * regio Rid.Array.t) *)


(************************** REGIO COLOR *******************************)

module Color = struct
  type t = Color.t


  let climaxNuance t h = 
    let open Tfloat in
    let tList = [      -32.;     -24.;    -16.;    -8.;     0.;      8.;   16.;     24.] in
    let nList = [ N.celadon; N.amande; N.pomme; N.vert; N.lime; N.jaune; N.ble; N.ambre] in
  (*  nuances théoriques pour une hugros nulle *)
    let hList = [  50.;         250.;       500.;      1000.;     2000.] in
    let dList = Tlist.init 5 (fun n-> (foi n) * 0.75) in
  (* décalage en nuance résultant d’hugros *)
    let tNuance = swy tList (nList:>float list) (foi t) in
    let hDelta  = swy hList  dList (foi h) in
    N.cut N.celadon N.ambre (N.custom (tNuance - hDelta))
  (* nuance du climax comprise entre celadon (froid) et ambre (chaud et hyperaride) *)
  
  (* nuance de l’altitude *)
  
  let ariditasIntens a =
    let aList = [    0;    5;   10;   20;  30;  40] in (* ariditas (0=hyperaride) *)
    let iList = [  140;  110;   80;   50;  20;   0] in (* perte d’intensité *)
    Ext.swy aList iList a
  
  let iDef = 130 (* intensité par défaut*)
  let lDef = 380
  let co(a,b,c) = Color.nil a b c

  let tLumin t = ((foi(max 0 (20-t)))**(1.5)) *. 0.4
  
  let ocean ?(a=0) ?(t=15) () = co(N.ocean, iDef+600, lDef-160 + iof(1.38**(foi(12+a))) + iof(tLumin t) ) 
  
  let incognita       = ocean ~a:(-4) ()
  let terra_incognita = Color.gray (lDef-200)

  let snow = Color.white

  let seaIce ?(t=15) () = co(N.ocean, iDef+100, lDef-160 + iof(tLumin t *. 2.0) )

  let coast = co (N.ambre, 160, lDef+60)

  let oceanFond alt = match alt with
  | (-1) -> co(N.ambre, 100, lDef*90/100)
  | (-2) -> co(N.ambre,  50, lDef*20/100)
  |  _   -> co(N.ambre,   0, 0) 
  
  let flumen = co(N.custom(1.81), iDef+500, lDef-120 )
  
  let border = Color.gray 600
 
  let constantColor co =
    {
    tegmenF  = co;
    tegAlt   = co;
    montes   = co;
    imperii  = co;
    impAlt   = co;
    nationes = co;
    natAlt   = co;
    politeia = co;
    polAlt   = co;
    populatio= co;
    popAlt   = co;
    popNat   = co;
    vis      = co;
    visAlt   = co;
    artes    = co;
    }

  let regio rs r rv artes politeia vis =
    let alt     = R.alt r in
    let thermos = R.thermos r in
    match R.hydros r with
    | R.Ocean 
    | R.Sea      -> constantColor (ocean  ~a:alt ~t:thermos () )
    | R.SeaIce   -> constantColor (seaIce        ~t:thermos () )
    | R.Inlandsis-> constantColor (co(N.turquoise, iDef, lDef + 200))
    | _->let hugros = R.hugros r in
         let tegLum = let s=Rv.silva ~rv r in 80 - 3*s in
         let altitude = R.altitude r in
         let altLum = (Ext.weighmean (4*alt) ((min 4400 altitude)/100) 0 1) * 8 - 50 in 
         let dominus= Rv.dominus rv in
         let incola, populatio, urbs = match Rv.contents rv with
         | Rv.Desertum_for n -> Nid.none, 0., 0
         | Rv.Incol   incola -> 
           let populatio = max 0. (1. +. log ((Rvi.plebs incola) /. (rs))) in
           let urbs      = if Rvi.oikos incola=Rvi.Urbs then 0 else 0 in
           (Rvi.nid incola, populatio, urbs) in
         let incLum = if incola=Nid.none then (-40) else 0 in
         let nuance   = function
         | `imperii  -> Ci.natio dominus 
         | `nationes -> Ci.natio incola 
         | `vis      -> N.custom           (2.  *. vis)
         | `populatio-> N.add    N.celadon (1.  *. populatio)
         | `politeia -> Ci.politeia politeia
         | _         -> if boi urbs then N.orange else climaxNuance thermos hugros in
         let intens   = function 
         | `imperii  -> (if dominus<>Nid.none then (if incola=dominus then 500 else 300) else 0)
         | `politeia 
         | `nationes -> (if incola=Nid.none then 0   else 400)
         | `vis      -> (if incola=Nid.none then 0   else 300 + iof (vis       *. 100.))
         | `populatio-> (                                 400 + iof (populatio *. 100.))
         | _         ->  iDef + 100 - (ariditasIntens(R.ariditas r)) - urbs in
         let lumina    l = min 640 (lDef + urbs + l ) in
         let color n   l = co(nuance n, intens n, lumina l) in
         let colos n i l = co(nuance n, intens i, lumina l) in
             {
             tegmenF   = color `tegmen                (tegLum+altLum/4);
             tegAlt    = color `tegmen                (altLum+tegLum/2);
             imperii   = color `imperii               (tegLum/4 + incLum);
             impAlt    = color `imperii               (altLum/2 + incLum);
             nationes  = color `nationes              (tegLum/4 + incLum);
             natAlt    = color `nationes              (altLum/2 + incLum);
             politeia  = color `politeia              (tegLum/4 - 40);
             polAlt    = color `politeia              (altLum/2);
             populatio = color `populatio             (iof (populatio *. 10.) - 180);
             popAlt    = colos `populatio  `populatio (altLum/2 - 40);
             popNat    = colos `nationes   `populatio (iof (populatio *. 20.) - 180);
             vis       = color `vis                   (tegLum/4);
             visAlt    = color `vis                   (altLum/2);
             montes = ( match alt with
                    | 0 -> co(N.amande   , iDef+90, lDef - 40 + urbs) (*    0 *)
                    | 1 -> co(N.vert     , iDef+20, lDef +  0 + urbs) (*  400 *)
                    | 2 -> co(N.jaune    , iDef+20, lDef + 20 + urbs) (* 1000 *)
                    | 3 -> co(N.ambre    , iDef+60, lDef + 90 + urbs) (* 1600 *)
                    | 4 -> co(N.corail   , iDef+40, lDef +150 + urbs) (* 2400 *)
                    | 5 -> co(N.cerise   , iDef+20, lDef +210 + urbs) (* 3200 *)
                    | _ -> co(N.violine  , iDef   , lDef +280 + urbs) (* 4000 *));
             artes = Color.of_rvb (Ars.to_rvb artes);
             }

  (* la regioFun n’est calculé qu’une fois, avant l’affichage ; elle donne la couleur pour tous les filtres*)
end

(*******************************************************************************************)
(*
let seaLevel = 0.000 (*pas touche*)
let pga (* plain graphical altitude *) =  0.030 (*optimisation pente plage*)
let rbd (* river basic depth *)        =  0.020
let rsl (* river surface level *)      =  0.004 (*en dessous, trop de rivières disparaissent trop facilement avec
le recul en zoom, ou en cas de canyon (même léger) *)
let sma (* sea maximum altitude *)     =(-0.010)
let ema (* estuaire maximum altitude *)=(-0.0002) (*sous le niveau de la mer pour que la mer atteigne le centre de
l’estuaire == dernière case terrestre du fleuve*)
let fmd (* flumen maximum depth *)     =  pga -. rbd

let ga a =
  let rec g a = match a with
  | 0 -> pga
  | a -> g (a-1) +. 0.040 +. (foi a) *. 0.120 in
  let t = Array.init (altMax+1) g in (*EFF*)
  t.(a)
(* montée progressive en alt, mais plus douce (plateau à 400 m exagérés)*)

let gAltitude r =
  let flumenDepth river = min fmd ((foi river) *. 0.0001 *. 0.0020) in
  match r.alt with
  | a when a<(-3) -> (-0.2500)
  | (-3)-> (-0.2000)
  | (-2)-> (-0.1500) (*transition brutale entre (-1) et (-2) : tallus continental*)
  | (-1)-> (sma -. (foi(Random.int 11)) *. 0.0020 ) (*pour avoir des côtes courbes*)
  |   0 when r.coast && r.river>0 -> cut (sma *. 0.2) (ema) (ema -. flumenDepth r.river) (*estuaire*)
  |   a -> ga a    (*altitude*)
        -. (boi r.river => rbd +. flumenDepth r.river) (0.) (*lit du fleuve*)

let wAltitude gAlt = max seaLevel (gAlt +. rsl)
(* lit du fleuve + rsl sauf si océan =>niveau de la mer*)
*)
(**********************************************************************************************************************)
let regio game rid =
  let orbis = Game.orbis game in
  let e  = Orbis.espace orbis in
  let rm = Orbis.regioMap orbis in
  let im = orbis.Orbis.imperiumMap in
  let nl = orbis.Orbis.natioList in
  let rs = E.rs e in
  let r = Rm.get rm rid
  and rv = Im.get im rid in
  let dominus = Im.dominus im rid in
  let artes, politeia, vis = match Im.incola im rid with
  | None        -> [], Politeia.anarchy, 0.
  | Some incola -> let n = NatioList.get nl (Rvi.nid incola) in
    let artes    = Natio.artes    n  in
    let vis      = Natio.vis      n  in
    let politeia = Natio.politeia n  in
    artes, politeia, vis in
      { 
      rid = rid ;
      alt = R.alt r;
      coast = R.coast r;
      thermos = R.thermos r;
      hydros = R.hydros r;
      silva  = Rv.silva r;
      mountain = R.mountain r;
      tegmen   = Rv.tegmen ~rv r ;
      color   = Color.regio rs r rv artes politeia vis;
      borders = List.filter (fun rid -> Im.dominus im rid <> dominus) (E.Regio.proximae e rid);
      brouillards = Rv.brouillards rv;
      }
 
let make_ria game = 
  let sir = E.sir (Orbis.espace (Game.orbis game)) in
  Rid.Array.init sir (regio game)


let make_qtree game = 
  let open Tfloat in
  let empty  = Qtree.empty ~ul:(V2.make (E.lonMin * 2.) E.latMin) ~lr:(V2.make (E.lonMax * 2.) E.latMax) in
  let e      = Orbis.espace (Game.orbis game) in
  let sir    = E.sir e in (* map size in regiones *)
  let leafSir= iof((foi sir)**0.5) in (* leaf size in regiones *)
  let double_insert qt i = 
    let rid      = Rid.oi i in
    let lat, lon = E.Regio.coords e rid in
    let regio    = regio game rid in
    let insert qt aLon =
      let regioPos = V2.make aLon lat in (*Qtree veut des V2 ; on met lon en x, lat en y *)
      Qtree.insert leafSir regioPos regio qt in
    let lonBis = if lon > 0. then (lon - E.wid) else (lon + E.wid) in
    let qt = insert qt lon    in
    let qt = insert qt lonBis in qt in
    (* double insert : on construit une map qui va de -360° à +360° en longitude (deux fois plus large que la
    normale)*)
  (Ext.fold_left double_insert empty sir)

let make game =
  let ria = make_ria game in
  let qtree = make_qtree game in
  Qtree (ria, qtree)

let get tabula rid =
  match tabula with
  | Qtree (ria, qtree) -> Rid.Array.get ria rid
(* obtenir une regio d’une tabula *)

(*eof*)
