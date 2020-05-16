(*

 ****************************** src/game/scene.ml ******************************


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
open Humanitas_orbis
open Std

module E = Espace
module ED = Espace.Direction

type forme =
| Plan
| Sphere
(* La forme de la scène n’est pas la forme de l’espace : on peut souhaiter afficher la sphere en plan *)

type options = {
  altitude : bool;
  borders  : bool;
  nations  : bool;
  }

type natio = {
  urbsRid   : Rid.t;
  urbsLat   : float;
  urbsLon   : float;
  has_nav   : bool;
  ter_rayon : float;
  mer_rayon : float;
}


type t = {
  forme     : forme;
  angle     : Level.t;
  scale     : Level.t;
  earthMode : bool; (*affichage optimal de la Terre sur tte la surface d’affichage*)
  filter    : Tabula.filter;
  options   : options;
  cr        : Rid.t;   (*centered_regio*)
  sr        : Rid.t option; (*selected_regio*) 
  natio     : natio option; (*éventuelle limitation de la scène visible aux connaissances géo d’une natio *)
  }

let filter     s = s.filter
(*let scale      s = s.scale*)
(*let angle      s = s.angle*)
let forme      s = s.forme     
let earthMode  s = s.earthMode 
let sr         s = s.sr
let altitude   s = s.options.altitude
let borders    s = s.options.borders
let nations    s = s.options.nations

let cr     ?e  s = match e, forme s, earthMode  s with
  | Some e, Plan, true -> 
    let _y, x = E.Cylinder.yx_of_rrid (E.resolution e) (s.cr) in
    E.Cylinder.rid_of_ryx (E.resolution e) (E.hir e /2) x
  | None  , Plan, true -> raise (Failure "Scene.cr : veuillez précisez l’espace* de travail")
  | _                  -> s.cr
(* centered regio : en mode Scene.Plan && Scene.earthMode , la latitude centrée est bloquée à zéro (équateur) *)

let filterList = [ `montes; `nationes; `tegmen]

(************************************ MODULE SCALE ***********************************)
module Scale = struct
(*  type t = Level.t*)
  let ratio     = 1.333
  let altMin    =  3000. (*  3000 km de la surface de la terre (niveau de la mer) *)
  let altMax    = 15000.
  let int       = Level.to_int
  let min     c = Level.lower_bound c
  let max     c = Level.upper_bound c

  open Tfloat
  let alt     c = altMin * (ratio ** (foi(max c -- int c)))
  let wik     c = alt c * 2. (*approximation*)
  let make   = 
      let nb      = iof (log (altMax/altMin) / log ratio) in (* nb d’échelles différentes disponibles *)
      let min     = 1 in            (* échelle minimum == altitude maximum *)
      let max     = min ++ nb in     (* échelle maximum == altitude minimum *)
      Level.smake min (min++2) max 

  let extent  c = max c
  let range   c = int c
  let incr    c = Level.incr  c
  let decr    c = (Level.decr  c)
  let set   c v = (Level.set c v) (*Level.set is secured*)
  let set_min c = (set c  (min c))

end
(**************************************************************************************)

let ascale     scene = 
  if earthMode  scene then 0.
  else let s = scene.scale in foi (Scale.int s) /. foi (Scale.max s)
(* actual/apparente scale *)

let wik scene =
  if earthMode scene then 40000.
  else Scale.wik (scene.scale)

let alphaMax = 90. 
let alphaMin = 40.

let alpha (s:t) =
  let r = foi(Scale.range s.scale) in
  let e = foi(Scale.extent s.scale) in
  let open Tfloat in 
  let angleExtent = foi (Level.extent s.angle) in
  let ar = ( (alphaMax / alphaMin) - 1. ) / angleExtent in (*alpha ratio*)
  let a = angleExtent - foi(Level.range s.angle) in
  (90. / ( 1. + (ar * (a*r/e) )))
(** angle effectif de la camera pour une scene (angle et scale) donnée *)


let altdip (s:t) alt = 
  if earthMode  s then 0.
  else Tfloat.(foi alt * ((50000. / (Scale.alt s.scale) * (alphaMax-alpha s)/90. )**0.5)  )
(** altitude delta (en y) en pixels *)

let fluwip (s:t) flux = 
  let c = if earthMode  s then (Scale.set_min s.scale) else s.scale in
(*  Tfloat.( 2.6 * ((cut 500. 20000. flux)**0.4) / ((Scale.alt c)**0.42) )*)
  Tfloat.( 5.2 * ((cut 50. 2000. flux)**0.4) / ((Scale.alt c)**0.42) )

(************************************* MODULE GEORECT ************************************)

module GeoRect = struct
(** = partie de la surface de la terre à afficher *)

  type scene = t
  type t =
    {
    lonMin : float;
    lonMax : float;
    latMin : float;
    latMax : float;
    }
  
  let fittingLat crLat oaLat wLat = (*optimale apparente lat, working lat*)
    let open Tfloat in
    let rec f i wLat =
      let od = oaLat - crLat in
      let ad = E.latdid crLat wLat in
      let d  = ad - od in    (* écart à l'apparent optimal *)
      let r     = 1. / (E.yxr (iof E.latMax)) in (* carré du ratio optimal pour se rapprocher de la wLat optimale au plus vite pour tous oalat et wlat fournis *)
      let bwLat = wLat  - d * r * r in (* better wLat *)
(*      print_string ("i = "^soi i^" ; ");*)
(*      print_endline ("d = "^Strn.float (-2) d);*)
      let d=abs d in if d<0.05 || i>9 then bwLat else f (i++1) bwLat  in
    f 0 wLat
  (* correction de wLat par approximation successives, de façon à ce que la distance apparente entre wlat et crLat égale la distance entre oalat et crLat *)
  (* on arrete de chercher quand la précision est satisfaisante : ± 0.05° (obtenu en 6 à 8 coups) *)

  let mercator crLat g =
    { g with 
      latMin = fittingLat crLat g.latMin g.latMin ;
      latMax = fittingLat crLat g.latMax g.latMax }
  (* correction du geoRect au vu des déformations à l'affichage qui suivront (pour les regions polaires, dans le cas d'une projection de type Mercator *)


  let make (crLat, crLon) scene screen =
    let open Tfloat in
    let sr = Screen.ratio screen in
    match earthMode scene, forme scene with
    | true , _     -> let hwid = E.wid/2. in {
      lonMin = crLon - hwid - 0.1 ; (* pour garantir l’affichage sur tout l’écran, quitte à afficher 2 fois la regio à chaque
      bord de l’écran *)
      lonMax = crLon + hwid + 0.1 ;
      latMin = E.latMin ;
      latMax = E.latMax ;
      }
    | false, _     ->
      let alt   = Scale.alt scene.scale in (* distance de la camera au sol *)
      let omega = 30. in (* (demi)ouverture de la camera =30°, arbitraire pour scene2D, à corriger en fonction du modèle 3D *)
      let pi    = 3.1415927 in
      let radian= ( * ) (pi/180.) in
      let ro    = radian omega in
      let nvh ra= (alt * sin ro) / sin (ra - ro) in
      let svh ra= (alt * sin ro) / sin (pi - ro - ra) in
      let hoa ra= nvh ra + svh ra in
      let wik = hoa (radian 90.) * sr in
      let wid = wik / E.adik in
      let hwid = wid * 0.50 in
      let secur = 1.08 in
      mercator crLat {
        lonMin = crLon - hwid ;
        lonMax = crLon + hwid ;
        latMin = crLat - hwid * secur / sr ;
        latMax = crLat + hwid * secur / sr ;
        } 
   (* "rectangle" terrestre, en degrés de lon/lat, à afficher sur la surface-écran disponible *)
 
  let compute e scene screen = (make (Espace.Regio.coords e scene.cr) scene screen)
  
  let is_visible     gr (lat,lon) = lon>gr.lonMin && lon<gr.lonMax && lat >gr.latMin && lat<gr.latMax 

  let is_borderline  gr (crLat, crLon) (lat,lon) = 
    let open Tfloat in
    let latMin = gr.latMin + (crLat - gr.latMin) * 0.30
    and lonMin = gr.lonMin + (crLon - gr.lonMin) * 0.20
    and latMax = gr.latMax - (gr.latMax - crLat) * 0.30
    and lonMax = gr.lonMax - (gr.lonMax - crLon) * 0.20 in
    match ( lat<latMin , lon<lonMin , lat>latMax , lon>lonMax ) with
    | true , true , false, false-> Some ED.nw
    | false, true , true , false-> Some ED.sw
    | false, false, true , true -> Some ED.se
    | true , false, false, true -> Some ED.ne
    | true , _    , _    , _    -> Some ED.nord
    | _    , true , _    , _    -> Some ED.west
    | _    , _    , true , _    -> Some ED.sud
    | _    , _    , _    , true -> Some ED.east
    | _                         -> None

  
  
  let visibleCoords gr (lat,lon) =
    let open Tfloat in
    let f p min max x =
      let nx = if x<min then x+p else if x>max then x-p else x in
      if nx>min && nx<max then Some nx else None in
    match (f (E.hid) gr.latMin gr.latMax lat) , (f (E.wid) gr.lonMin gr.lonMax lon) with
    | Some lat, Some lon -> Some (lat,lon)
    | _ -> None
  (* s’assure que la position donnée se trouve dans le rectangle à afficher ; réalise les modulos nécessaires pour l’aspect cylindrique ; renvoie None si la regio n’est pas dans le rectangle à afficher *)
  
end


(******************************* UPDATE FUNCTIONS **************************************)
let zoom s task =
  let module C = Scale in
  let c = s.scale in
  match earthMode  s, task, s.sr with
  | true,  `zoom_in , Some sr -> { s with earthMode =false; scale = C.set_min c; cr=sr }
  | true,  `zoom_in , None    -> { s with earthMode =false; scale = C.set_min c        }
  | false, `zoom_in , Some sr -> { s with                   scale = C.incr c   ; cr=sr }
  | false, `zoom_in , None    -> { s with                   scale = C.incr c           }
  | false, `zoom_out, _ when C.int c=C.min c -> { s with earthMode =true                       }
  | false, `zoom_out, _                      -> { s with                   scale = C.decr c    }
  | _                                        ->   s


let urbsRid o p = 
  let nid = Game.Player.pov p in
  if nid <> Nid.none then NatioList.urbsRid o.Orbis.natioList (Game.Player.pov p) else Espace.centerRid o.Orbis.espace


let move_to_capitolium o s p =
  { s with 
(*    earthMode=false;*)
(*           scale    = Level.set s.scale 5;*)
           cr       = urbsRid o p }
(* centrage sur la capitale *)


let natio o p =
  let nid = Game.Player.pov p in
  if nid == Nid.none 
  then None
  else 
    let e = o.Orbis.espace in
    let n = NatioList.get o.Orbis.natioList nid in
    let urbsRid = Natio.urbsRid n in
    Some {
    urbsRid;
    urbsLat = E.Regio.latitude  e urbsRid;
    urbsLon = E.Regio.longitude e urbsRid;
    has_nav   = Natio.has_nav n;
    ter_rayon = Natio.ter_rayon n;
    mer_rayon = Natio.mer_rayon n;
  }
(*** FIXME ***)

let create o p = 
  let natio = natio o p in
  {
  filter   = `tegmen;
  scale    = Scale.make;
  angle    = Level.make 4 9 9;
  forme    = Plan;
  earthMode= true;
  options  = { altitude = false;  borders = true ; nations=false };
  cr       = ( match natio with None   -> Espace.centerRid o.Orbis.espace
                              | Some n -> n.urbsRid );
(*  cr       = urbsRid o p;*)
(*  cr = (*Rid.first*)  NatioList.urbsRid (o.Orbis.natioList) (List.hd o.Orbis.natioIdList)  (*Espace.centerRid (o.Orbis.espace)*);*)
  sr       = None ;
  natio    = natio;
  }


let reset s =
  {
  s with
  filter   = `tegmen;
  options  = { altitude = false;  borders = false ; nations=false };
  }


let map_move o s (dir,int) =
  let e = o.Orbis.espace in
  match E.forme e with
  | E.Cylinder _ -> 
    let res = E.resolution e in
    let ay,ax = E.Cylinder.yx_of_rrid res (s.cr) in
    let vy,vx = E.vecteur_of_direction dir in (* variations *)
    let d = max 1 (iof(foi int *. 0.05 *. Scale.alt s.scale /. (E.rw e) )) in
    { s with cr = E.Cylinder.rid_of_ryx res (ay+d*vy) (ax+d*vx) }
  | _ -> s
(* déplacement de la carte == déplacement de la centered regio *)
(* ne fonctionne qu’en mode cylindre pour le moment *)

 
let move_sr o s (dir,int) =
  match s.sr with
  | None    -> { s with sr = Some s.cr }
  | Some sr ->
  let e = o.Orbis.espace in
  let res = E.resolution e in
  let ay,ax = E.Cylinder.yx_of_rrid res sr in
  let vy,vx = Espace.vecteur_of_direction dir in (* variations *)
  { s with sr = Some (E.Cylinder.rid_of_ryx res (ay+int*vy) (ax+int*vx)) }
(* déplacer la sr avec le keypad *)


let update g s p task = 
  let o = Game.orbis g in
  match task with
| `map_center rid      -> { s with cr = rid } 
| `map_move (dir,int)  -> map_move o s (dir,int)
| `move_to_capitolium  -> move_to_capitolium o s p 
| `move_sr  (dir,int)  -> move_sr  o s (dir,int)
| `select_regio ridOpt -> { s with sr = ridOpt } 
| `secure_sr           -> { s with sr = match s.sr with None -> Some s.cr | sr -> sr } 
| `select_filter f     -> { s with filter = f; options = { s.options with nations=false } }
| `switch_filter       -> { s with filter = try (Tlist.following_or_first s.filter filterList) with Failure _ -> `tegmen }
| `angle_up            -> { s with angle = Level.incr s.angle }
| `angle_down          -> { s with angle = Level.decr s.angle }
| `zoom_in             -> zoom s task
| `zoom_out            -> zoom s task
(*| `new_game o          -> create o player*)
| `toggle_earthMode    -> { s with earthMode = not s.earthMode }
| `toggle_altitude     -> { s with options = { s.options with altitude = not s.options.altitude } }
| `toggle_borders      -> { s with options = { s.options with borders  = not s.options.borders  } }
| `toggle_nations      -> { s with options = { s.options with nations  = not s.options.nations  } }
| `defaultDisplay      -> reset s
| _                    ->   s

