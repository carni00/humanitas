(*

 ****************************** Partitio.ml ******************************


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
open Tfloat

module Nil = Nid.Nil

type t = {
  labor     : float;
  sapientia : float;
  religio   : float;
  militaria : float;
  oppressio : float;
  luxus     : float;
  otium     : float;
  }

type attributio = 
| LAB
| SAP
| REL
| MIL
| OPP
| LUX
| OTI

type usus = attributio*float
(* expérience des spécialistes *)

type natio = {
  g           : G.Natio.t;
  pyramid     : Dx.Pyramid.t;
  artes       : Ars.t list;
  politeia    : Politeia.t;
  plebs       : float;
  fides       : float;
  libertas    : float;
  efficientia : float;
  facultas    : float;
  agriCopia   : float;
  ususList    : usus list;
  pp          : t;
  }
(* données de la natio qui déterminent le calcul d’une partitio *)


let make ~labor ~sapientia ~religio ~militaria ~oppressio ~luxus ~otium = {
  labor;
  sapientia;
  religio;
  militaria;
  oppressio;
  luxus;
  otium;
  }


let null =
        {
        labor     = 0.;
        sapientia = 0.;
        militaria = 0.;
        oppressio = 0.;
        religio   = 0.;
        luxus     = 0.;
        otium     = 0.;
        }

let debug =
        {
        labor     = 1000.;
        sapientia = 2000.;
        militaria = 3000.;
        oppressio = 4000.;
        religio   = 5000.;
        luxus     = 6000.;
        otium     = 7000.;
        }

let attrib p a = match a with
| LAB -> p.labor
| SAP -> p.sapientia
| MIL -> p.militaria
| OPP -> p.oppressio
| REL -> p.religio
| LUX -> p.luxus
| OTI -> p.otium

let lab p = p.labor
let sap p = p.sapientia
let mil p = p.militaria
let opp p = p.oppressio
let rel p = p.religio
let lux p = p.luxus
let oti p = p.otium


let cibus        p = p.labor (* nourriture *)
let periti       p = p.militaria + p.oppressio + p.religio
let spolium      p = p.militaria + p.oppressio + p.religio + p.luxus
let stratiotikon p = p.militaria + p.oppressio + p.religio + p.otium
let alienatio    p = p.militaria + p.oppressio + p.religio + p.luxus + p.otium
let humanitas    p = p.militaria + p.oppressio + p.religio + p.luxus + p.sapientia
let servitium    p = spolium p * 0.1
let summa        p = p.militaria + p.oppressio + p.religio + p.luxus + p.sapientia + p.labor + p.otium



let set_att p (a,v) = match a with
| LAB -> { p with labor=v }
| SAP -> { p with sapientia=v }
| OPP -> { p with oppressio=v }
| MIL -> { p with militaria=v }
| REL -> { p with religio=v }
| LUX -> { p with luxus=v }
| OTI -> { p with otium=v }


type fun_cons =
| ADD_PL of t Nil.t
| ADD_P  of t 
| SBS_P  of t 
| MUL_I  of float
| DIV_I  of float
| FUN    of (attributio -> float -> float)
(** identifiant d’une fonction d’altération d’une partitio.t *)

let alter p (fcl:fun_cons list) =
  let fun_alter = function
  | ADD_PL(pList)-> (fun a v -> Nil.fold_left (fun s p -> s+(attrib p a) ) v pList )
  | ADD_P(p)     -> (fun a v -> v + attrib p a)
  | SBS_P(p)     -> (fun a v -> v - attrib p a)
  | MUL_I(i)     -> (fun a v -> v * i)
  | DIV_I(i)     -> (fun a v -> squot 0. v i) (*la plèbe peut être nulle*) 
  | FUN(f)       ->  f in
  let fun_list aid = List.map (fun fcons ->(fun_alter fcons) aid) fcl in
  let f aid att    = Tlist.applique att (fun_list aid) in
    {
    labor      = f LAB p.labor;
    sapientia  = f SAP p.sapientia; 
    oppressio  = f OPP p.oppressio;
    militaria  = f MIL p.militaria;
    religio    = f REL p.religio;
    luxus      = f LUX p.luxus;
    otium      = f OTI p.otium
    }
(* application d’une liste de transformations aux membres de p *) 

let listSum pList = match pList with (nid,p)::q -> alter p [ADD_PL q] | []->null
(* somme de partitio *) 

let actioFcons pyramid plebs = MUL_I ( Dx.Pyramid.facultas_ratio pyramid plebs )

let fructusFcons eff artes ususList = 
  let fun_eff a v =
    let et = Ars.eff_tab artes in 
    match a with
      LAB -> v *  eff
    | SAP -> v *  et.Ars.sap 
    | OPP -> v * (*u+usus.mil) *) et.Ars.opp
    | MIL -> v * (*u+usus.mil) *) et.Ars.mil
    | REL -> v * (*u+usus.rel) *) et.Ars.sap
    | LUX -> v *  eff
    | OTI -> v in
  (* incidence des facteurs de productivité sur la production *)
  FUN fun_eff

let factumFcons plebs = MUL_I plebs
  
let fructus_of_factum p plebs = alter p [DIV_I plebs]



let actio p(*partitio*) n(*pnatio*) =
  alter p [actioFcons n.pyramid n.plebs]

let fructus p(*partitio*) n(*pnatio*) =
  alter p [actioFcons n.pyramid n.plebs ; fructusFcons n.efficientia n.artes n.ususList ]

let factum p(*partitio*) n(*pnatio*) =
  alter p [actioFcons n.pyramid n.plebs ; fructusFcons n.efficientia n.artes n.ususList ; factumFcons n.plebs ]

(*let fructus_of_factum p n =*)
(*  alter p [fructus_of_factumFcons p n.plebs]*)

module Record =
  struct
  type partitio = t
  type t = {
    attrib : partitio;
    actio  : partitio;
    fructus: partitio;
    factum : partitio;
    }
  let compute p(*partitio*) n(*pnatio*) =
    let actio   = alter p       [actioFcons   n.pyramid n.plebs] in
    let fructus = alter actio   [fructusFcons n.efficientia n.artes n.ususList ] in
    let factum  = alter fructus [factumFcons  n.plebs ] in {
      attrib = p;
      actio  = actio;
      fructus= fructus;
      factum = factum;
    }
  let attrib  f = f.attrib
  let actio   f = f.actio
  let fructus f = f.fructus
  let factum  f = f.factum
  end




let damnum_of_factum p (funus,chora) =
  let ratio = ( funus.Rv.plebs /. chora.Rv.plebs ) in
  let f a v = match a with
  | LAB -> v * ratio * 0.50
  | MIL -> v * ratio * 0.75
  | LUX -> v * ratio
  | _   -> 0. in
  alter p [FUN f]

let tributum_of_damnum p relatio =
  let f a v = match a with
  | LAB -> v
  | MIL -> if relatio=Junctiones.Pax then v else 0.
  | LUX -> if relatio=Junctiones.Pax then v else 0.
  | _   -> 0. in
  alter p [FUN f]



(************************ CALCUL DE LA NLE PARTITIO ******************************)

let nextLuxus politeia p alien =
  match Politeia.is_aristocratic politeia with
  | false -> 0.
  | true -> let plr (* previous luxus rate *) = squot 0. p.luxus (alienatio p) in
            alien * (min 1. (plr + 0.001))
(* le luxe augmente d’un millième d’alienatio chaque tour *)


let resistance_a_l_occupation g centralCoef =
  let module Gn = G.Natio in
  let occupation_taux = squot 0. (max 0. (Gn.choraAmp g - Gn.centrAmp g)) (Gn.choraAmp g) in
  occupation_taux * (u + centralCoef)


let civicMilitaria gn lab alien libertas politeia agriCopia warCoef =
  let centralCoef = if Politeia.is_centralized politeia then u else 0. in
  (* seules les politeia bureaucratiques peuvent utiliser l’armée pour s’approprier des terres à coloniser *)
  let f x = (2. ** (x * 10.) )    (* chaque dixième d'agriCopia augmente d’un facteur 2 le résultat*)
  and g x = (8. ** (x * 10.) ) in
  let civicMilitaria =
  (* protection_des_recoltes *)                0.001 * f (1.4 - agriCopia) 
  (* politique_coloniale     *)+ centralCoef * 0.001 * g (1.2 - agriCopia) 
  (* defense_du_territoire   *)+ (resistance_a_l_occupation gn centralCoef) in
  min3 (u-lab-alien) libertas civicMilitaria
  (* la survie passe avant la milice civique *)
  (* la milice civique n’est assuré que par les peuples libres *)
(*  défense anarchique des récoltes et des terres, et conquete de nouvelles terres vierges (républiques) *) 


let needed_labor n =
  let ar = Dx.Pyramid.alimonium_ratio n.pyramid n.plebs in
  let fr = Dx.Pyramid.facultas_ratio n.pyramid n.plebs in
  let fsp= G.Natio.fineSumPle (n.g) in (* somme de la plèbe frontalière == occupée *)
  let pr = 0.33 * fsp / n.plebs in (* pillage ratio *)
  let nc = ar * (u+pr) in (* needed cibus *)
  (min u (squot u nc (fr*n.efficientia)))
(* indice : labor nécessaire à la satisfaction des besoins scx primaires *)

let primary (n:natio) = 
  let nl = needed_labor n in
  let eo (*e_oppressio*) = n.pp.oppressio * Ars.eff n.artes Ars.MET * Ars.eff n.artes Ars.GUN in
(* valeur : oppressio effective / fructus.oppressio *)
(*  let cr (*copia_ratio*) = copiaRatio n.facultas n.plebs ar fr n.pp in*)
  let lab (*labor*) = min3 u (nl*1.1) (nl*n.agriCopia) in
(* indice : labor est limitée par l'insuffisance relative de terres arables *)
  let alien = (u-lab) * (cut 0. u ( n.fides ** 0.5 )) in
  let mil = civicMilitaria n.g lab alien n.libertas n.politeia n.agriCopia 0. in
  let sap (*sapientia*) = max 0. (u - lab - max (nl-lab) (alien + mil + eo) ) in
  let lux = nextLuxus n.politeia n.pp (u-lab-sap-mil) in
  let oti = max 0. (u-lab-sap-mil-lux) in (* soustraction de float, on vérifie que résultat >= 0 *)
(* désoeuvrement primaire : ie à moins que l’empire n’en fasse qqchose *)  
  List.fold_left set_att null [(LAB,lab);(SAP,sap);(LUX,lux);(MIL,mil);(OTI,oti)]
(* découpage primaire : ie avant stratiotikon,  de la capacité sociale *)

let compute n (s:Stratiotikon.t) =
  let module Strat = Stratiotikon in
  let p = primary n in
  let mil = p.otium * (Strat.mil s) + p.militaria in
  let rel = p.otium * (Strat.rel s) in
  let opp = p.otium * (Strat.opp s) in
  let oti = u-p.labor-p.sapientia-mil-rel-opp-p.luxus in
  List.fold_left set_att p [(MIL,mil);(REL,rel);(OPP,opp);(OTI,oti)]
(* découpage après stratiotikon de la capacité sociale *)
