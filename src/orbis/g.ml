(*

 ****************************** G(eographia).ml ******************************


 *  This file is part of Humanitas.

 *  Humanitas is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License,
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
module Rvi=Rv.Incola

type chora = Rv.gRegio

type t = {
  contactusMx  :  bool Nid.Nix.t; (* matrice des contacts *)
  finesAmpMx   :   float Nid.Nix.t; (* matrice des amplitudes des marges == zones ss occupation militaire (leur superficie) *)
                                  (* y = incola, x = dominus *)
  imperiumArray:   float Nid.Nia.t; (* superficie des imperii *)
  choraArray   : chora Nid.Nia.t; (* array des chora (ttes données des chora) *)
  finesMx      : chora Nid.Nix.t; (* matrice des marges == zones occupées *)
}
(* des arrays parcequ’on tire ces données de la lecture de l’imperiumMap : carte de 30 000 regiones.
L’efficacité s’impose, et elle passe par une lecture unique de l’imperiumMap. *)

type   nid = Nid.t
module Nia = Nid.Nia
module Nix = Nid.Nix
module Nil = Nid.Nil
module Nim = Nid.Nim

let contactus    g y x =  Nix.get g.contactusMx y x
let finesAmp     g y x =  Nix.get g.finesAmpMx  y x
let fines        g y x =  Nix.get g.finesMx     y x
let imperium     g   x =  Nia.get g.imperiumArray x

let chora        g   x =  Nia.get g.choraArray   x
let choraAmp     g   x = (Nia.get g.choraArray   x).Rv.superficies
let facultas     g   x = (Nia.get g.choraArray   x).Rv.facultas
let hospitalitas g   x = (Nia.get g.choraArray   x).Rv.hospitalitas
let instrumentum g   x = (Nia.get g.choraArray   x).Rv.instrumentum
let latifundium  g   x = (Nia.get g.choraArray   x).Rv.latifundium 
let plebs        g   x = (Nia.get g.choraArray   x).Rv.plebs

  
let pil nil g n = List.filter (contactus g n) nil
(* listes des nationes, par leur id, avec lesquelles la nation n est en contact *)

(*
let warRatio g j i =
  let warNb = J.warNb j i in
  let contactNb = Nia.fold_left (fun s b -> s+iob b) 0 (Nix.line g.contactusMx i) in
  quot warNb contactNb 0
(* proportion de nations voisines avec lesquelles on est en guerre *)

let lostRatio g j i = 
  let warNatioList = J.warNatioList j i in
  let lostChora = List.fold_left (fun s d -> s+fines g i d) 0 warNatioList in
  quot lostChora (chora g i) 0
(* proportion de notre chora occupée par des nationes avec lesquelles on est en guerre *)
*)

(****************************************************************************************)

let nullChora = {
  Rv.superficies = 0.;
  Rv.facultas    = 0.;
  Rv.hospitalitas= 0.;
  Rv.instrumentum= 0.;
  Rv.plebs       = 0.;
  Rv.latifundium = 0.;
  }

let addRegio chora regio = 
  let regioPlebs = regio.Rv.plebs in
  {
  Rv.superficies = chora.Rv.superficies  +. regio.Rv.superficies;
  Rv.facultas    = chora.Rv.facultas     +. regio.Rv.facultas   ;
  Rv.hospitalitas= chora.Rv.hospitalitas +. regio.Rv.hospitalitas *. regioPlebs;
  Rv.instrumentum= chora.Rv.instrumentum +. regio.Rv.instrumentum *. regioPlebs;
  Rv.plebs       = chora.Rv.plebs        +. regioPlebs;
  Rv.latifundium = chora.Rv.latifundium  +. regio.Rv.latifundium; (*somme des plèbes latifundiaires*)
  }
  
let finalizeChora chora plebs =
  {
  chora with
  Rv.hospitalitas = chora.Rv.hospitalitas /. plebs;
  Rv.instrumentum = chora.Rv.instrumentum /. plebs;
  Rv.latifundium  = chora.Rv.latifundium  /. plebs; (* plèbe en régime latifundiaire / plèbe totale *)
  }


let substractChora chora chorb =
  let pleas = chora.Rv.plebs in
  let plebs = chorb.Rv.plebs in
  {
  Rv.superficies = chora.Rv.superficies  -. chorb.Rv.superficies;
  Rv.facultas    = chora.Rv.facultas     -. chorb.Rv.facultas   ;
  Rv.hospitalitas=(chora.Rv.hospitalitas *. pleas -. chorb.Rv.hospitalitas *. plebs) /. (pleas -. plebs);
  Rv.instrumentum=(chora.Rv.instrumentum *. pleas -. chorb.Rv.instrumentum *. plebs) /. (pleas -. plebs);
  Rv.plebs       = chora.Rv.plebs        -. chorb.Rv.plebs;
  Rv.latifundium = chora.Rv.latifundium  -. chorb.Rv.latifundium;
  }
 


(****************************************************************************************)

let make() =
  {
  contactusMx   = Nix.make false ; 
  finesAmpMx    = Nix.make 0. ; 
  finesMx       = Nix.make nullChora ; 
  imperiumArray = Nia.make 0. ;
  choraArray    = Nia.make nullChora ;
  }


let create e rm im =
  let open Tfloat in
  let g = make() in
  let f rid rv =
    let r  = Rm.get rm rid in
    let rs = R.area r in
    let dom= Rv.dominus rv
    and con= Rv.contents rv in
    if dom <> Nid.none then
      (
      Nia.set g.imperiumArray dom (imperium g dom + rs);
(* dominus <> Nid.none => imperium augmenté *)
      let f pid =
        let pdom = Im.dominus im pid in
        if (pdom <> Nid.none) && (pdom <> dom)
        then Nix.set g.contactusMx dom pdom true in
      List.iter f (E.Regio.lesQuatre e rid);
(* deux imperii voisins => contact (inutile d'écrire le contactus symétrique central dans la matrice immédiatement, puisqu'on regarde toutes les regiones) *)
      match con with Rv.Desertum_for _ -> () | Rv.Incol incola -> let inc = Rvi.nid incola in
      if dom <> inc then
        (
        Nix.set g.contactusMx inc dom true;
        Nix.set g.contactusMx dom inc true;
	Nix.set g.finesAmpMx  inc dom (finesAmp g inc dom + rs); (*regio superficies*)
	Nix.set g.finesMx  inc dom (addRegio (fines g inc dom) (Rv.gRegio rs r incola)) (*regio superficies*)
	)
(* incola et dominus différent => contact ET fines augmentées *)
      );
    match con with Rv.Desertum_for _ -> () | Rv.Incol incola -> let nid = Rvi.nid incola in
      (
      Nia.set g.choraArray   nid (addRegio (chora g nid) (Rv.gRegio rs r incola));
      (*la productivité de la natio est la moyenne des productivité de chaque regio pondérée par sa population*)
      ) in
(* incola <> Nid.none => chora augmentée *)
(* on lit toutes les regiones et on en tire pas à pas toutes les infos nécessaires, en mettant à jour des arrays *)
  let _ = Im.nIter f im ;
          Nia.nIter (fun i   c -> Nia.set g.choraArray i   ( finalizeChora c (plebs g i) )) g.choraArray;
          Nix.nIter (fun i d c -> Nix.set g.finesMx     i d ( finalizeChora c (plebs g i) )) g.finesMx ;
          in g

(************************************ G.Natio : géographie d’une natio *****************************)

module Natio = struct
  type g = t


  type t = {
    facultas       : float;  (* potentiel de production de la chora, tegmen compris *)
    hospitalitas   : float;  
    instrumentum   : float; 
    latifundium    : float;  (* plèbe latifundiaire / plèbe totale : [0..1] *)
    plebs          : float;  (* population nationale *)
    imperium       : float;    (* superficie de l’imperium *)
    chora          : chora;  (* données relative au total de notre chora *) 
    centrum        : chora;  (* données relative au " centre " : la chora sous notre controle *)
    funusList      : chora Nid.Nil.t; (* données relatives aux " pertes     " : la chora sous le controle d’autres empires *)
    finesList      : chora Nid.Nil.t; (* données relatives aux " frontières " : les chora étrangères sous notre controle *)
    choraAmp       : float;    (* superficie des chora *)
    centrAmp       : float;
    funusAmp       : float   Nid.Nil.t; (* subdivisions de notre chora par imperium *)
    pil            : nid list;
  }
  
  open Tfloat

  let make g nil nid = 
    let pil          = pil nil g nid in
    let chora        = chora g nid in
    let choraAmp     = choraAmp g nid  in  (* superficie des chora *)
    let funusAmp     = Nil.init   (fun i -> finesAmp g nid i) pil in
    let funusList    = Nil.filter (fun c -> c.Rv.superficies<>0.) (Nia.to_nil (Nix.line   g.finesMx nid)) in
    let finesList    = Nil.filter (fun c -> c.Rv.superficies<>0.) (           (Nix.column g.finesMx nid)) in
    {
    facultas       = facultas g nid;   (* potentiel de production des chora, tegmen compris *)
    hospitalitas   = hospitalitas g nid;
    instrumentum   = instrumentum g nid;
    latifundium    = latifundium g nid;
    plebs          = plebs g nid;   (* population de chaque natio*)
    imperium       = imperium g nid;   (* superficie des imperii *)
    chora          = chora ;
    centrum        = Nil.fold_left (fun s c -> substractChora s c) chora funusList;
    funusList;
    finesList;
    choraAmp       = choraAmp; (* superficie des chora *)
    centrAmp       = Nil.fold_left (fun s a -> s-a) choraAmp funusAmp ; (* superficie de la chora controlée *)
    funusAmp;
    pil            = pil; (* listes des nationes, par leur id, avec lesquelles la nation nid est en contact *)
    }
  
  let facultas       n = n.facultas 
  let hospitalitas   n = n.hospitalitas   
  let instrumentum   n = n.instrumentum 
  let latifundium    n = n.latifundium  
  let plebs          n = n.plebs   
  let imperium       n = n.imperium 
  let chora          n = n.chora 
  let centrum        n = n.centrum   
  let funusList      n = n.funusList
  let finesList      n = n.finesList
  let choraAmp       n = n.choraAmp      
  let centrAmp       n = n.centrAmp      
  let funusAmp       n = n.funusAmp    
  let pil            n = n.pil 

  let funuSumAmp     n = Nil.fold_left (+) 0. n.funusAmp
  let funuSumPle     n = Nil.fold_left (fun s c -> s +  c.Rv.plebs) 0. n.funusList

  let fineSumAmp     n = Nil.fold_left (fun s c -> s + c.Rv.superficies) 0. n.finesList
  let fineSumPle     n = Nil.fold_left (fun s c -> s +  c.Rv.plebs) 0. n.finesList

  let impAmp         n = choraAmp n - funuSumAmp n + fineSumAmp n
  let impPle         n = plebs    n -  funuSumPle n +  fineSumPle n


  let null =
    {
    facultas       = 0.;   (* potentiel de production des chora, tegmen compris *)
    hospitalitas   = 0.;   
    instrumentum   = 0.; 
    latifundium    = 0.; 
    plebs          = 0.;   (* population de chaque natio*)
    imperium       = 0.;   (* superficie des imperii *)
    chora          = nullChora ;   
    centrum        = nullChora;   
    funusList      = Nid.Nil.empty;
    finesList      = Nid.Nil.empty;
    choraAmp       = 0.;  
    centrAmp       = 0.; 
    funusAmp       = Nid.Nil.empty;
    pil            = [];
    }

  end

let natioList g nil = Nid.Nil.init (fun nid -> Natio.make g nil nid) nil


