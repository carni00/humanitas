(*

 ****************************** Regio_Variable.ml ******************************


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

type nid = Nid.t


type tegmen =
  | Hydros    of R.hydros
  | Desertum  of R.climax (* aucune mise en valeur courante *)
  | Fields_and_woods
  | Fields_and_pasture
  | Pasture
  | Fields
  | Irrigation
  | Tmine
  | Turbs
(* couvert regional effectif = produit cartésien des conditions naturelles et de la mise en valeur humaine *)


(************************* MODULE INCOLA *******************************)

module Incola = struct

  open Tfloat

  type oikos =
    | Saltus   (* terre peu ou inégalement mise en valeur (élevage et/ou bien agri sur les meilleures terres uniquement) *)
    | Ager     (* terre intégralement mise en valeur *)
    | Mine     (* exploitation des ressources du sous-sol *)
    | Urbs     (* grande cité *)
  (* forme et intensité de la mise en valeur de la regio par l’homme *)

  type dominium =
    | Mir           (* communauté paysanne *)
    | Latifundium   (* grande propriété privée *)
    | Minifundium   (* petites propriétés privées *)
  (* organisation sociale *)

  type t = {
    nb           : float;      (* nombre d’habitants de la regio (notez que la taille en km2 de la regio est variable selon la
    taille en regio de la carte)*)
    nid          : nid;     (* nationalité des habitants *)
    oikos        : oikos ;
    dominium     : dominium ;
    instrumentum : float; (** capital fixe spécifique à la regio (indice indépendant de la superficie de la regio) *)
    }

  let tegmen climax oikos = match climax, oikos with
    | R.Forest    _  ,Ager
    | R.Woodland  _  ,Ager                
    | R.Grassland _  ,Ager       -> Fields
    | R.Steppe       ,Ager       -> Irrigation
    | R.Forest    _  ,Saltus       
    | R.Woodland  _  ,Saltus     -> Fields_and_woods
    | R.Grassland _  ,Saltus     -> Fields_and_pasture
    | R.Steppe       ,Saltus     -> Pasture
    | _              ,Urbs       -> Turbs
    | _              ,Mine       -> Tmine
    | c              ,_          -> Desertum c

  let facultas rs hosp inst tegmen =
    let instFactor = u + inst / 100. in
    let base = match tegmen with
    | Desertum _        -> hosp * 0.1 (* chasse cueillette *)
    | Pasture           -> (min hosp 20.) + 10. * instFactor
    | Fields_and_pasture
    | Fields_and_woods  -> hosp + 20. * instFactor
    | Fields           
    | Irrigation      
    | Turbs             -> hosp + 80. * instFactor
    | _                 -> 0.
    (* fructus maximum (annuel) pouvant être extrait d'une superficie de 50 km2 *)
    in (base * rs / 50.)
    (* fructus maximum (annuel) pouvant être extrait d'une regio, l'année prochaine, avec une plèbe infinie *)
    (* facultas se distingue d'hospitalitas en ce qu'elle prend en considération les transformations humaines du territoire, l'instrumentum de
    la nation, et la superficie de la regio *)
    (* facultas n'est pas un indicateur de productivité du travail *)

  let densitas rs plebs = plebs / (rs)

  module Next = struct
  
    let instrumentum i artes sophia = 
      let artes_limit  = Ars.instMax artes
      and sophia_limit = sophia*1000. in
      let max = min sophia_limit artes_limit in
      if i<max
      then i + (1.+i) * (10. ** (-1. -4.*i/max)) (* Croissance *)
      else i + (max-i)*0.5 (* Décroissance *)
  
    let oikos r oikos tegmen plebs inst agriCopia =
      let hosp     = R.hospitalitas r in
      let facultas = facultas (R.area r) hosp inst tegmen in
      let deboisemt c = c < 1.05 && Random.int (1 lsl (max 1 (iof((c-u)*100.)))) = 0 in
           if plebs < facultas then (oikos, false)
      else if oikos==Ager && ( plebs / (R.area r) >= 4. ) then (Ager, true)
      else ((match tegmen with
      | Fields_and_pasture -> if (hosp + inst)>=100. then Ager else oikos
      | Fields_and_woods   -> if (hosp + inst)>=120. && deboisemt agriCopia then Ager else oikos
      | Pasture            -> (match R.hydros r with R.River _ -> if inst>=40. then Ager else oikos | _ -> oikos)
      | Desertum _         -> Saltus (* colonization *)
      | _ -> oikos ),false)
      (* returns the next oikos and a boolean : wether it is a new possible civitas *)

  end

  let make nb nid oikos dominium instrumentum = {
    nb;
    nid;
    oikos;
    dominium;
    instrumentum;
    }

  let create r nid oikos age = 
    let inst    = if age < 200 then (foi age)/10.  else log (foi age) * 7.55 / 2. in
    let tegmn   = tegmen (R.climax r) (oikos) in
    let oikos,_ = Next.oikos r oikos tegmn ( (R.area r) * 10.) inst (1.2) in
    let tegmn   = tegmen (R.climax r) (oikos) in
    let facultas= facultas (R.area r) (R.hospitalitas r) inst tegmn in
    {
    nb   = facultas * (log (foi age) / 6.); (* approximation temporaire faute de tegmen *)
    nid  = nid;
    oikos= oikos;
    dominium = Mir;
    instrumentum = inst;
    }
 
  let colonus nid agriCopia facultas =
    {
    nb = facultas * ((max u (u / agriCopia + 0.1)) ** 2.) ;
    nid;
    oikos = Saltus;
    dominium = Mir;
    instrumentum = 0.;
    }



  let oikos        i = i.oikos    
  let dominium     i = i.dominium    
  let nid          i = i.nid
  let plebs        i = i.nb
  let instrumentum i = i.instrumentum
  let densitas  rs i = densitas rs (i.nb)

end

(*****************************************************************************)

type contents =
  | Desertum_for of int
  | Incol        of Incola.t


module Brouillards = struct
  type t = int
  let mark int nid = Ext.fill_nth_bit int (Nid.ti nid)
  (** la natio nid connaît la regio *)
  let read int nid = (Ext.read_nth_bit int (Nid.ti nid) == 1)
  (** la natio nid connaît-elle la regio ? *)
  let null = (0 lsr 64)
  let init nid = mark null nid
end

(** brouillards de guerre == visibilité de cette regio pour chacune des natio *)

type t = {
  dominus   : nid;      (* propriétaire *)
  contents  : contents; (* contenu *)
  brouillards : Brouillards.t ;
  }
(* Eléments variables d'une regio *)


let null = {
  dominus     = Nid.none;
  contents    = Desertum_for 64; (*nb d'années pour atteindre le stade forêt*)
  brouillards = Brouillards.null;
  }

let make dominus contents = 
  {
  dominus;
  contents;
  brouillards = Brouillards.init dominus;
  }
(*appelé par Im.create uniquement*)

let update rv dominus contents = 
  {
  dominus;
  contents;
  brouillards = Brouillards.mark (rv.brouillards) dominus;
  }
(* il faut créer un nouveau record : la modification de rv fausserait la carte de lecture de Im.update *)
(* on récupère seulement la valeur du brouillard (le brouillard doit être mis à jour chaque tour d’un bit par natio et par regio découverte *)
(* en l’occurence, chaque rv est marké chaque tour du bit du dominus du tour *)
(*appelé par Im.update uniquement*)


let dominus     rv = rv.dominus
let contents    rv = rv.contents
let incola      rv = match rv.contents with
  | Desertum_for n -> None 
  | Incol        i -> Some i
let incola_id   rv = match rv.contents with
  | Desertum_for n -> Nid.none 
  | Incol        i -> Incola.nid i

let plebs       rv = match rv.contents with
  | Desertum_for n -> 0. 
  | Incol        i -> Incola.plebs i
let brouillards rv = rv.brouillards

module Fun = struct
  let tegmen hydros climax contents = match contents, hydros with 
    | Incol i, _                -> Incola.tegmen climax (Incola.oikos i)
    | Desertum_for t, R.Dry
    | Desertum_for t, R.River _ ->  ( match climax with
        | _                      when t>=64 -> Desertum climax
        | R.Forest R.RainF       when t>=32 -> Desertum (R.Woodland R.TropicalF)
        | R.Forest f             when t>=32 -> Desertum (R.Woodland f)
        | R.Forest  R.RainF    
        | R.Woodland R.TropicalF when t< 32 -> Desertum (R.Grassland R.Savanna)
        | R.Forest _           
        | R.Woodland _           when t< 32 -> Desertum (R.Grassland R.OtherG) 
        | _                                 -> Desertum climax )
        
    | _                                              -> Hydros hydros
    (* tegmen function*)

  let silva physis tegmen = match tegmen with
    | Turbs
    | Tmine                   ->  0
    | Pasture 
    | Desertum R.Steppe       ->  4 + physis
    | Fields_and_pasture
    | Fields                 
    | Irrigation              -> 10 + physis / 2
    | Desertum (R.Grassland _)-> 10 + physis
    | Fields_and_woods        -> 12 + physis 
    | Desertum (R.Woodland _) -> 16 + physis
    | Desertum (R.Forest _)   -> 20 + physis
    | _                       ->  0 + physis
  (* densité forestière *)

  open Tfloat

  let chorability ?inst:(inst=0.) tegmen = match tegmen with
    | Desertum (R.Forest   R.Deciduous) 
    | Desertum (R.Forest   R.TropicalF) -> ((Std.cut 0. 100. (inst -  50.)) / 2.)
    | Desertum (R.Forest   R.RainF    ) -> ((Std.cut 0. 100. (inst - 100.)) / 2.)
    | Desertum (R.Woodland R.TropicalF) (* Mexique et Nouvelle Guinée sont agricoles en 4000 BCE *)
    | Desertum (R.Woodland R.Deciduous) (* Europe et Plateau andin sont agricoles en 4000 BCE *)
    | Desertum (R.Grassland _)
    | Desertum R.Steppe
    | Fields
    | Fields_and_woods
    | Fields_and_pasture 
    | Irrigation
    | Pasture     
    | Turbs        -> 100.
    | _           ->   0.

  let isFarmable ?inst:(inst=0.) tegmen = (chorability ~inst tegmen > 0.)
  (* == la regio est-elle susceptible d’appartenir à la chora ? *)
  (* Oui, is_farmable dépend du tegmen : couvert végétal réel, et non du climax *)

end


let instrumentum rv = match rv.contents with
| Desertum_for n -> 0.
| Incol        i -> Incola.instrumentum i


let tegmen ?rv:(rv=null) r = Fun.tegmen  (R.hydros r) (R.climax r) (contents rv)
let silva  ?rv:(rv=null) r = Fun.silva (R.physis r) (tegmen ~rv r ) 
let is_farmable ?rv:(rv=null) r = Fun.isFarmable ~inst:(instrumentum rv) (tegmen ~rv r )
let facultas r rv = Incola.facultas (R.area r) (R.hospitalitas r) (instrumentum rv) (tegmen ~rv r ) 

let densitas r rv = match rv.contents with 
| Desertum_for n -> 0.
| Incol        i -> Incola.densitas (R.area r) (i)

(*****************************************************************************************************************)


type passability =
| Passable
| Navigable
| Not_passable

let is_passable r rv = 
  match R.hydros r with
  | R.Ocean     -> Navigable
  | R.Dry       -> if   (match contents rv with Incol _ -> true | _ -> false) (* zone habitée *) 
                   ||   (R.pluvia r < 2000) (* pas rain forest *)
                   then Passable else Not_passable
  | R.River _   -> Passable (* en canoë *)
  | R.Sea       -> Passable
  | R.Lake      -> Passable
  | R.SeaIce    -> Not_passable
  | R.Inlandsis -> Not_passable

(*****************************************************************************************************************)

type gRegio = {
  superficies    : float;
  facultas       : float;
  hospitalitas   : float;
  instrumentum   : float;
  plebs          : float;
}

let gRegio rs r rvi = 
  let hospitalitas = R.hospitalitas r in
  let instrumentum = Incola.instrumentum rvi in
  let plebs        = Incola.plebs rvi in
  let tegmen       = Incola.tegmen (R.climax r) (Incola.oikos rvi) in
  {
  superficies = rs;
  facultas = Incola.facultas rs hospitalitas instrumentum tegmen;
  hospitalitas;
  instrumentum;
  plebs;
  }

(*EOF*)
