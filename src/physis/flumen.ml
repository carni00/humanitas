(*

 ****************************** flumen.ml ******************************


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
module E = Espace

type regio = {
  rid : Rid.t;
  direction   : E.direction; (*position relative de l’aval*)
  dest : Rid.t; (*regio située en aval *)
  fluxus : int; (*flux/débit*)
  }

type t = 
  {
  fid : int; (*identifiant : sert à retrouver le fleuve dans lequel un affluent se jette, dans la génération des
  fleuves*)
  cours : regio list; (*de l’aval à l’amont*)
  origine : origine;
  fin : fin;
  }
(* un flumen.t est en réalité un fleuve ou un segment de fleuve (cas d’un fleuve ayant des affluents*)

and origine =
| Source
| Affluents of t list
(* les affluents s’entendent de tous les cours d’eaux, sans exception, qui se jette dans le confluent. On ne
distingue pas le « fleuve principal » de ses « affluents » au sens commun *)

and fin =
| Estuaire  of Rid.t (* la case d’océan dans laquelle un fleuve se jette*)
| Confluent of Rid.t (* la première case du fleuve dans lequel un affluent se jette, soit graphiquement la regio de
confluent*)
| NoF (*situation initiale lors de génération d’un fleuve*)

let len flumen = Tlist.len (flumen.cours) (*longueur d’un flumen = segment de fleuve *)

let uncomputedfluxus = -65535 (*valeur initiale du débit du fleuve*)

let regioMake rid dir dest = {
  rid = rid;
  direction = dir;
  dest = dest;
  fluxus = uncomputedfluxus;
  }

let null = {
  fid = 0;
  cours = [];
  origine = Source;
  fin = NoF;
  }

let make id rid dir dest = {
  fid = id;
  cours = [regioMake rid dir dest];
  origine = Source;
  fin = NoF;
  }
  
let add    flumen rid dir dest = { flumen with cours=(regioMake rid dir dest)::flumen.cours } (*allongement du fleuve vers
l’aval *)
let setEstuaire  flumen rid    = { flumen with fin=Estuaire  rid }
let setConfluent flumen rid fid= { flumen with fin=Confluent rid; fid=fid }
(* on donne à ce flumen-affluent l’id du flumen qu’il rejoint *)


let rec cut tList qList rid = 
  match qList with
  | [] -> raise (Failure "Flumen.cut")
  | t::q when t.rid=rid -> List.rev (t::tList), q
  | t::q -> cut (t::tList) q rid
(* coupe une liste de rid à une rid précise, la rid en question revient à l’aval = tList *)


let cat flumen nvlAffluent =
  let confluent = match nvlAffluent.fin with Confluent rid -> rid | _ -> raise (Failure "Flumen.cat") in
  (*flumenList.create appelle cat après un setConfluent *)
  let rec f flumen =
    match (List.exists (fun regio-> (regio.rid=confluent)) flumen.cours) with
    (* il s’agit de trouver quel segment du fleuve il faut découper pour y insérer le nvl affluent*)
    | true -> let aval, amont = cut [] flumen.cours confluent in
              ( match amont, flumen.origine with
              | [],Affluents affs -> 
                     { 
                     flumen with
                     origine = Affluents (nvlAffluent::affs)
                     }
                     (* cas particulier : l’affluent se jette dans un confluent existant (assez fréquent) *)
              | [],Source -> { 
                     flumen with
                     origine = nvlAffluent.origine;
                     cours   = flumen.cours ^^ nvlAffluent.cours;
                     }
                     (* cas particulier : l’affluent se jette dans la source du flumen (improbable,mais..) *)
              | _ -> {
                     flumen with
                     cours = aval;
                     origine = Affluents [ { 
                               flumen with
                               cours = amont;
                               fin = Confluent confluent;
                               };
                               nvlAffluent ];
                     (* cas ordinaire : nouveau confluent créé par le nvel affluent*)
                     } )
    | false -> match flumen.origine with
               | Affluents affs -> { flumen with origine = Affluents (List.map f affs) } (*Caml rules*)
               | Source -> flumen
  in f flumen
(* insère un nouvel affluent à un flumen existant, à la rid confluent *)

let fluxusUpdate rs thermos pluvia flumen =
  let newCours initFluxus cours =
    let rec g prevFluxus newCours aval = match aval with
    | []      -> newCours (*liste complète de l’aval à l’amont*)
    | r::aval -> let i=r.rid in
                 let newFluxus  = max 0 (prevFluxus + ((pluvia i - 12*thermos i) * (rs i / 100) / 300)) in
                 (* 1000 mm annuel sur une regio de 100*100 km génèrent 333 m3/sec *)
                 (* actualNettPrec est négatif dans les déserts, les fleuves peuvent y voir leur débit réduit à
                 zéro*)
                 g newFluxus ({r with fluxus=newFluxus}::newCours) aval in
    g initFluxus [] (List.rev cours) in
    (* définit le flux le long d’un cours pour un segment de fleuve, avec un flux initial *)
  let rec f flumen = 
    match flumen.origine with
    | Source         -> { flumen with cours = newCours 0 (flumen.cours) }
    | Affluents affs -> 
      let rec h sum ucal cal(*unComputedAffList*) affs = match affs with
      | []   -> sum, ucal, cal
      | a::q -> let aFluxus = (List.hd a.cours).fluxus in (*la tête du cours est l’aval*)
                let nucal, ncal = if aFluxus=uncomputedfluxus then (a::ucal,cal) else (ucal,a::cal) in
                h (sum+aFluxus) nucal ncal q in
      (* calcule la somme des flux finaux des affluents, et relève s’ils ont été calculés *)
      let fluxSum, ucal, cal = h 0 [] [] affs in 
      match ucal with
      | [] -> { flumen with cours = newCours fluxSum (flumen.cours) }
      | _  -> let computedAffs = ((List.map f ucal)^^cal) in
              let fluxSum = List.fold_left (fun s a -> s+(List.hd a.cours).fluxus) 0 computedAffs in
              { flumen with origine = Affluents computedAffs;
                            cours   = newCours fluxSum flumen.cours } in
  f flumen
(* calcule le débit pour tout le cours d’un fleuve et de ses affluents *)

let rec iter g flumen =
  (match flumen.origine with Affluents affs -> List.iter (iter g) affs | Source -> ());
  List.iter g flumen.cours
(* iter sur chaque Flumen.regiones du flumen et de ses affluents *)

(* EOF *)
