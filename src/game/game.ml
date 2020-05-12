(*

 ****************************** src/game/game.ml ******************************


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
module Nil = Nid.Nil
module Til = Tid.Til

module Player = struct
  
  type role = 
  | Admin
  (* administrateur de la partie *)
  | Basileus of Nid.t
  (* joueur ordinaire *)
  | Civis    of Nid.t
  (* citoyen : voit le monde à travers les yeux d’une nation, mais ne peut intervenir. Cas des princes déchus,
  notamment *)
  | Spectator
  (* spectateur : voit le monde comme un administrateur, mais ne détient aucun pouvoir *)
  (* roles qui peuvent être occupé par une personne qui joue à Humanitas = interagit avec un exécutable Humanitas *)
  
  type id = t Tid.t
  and  t  = 
    {
    name : string;
    role : role;
    pov  : Nid.t;
    }

  let name p = p.name
  let role p = p.role
  let pov  p = p.pov 
  let make name role = 
    {
    name;
    role;
    pov = match role with
    | Basileus nid -> nid
    | Civis    nid -> nid
    | Spectator    -> Nid.none
    | Admin        -> Nid.none
    }

end

(*********************************************)

type id = t Tid.t

and  t = {
  orbis     : Orbis.t ;
  players   : (Player.t) Til.t ;
  strategies: StrategicaList.t
}
(* les stratégies ne sont pas mixés aux players pour deux raisons :
- les poleis, qui mettent en oeuvre une stratégie, ne sont pas nécessairement monarchiques 
- un player n’est pas nécessairement un monarque *)


let orbis       g = g.orbis
let strategies  g = g.strategies
let players     g = g.players

let first_pid   g = Til.first_id g.players

type laws = {
  resolution : Espace.resolution;
(*  playerList : Player.t list;*)
  }
(** caractères fondamentaux d'une partie *)

let alter_player_pov players pid nid = Til.alter players pid (fun p -> { p with Player.pov = nid } )

let create laws =
  let orbis     = Orbis.create (Espace.Cylinder Espace.Octal) laws.resolution in
  let nil       = orbis.Orbis.natioIdList in
  let strategies= StrategicaList.make nil in
  let admin     = Player.make "Carnifex" Player.Admin (*Tlist.last nil*) in
  let players   = Til.add (Til.empty) admin in
  {
  orbis;
  players;
  strategies;
  }


let get_player game pid = Til.nth game.players pid

let get_player_pov game pid = Player.pov (Til.nth game.players pid)

let alter_player_pov game pid nid = { game with players = alter_player_pov game.players pid nid }


let cycle game =
  let orbis = Orbis.update game.orbis game.strategies in
  let strategies = StrategicaList.update game.strategies orbis.Orbis.natioList orbis.Orbis.proximaeList orbis.Orbis.sd in
  (* on ne peut envoyer orbis à StrategicaList : ref cyclique *)
  {
  game with
  orbis   = orbis;
  strategies = strategies;
  }
(* mise à jour orbis en fonction des stratégies, 
puis calcul des stratégies en fonction de l'orbis (stratégie par défaut pour les players humains) *)

let update_orbis game int = Ext.applique cycle game int

let update_orbis_till_next_eventum game0 =
  let rec test game = match game.orbis.Orbis.addendum with
  | [] -> test (cycle game)
  | _  -> game in
  test (cycle game0)

(* on passe un tour, puis on teste s’il y a des eventi, sinon on repasse un tour ... *)

