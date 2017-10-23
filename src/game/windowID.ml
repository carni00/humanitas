(*

 ****************************** WindowID.ml ******************************


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

type t = 
| Towers
| Artes
| Chora
| Computing
| Consilium
| ContactusMx
| Display
| Dx
| Game
| Help
| Humanitas
| Imperium
| Keys
| Mouse
| Natio 
| NewGame
| Orbis
| Partitio
| Polis
| Pyramid
| Quit
| Regio
| TaskHistory
| Tactics
| Time
| Fines
(* queens_and_sheets *)

let queens_and_sheets = [
  Artes;
  Chora;
  Computing;
  Consilium;
  ContactusMx;
  Display;
  Dx;
  Game;
  Help;
  Humanitas;
  Imperium;
  Keys;
  Mouse;
  Natio ;
  NewGame;
  Orbis;
  Partitio;
  Polis;
  Pyramid;
  Quit;
  Regio;
  Tactics;
  TaskHistory;
  Time;
  Fines
]
(* toutes sauf towers *)

type position =
| Default
| Alter
| Left
| Right
| Central
| Valid

type status =
| Alive     (* pleine existence : visible et réactif *)
| Invisible  (* invisible, mais réactif clavier *)
| Frozen     (* non réactif, mais visible *)
| Glass      (* non réactif et transparent *)
| Nil        (* aucune existence physique *)


type duty =
| Queen
(* les fenetres du milieu de l’écran *)
| Tower
(* les fenetres-boutons *)
| Sheet
(* les pages composant les piles de droite et gauche *)

let duty = function
| Towers -> Tower
| Computing
| Quit
| Time
| Help
| NewGame
| Display
| ContactusMx
| Game   -> Queen
| _ -> Sheet

let defSide = function
| Artes
| Chora
| Keys
| Mouse
| Orbis
| Partitio
| Pyramid
| Dx
| Regio
| Fines
| Tactics -> Right
| TaskHistory 
| Game
| Natio 
| Imperium
| Consilium
| Polis
| Humanitas
| _ -> Left
(* position par défaut des sheets *)


type tower =
| Esc
| Filter
| Save
| Coords
| Fps
| Gate of t (*towerButton servant à ouvrir d’autre window*)
(* towers *)

let topLeftTowers = [
  Esc;
  Gate Help;
  Gate Game;
  Gate Display;
  Filter;
  Gate Regio;
  ]

let topCentralTowers = [
  Gate Natio;
  ]

let topRightTowers = [
  Gate Imperium;
  Gate Consilium;
  Gate Polis;
  Gate Orbis;
  Gate Humanitas;
  Save ;
  ]

let bottomTowers = [
  Gate TaskHistory ;
  Gate Time ;
  Fps ;
  Coords
  ]

let towers = topLeftTowers^^topCentralTowers^^topRightTowers^^bottomTowers
let queens = List.filter (fun w -> duty w = Queen) queens_and_sheets
let sheets = List.filter (fun w -> duty w = Sheet) queens_and_sheets
