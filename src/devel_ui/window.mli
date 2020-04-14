(*

 ****************************** window.mli ******************************


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
open Humanitas_game

type title = string React.signal
type button = Sdlkey.t * string * Task.t list

type agencement = 
| Columns 
| Lines of (Frui.valign Frui.spacing * Frui.halign)

type element =
| Rect of (float * float * Color.t)
| LB of button
| SB of button
| S of string
| Z of string React.signal
| Box of (float * float * Frui.halign * element)
| List of (agencement * element list)

type t = title * element

val data  : Status.t React.signal -> WindowID.t -> t
val natio : Status.Atelier.t -> Nid.t -> WindowID.t -> t

val regio : Status.Atelier.t (*React.signal*) -> Rid.t (*React.signal*) -> t
(*val polis : Status.Atelier.t (*React.signal*) -> Nid.t (*React.signal*) -> t*)
(*val pyramid : Status.Atelier.t (*React.signal*) -> Nid.t (*React.signal*) -> t*)
val atelier : Status.Atelier.t (*React.signal*) -> WindowID.t -> t

(*
type sheetItem =
| Button of Button.t
| Rect of float (*rapport entre la w du rect et la w de la colonne*)
| S of string

type lineType =
| BL 
| SL of (Co.Nil.t option)

type sheetMx = {
  lineList : (lineType * (sheetItem list)) list;
  caList   : (Widget.halign*int*(Co.Nil.t option)) list; (*column aspect list*)
  }

type windowContents = 
| NoC
| Mx of sheetMx

type windowTitle =
| NoT
| SheetTitle of string

type windowWidth =
| A1 (*tout l’écran*)
| A4 (*sheet standart*)
| A3 (*double sheet gauche/droite*)

type t = {
  name : WindowID.t;
  title : windowTitle;
  color : Co.Nil.t;
  contents : windowContents;
  }

val create : Orbis.t -> Status.t -> WindowID.t -> t
val hil    : t -> int (*height in lines*)
val width  : WindowID.t -> windowWidth
val laList : t -> lineType list
*)
