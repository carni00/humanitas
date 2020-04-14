(*

 ****************************** src/devel_ui/handler.mli ******************************


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

open Humanitas_game

(*module NewTask : functor (Draw : Video.Draw) -> sig*)
(*  val get : unit -> Task.t list*)
(*end*)

type mods = {
  shift : bool;
  lalt : bool;
  ralt : bool;
  lctrl : bool;
  rctrl : bool;
  }


type event = 
| Mouse of [`press of ([`left | `middle | `right] as 'a) | `release of 'a] * int * int
| Key of Sdlkey.t * [`press | `release]

type t = mods * event

val next_event : unit -> t
val task_list  : Status.t -> (int -> int -> Picking.element option) -> t -> Task.t list

val mods : Sdlevent.keyboard_event option -> mods

val modulate_tasks : Task.t list -> Task.t list

