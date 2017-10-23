(*

 ****************************** src/devel_ui/bitmap.mli ******************************


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

type t = {
  screen  : Sdlvideo.surface;
  picking : Sdlvideo.surface;
  }

(*val make : Status.t -> t*)
(** initialisation de l’écran ; renvoie un Bitmap.t *)

val make : Screen.t -> t
(** initialisation de l’écran ; renvoie un Bitmap.t *)

(*val update : t -> Status.t -> int32 -> t*Status.t*)
(** le cas échéant, mets à jour le bitmap en fonction du status, et annote le status de la mise à jour du bitmap*)

