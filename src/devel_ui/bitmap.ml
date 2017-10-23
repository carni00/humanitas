(*

 ****************************** src/devel_ui/bitmap.ml ******************************


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

let make ss =
  let w,h,flags = if Screen.is_full ss
                  then Screen.iwip ss, Screen.ihip ss, [`DOUBLEBUF; `FULLSCREEN]
(*                  then Screen.frwip ss, Screen.frhip ss, [`DOUBLEBUF; `FULLSCREEN]*)
                  else Screen.iwip  ss, Screen.ihip  ss, [`DOUBLEBUF] in
  let screen = Sdlvideo.set_video_mode ~w:w ~h:h ~bpp:32 flags in
    {
    screen  = screen;
    picking = Sdlvideo.create_RGB_surface_format screen [] ~w:w ~h:h
    }
(** initialisation de l’écran ; renvoie un Bitmap.t *)

(*let make status = 
  make' (Status.screen status)*)

(*let update bitmap status no_color = 
(*  let bitmap,status =*)
(*    if Screen.is_upToDate (Status.screen status) then bitmap,status*)
(*    else make status, Status.update status [`note_screenUpdate] in*)
    (* mise à jour de la taille des bitmaps *)
  let _ = Sdlvideo.fill_rect bitmap.picking no_color in
    (* initialisation du bitmap picking ; on ajoute les éléments sur un bitmap vide *)
  bitmap, status*)
(** on update le bitmap au début de chaque cycle avant l’affichage *)

