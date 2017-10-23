(*

 ****************************** video.ml ******************************


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
open Batteries
open React
    
module Make(X : sig end) = struct

  let screen_geometry, set_screen_geometry =
    let s, f = S.create (Screen.create ()) in
    s, fun x -> f x

  let surface_of_screen ss = 
    let w,h,flags = 
      if Screen.is_full ss
      then Screen.frwip ss, Screen.frhip ss, [`DOUBLEBUF; `FULLSCREEN]
      else Screen.iwip  ss, Screen.ihip  ss, [`DOUBLEBUF] 
    in
    Sdlvideo.set_video_mode ~w:w ~h:h ~bpp:32 flags

  let mouse_state () = 
    let open Sdlmouse in
    let (x,y,bs) = get_state () in
    { UUI.m_x = x ; m_y = y ;
      m_left = List.mem BUTTON_LEFT bs ;
      m_middle = List.mem BUTTON_MIDDLE bs ;
      m_right = List.mem BUTTON_RIGHT bs ; }

  let screen_surface = S.map ~eq:( == ) surface_of_screen screen_geometry

  let flip () = 
    Sdlvideo.flip (S.value screen_surface) ;
    Sdlvideo.fill_rect (S.value screen_surface) Color.(to_sdlvideo (gray 240))

  module Key = struct
    type t = Sdlkey.t
  end

  type font = Sdlttf.font

  let default_font = Sdlttf.open_font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 15

  let width  = S.map (Screen.iwip |- float) screen_geometry
  let height = S.map (Screen.ihip |- float)  screen_geometry
  let screen_size = S.map (fun g -> (Screen.iwip |- float) g,  (Screen.ihip |- float) g) screen_geometry

  let text_position pos {Frui.left ; right ; top ; bottom} w h = 
    let x = match pos with
      | `centered -> (left +. right -. w) /. 2.
      | `left     -> left
      | `right    -> right -. w
    and y = (top +. bottom -. h) /. 2. in
    int_of_float x, int_of_float y
      
  let rect_of_box {Frui.left ; right ; top ; bottom} = 
    Sdlvideo.rect (iof left) (iof top) (iof (right -. left)) (iof (bottom -. top))
      
  let draw_text ?(pos = `centered) font msg box = 
    let open Sdlvideo in 
    let dst = S.value screen_surface in
    let msg_surf = Sdlttf.render_text_blended font ~fg:Color.(to_sdlttf white) msg in
    let { w ; h } = surface_info msg_surf in
    let x, y = text_position pos box (float w) (float h) in
    let dst_rect = rect x y 0 0 in
    set_clip_rect dst (rect_of_box box) ;
    blit_surface ~src:msg_surf ~dst ~dst_rect () ;
    unset_clip_rect dst

  let text_size font msg =
    let x, y = Sdlttf.size_text font msg in
    float x, float y

  let draw_rect ?alpha co box =
    let open Sdlvideo in 
    let dst = S.value screen_surface in
    fill_rect ~rect:(rect_of_box box) dst (Color.to_sdlvideo co)
      
  module Button = struct
    type style = unit

    let default = ()
    let size style label =
      text_size default_font label

    let draw style label pressed box =
      draw_rect (if pressed then Color.yellow else Color.red) box ;
      draw_text default_font label box

  end

  module Label = struct
    type style = unit

    let default = ()
    let size style text =
      text_size default_font text
    let draw style text box =
      draw_text default_font text box
  end

  module Div = struct
    type background = Color.t
    let border_size _ = { Frui.left = 0. ; right = 0. ; top = 0. ; bottom = 0. }

    let draw background focus box =
      let open Frui in
      match background with
      | None -> ()
      | Some bg ->
	draw_rect (if focus then Color.blue else bg) box ;
	draw_rect bg { left = box.left +. 2. ; right = box.right -. 2. ; 
		       top = box.top +. 2. ; bottom = box.bottom -. 2. }
  end

  module Window = struct
    type id = [`topbar]
    type style = unit
    let default = ()
    let string_of_id = function
      | `topbar -> "topbar"
      | `central_block -> "central_block"
  end
end

