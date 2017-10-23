(*

 ****************************** Video.mli ******************************


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

type video_info 
type fontFun = (Font.id -> int -> Sdlttf.font)
(** function that associates a SDL font to a font style and a font size *)

val video_info : Screen.t -> video_info
val print_info : video_info -> unit

val make_fontFun  : Font.id list -> fontFun
(** returns a font function that support the fonts of the given list *)

module type Draw = sig
  include Frui.Backend with
			 type Key.t = Sdlkey.t
		         and type Div.background = Color.t
		         and type Window.id = WindowID.t


  type xAlign = Left | Middle | Right   | Left_or_right
  type yAlign = Top  | Medium | Bottom  | Top_or_bottom

  val screen_geometry : Screen.t React.signal
  val set_screen_geometry : Screen.t -> unit
  val ycorratio : float React.signal
  val swip : int React.signal
  val ship : int React.signal
  val swie : int React.signal
  val shie : int React.signal
  val ewip : float React.signal
  val ehip : float React.signal

  val clear_screen : unit -> unit
  val clear_picking: unit -> unit
  val get_element: int -> int -> Picking.element option

  val resolution : unit -> unit
  val scale      : int  -> unit
  val viseur     : int -> int -> unit

  val strnDim    : Font.id -> int -> string -> int * int
  val strn       : ?pe:Picking.element -> ?xd:int -> ?yd:int -> ?xAlign:xAlign -> ?yAlign:yAlign -> ?font:Font.id -> ?size:int ->
  ?co:Color.t -> string -> int -> int -> unit
  val antiStrn   : ?pe:Picking.element -> ?xd:int -> ?yd:int -> ?xAlign:xAlign -> ?yAlign:yAlign -> ?font:Font.id -> ?size:int -> string -> int -> int -> unit
  
  val fill_rect  : ?pe:Picking.element -> ?a:float -> Color.t -> int -> int -> int -> int -> unit
  val line       : ?pe:Picking.element -> ?a:float -> Color.t -> int -> int -> int -> int -> unit
  val thick_line : ?pe:Picking.element -> ?a:float -> Color.t -> int -> int -> int -> int -> int -> unit
  val rect       : ?pe:Picking.element -> ?a:float -> Color.t -> int -> int -> int -> int -> int -> unit
  
  val load_bmp   : string -> Sdlvideo.surface
  val fill_bmp   : ?a:float -> ?sw:int -> ?sh:int -> ?sx:int -> ?sy:int -> Sdlvideo.surface -> int -> int -> unit
 
  val flip       : unit -> unit
  val mouse_state : unit -> UUI.mouse_state
end

module Draw(X : sig end) : Draw
