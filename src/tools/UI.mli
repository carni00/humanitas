(** 
    An encapsulation of widget-like components, which provides layout functionalities and
    a reactive (in the sense of Functional Reactive Programming) interface.

    How to use this module in a program
    ===================================

    The library means to use React signals and events as a replacement for 
    mutable values, at least as much as possible. 

*)

open React

type mouse_state = {
  m_left : bool ;
  m_middle : bool ;
  m_right : bool ;
  m_x : int ;
  m_y : int
}

type box = UUI.box = {
  left : float ;
  right : float ;
  top : float ;
  bottom : float
}

(* pour plus tard : type visibility = [ `visible | `translucent | `invisible ] *)
type state = [ `active | `frozen | `invisible | `inactive ]

(** Vertical centering specification (similar to LaTeX's). When
    several widgets are side to side, they can be aligned at their
    top, bottom or center. *)
type valign = [`top | `bottom | `centered]

(** Horizontal centering specification. When several widgets are on
    top of each other, they can be horizontally aligned on their left
    side, right side or center *)
type halign = [`left | `right | `centered]

(** Horizontal layout specification for a line of widgets. If there is
    not enough space for the widgets, they are scaled down to fit in
    the allowed space. If there are filling widgets in the line, they
    take all the (horizontal) space so as to leave others their
    required size only. If there only non-filling widgets, the
    specifications are as follows:
    - left_aligned: widgets with required size at max, grouped at the left side
    - right_aligned: widgets with required size at max, grouped at the right side
    - centered: widgets with required size at max, grouped at the center
    - centered_with_space: widgets with required size at max, with equal space around each to occupy all width
    - justified: widgets (proportionnaly stretched to occupy the whole width
    - justified_with_space: like centered_with_space, without spaces on left and right sides
*)
type hcentering = [
  `left_aligned 
| `right_aligned
| `centered
| `centered_with_space
| `justified
| `justified_with_space
]

(** Vertical layout specification for a column of widgets. If there is
    not enough space for the widgets, they are scaled down to fit in
    the allowed space. If there are filling widgets in the column, they
    take all the (verticalf) space so as to leave others their
    required size only. If there only non-filling widgets, the
    specifications are as follows:
    - top: widgets with required size at max, grouped at the top
    - bottom: widgets with required size at max, grouped at the bottom
    - centered: widgets with required size at max, grouped at the center
    - centered_with_space: widgets with required size at max, with equal space around each to occupy all height
    - justified: widgets (proportionnaly stretched to occupy the whole height
    - justified_with_space: like centered_with_space, without spaces at the top and the bottom of the box

*)
type vcentering = [
  `top
| `bottom
| `centered 
| `centered_with_space
| `justified
| `justified_with_space
]

(** Dimension specification (vertical or horizontal. 
    - [`tight]: a widget will reclaim the minimum space imposed by its contents, and draw itself centered in the space it is given
    - [`fills]: like [`tight] but will draw itself filling all the space it is given 
    - [`expands]: takes all the place there is by pushing other widgets away
    - [`fixed x]: fixed dimension in pixels
*)
type size_spec = [
  `tight
| `expands
| `fixed of float
]

module type Backend = 
sig
  type font

  module Key : sig
    type t
  end

  val screen_size : (float * float) signal

  val default_font : font
  val draw_text : 
    ?pos:halign -> 
    font -> string -> box -> unit
  val text_size : font -> string -> float * float

  val draw_rect : ?alpha:float -> Color.t -> box -> unit

  module Button : sig
    type style
    val default : style
    val size : style -> string -> float * float
    val draw : style -> string -> bool -> box -> unit
  end

  module Label : sig
    type style
    val default : style
    val size : style -> string -> float * float
    val draw : style -> string -> box -> unit
  end

  module Frame : sig
    type style
    val default : style
    val border_size : style -> box
    val draw : style -> bool -> box -> unit
  end
end

module Make(B : Backend) : sig
  type widget 

  val void :     
    ?w:size_spec signal -> ?h:size_spec signal ->
    unit -> widget

  val label : 
    ?w:size_spec signal -> 
    ?h:size_spec signal -> 
    ?state:state signal ->
    ?style:B.Label.style signal ->
    string signal -> widget

  val button :
    ?w:size_spec signal -> 
    ?h:size_spec signal -> 
    ?state:state signal ->
    ?style:B.Button.style signal ->
    ?shortcut:B.Key.t ->
    string signal -> widget * bool signal * unit event

  val frame : 
    ?w:size_spec signal -> 
    ?h:size_spec signal -> 
    ?state:state signal ->
    ?focus:[`focus | `unfocus] event ->
    ?style:B.Frame.style signal ->
    widget -> widget

  val hpack : 
    ?w:size_spec signal -> 
    ?h:size_spec signal -> 
    ?state:state signal ->
    ?layout:hcentering -> 
    ?align:valign ->
    widget list -> widget

  val vpack : 
    ?w:size_spec signal -> 
    ?h:size_spec signal -> 
    ?state:state signal ->
    ?layout:vcentering -> 
    ?align:halign ->
    widget list -> widget

  val overlay : 
    ?w:size_spec signal -> 
    ?h:size_spec signal ->
    ?state:state signal ->
    widget list -> widget
  (** overlay a::b::[] puts a over b *)

  type t
  val make : widget signal -> t
  val draw : t -> unit
  val mouse_button_event : 
    t -> 
    [`left | `middle|`right] -> 
    [`pressed|`released] -> 
    x:int -> y:int -> 
    bool
  val keyboard_event :
    t -> B.Key.t -> [`press|`release] -> bool
end

