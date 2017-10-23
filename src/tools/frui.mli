open React

type box = {
  left : float ;
  right : float ;
  top : float ;
  bottom : float
}

type size_spec = [
  `tight
| `fills
| `expands
| `fixed of float
]

type visibility = [ `invisible | `translucent | `opaque ]

type layout = [
| `hpack of halign spacing * valign
| `vpack of valign spacing * halign
| `overlay
]
and halign = [`left | `center | `right ]
and valign = [`top | `center | `bottom ]
and 'a spacing = [ `packed of 'a | `justified | `spread]

module type Backend = sig
  module Key : sig
    type t
  end

  val screen_size : (float * float) signal

  module Label : sig
    type style
    val default : style
    val size : style -> string -> float * float
    val draw : style -> string -> box -> unit
  end

  module Button : sig
    type style
    val default : style
    val size : style -> string -> float * float
    val draw : style -> string -> bool -> box -> unit
  end

  module Div : sig
    type background
    val draw : background option -> bool -> box -> unit
  end

  module Window : sig
    type id
    type style
    val default : style
    val string_of_id : id -> string
  end
end

module Make(B : Backend) : sig
  type element
  type window

  module ES : React.S.S with type 'a v = element

  module type Toolkit = sig
    val window_focus : B.Window.id option signal
    val label : 
      ?w:size_spec signal ->
      ?h:size_spec signal ->
      ?visibility:visibility signal ->
      ?style:B.Label.style signal ->
      string signal -> element
    val button : 
      ?w:size_spec signal -> 
      ?h:size_spec signal ->
      ?visibility:visibility signal ->
      ?style:B.Button.style signal ->
      ?shortcut:B.Key.t ->
      ?alt_shortcut:B.Key.t ->
      string signal -> element * bool signal * unit event
    val div : 
      ?layout:layout signal ->
      ?w:size_spec signal -> ?h:size_spec signal ->
      ?visibility:visibility signal ->
      ?margin:box signal ->
      ?background:B.Div.background option signal ->
      ?framed:bool signal ->
      element list -> element
    val hpack : 
      ?spacing:halign spacing signal ->
      ?valign:valign signal ->
      ?w:size_spec signal -> ?h:size_spec signal ->
      ?visibility:visibility signal ->
      ?margin:box signal ->
      ?background:B.Div.background option signal ->
      ?framed:bool signal ->
      element list -> element
    val vpack : 
      ?spacing:valign spacing signal ->
      ?halign:halign signal ->
      ?w:size_spec signal -> ?h:size_spec signal ->
      ?visibility:visibility signal ->
      ?margin:box signal ->
      ?background:B.Div.background option signal ->
      ?framed:bool signal ->
      element list -> element
    val overlay : 
      ?w:size_spec signal -> ?h:size_spec signal ->
      ?visibility:visibility signal ->
      element list -> element
    val void :
      ?w:size_spec signal -> 
      ?h:size_spec signal -> 
      unit -> element
    val window :
      B.Window.id -> element signal -> window
  end

  type t

  type mouse_event = [
    `move 
  | `press of ([`left | `middle | `right] as 'a) 
  | `release of 'a
  ]

  type key_event = B.Key.t * [ `press | `release ]

  type ui_event = [
  | `layout of (element * box) list
  | `mouse_event of int * int * mouse_event * element option
  | `key_event of key_event * element option
  | `focus of B.Window.id * element option
  | `unfocus
  ]

  val string_of_ui_event : ui_event -> string

  val make :
    ?initial_focus:B.Window.id ->
    ((module Toolkit) -> window list * ui_event list event * 'a) ->
    t * ui_event list event * 'a

  val draw : t -> unit
  val print_element : element -> unit
  val mouse_input : 
    t -> 
    int -> int -> 
    mouse_event ->
    bool
  val keyboard_input :
    t ->
    B.Key.t ->
    [`press | `release] ->
    bool
end
