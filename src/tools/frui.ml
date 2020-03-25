(*
 * pending fixes :
 * - outdated_layout signal is incomplete (style, text contents)
 * - outdated_layout signal should be decomposed by window
 *)
open Printf
open React

let ( |> ) x f = f x
let ( % ) f g x = g (f x)
let cons l x = x :: l
let v x = S.value x
let add_opt x l = match x with
  | Some y -> y :: l
  | None -> l

let soio = function
  | None -> "none"
  | Some i -> sprintf "%d" i

let pp label s = printf "%s %s\n%!" label s

let bind_e s f = 
  let s' = S.map ~eq:E.equal f s in
  let init = S.value s'
  and changes = S.changes s' in
  E.switch init changes

type box = {
  left : float ;
  right : float ;
  top : float ;
  bottom : float
}

let string_of_box b = 
  sprintf
    "{ left = %g ; right = %g ; top = %g ; bottom = %g }"
    b.left b.right b.top b.bottom

let box_crop ~by b =
  let left = b.left +. by.left and right = b.right -. by.right
  and top = b.top +. by.top and bottom = b.bottom -. by.bottom in
  let left, right = if left < right then left, right else b.left, b.left
  and top, bottom = if top < bottom then top, bottom  else b.top, b.top in
  { left ; right ; top ; bottom }

let null_box = { left = 0. ; right = 0. ; top = 0. ; bottom = 0. }
let outside_box = { left = -1. ; right = -1. ; top = -1. ; bottom = -1. }

type size_spec = [
  `tight
| `fills
| `expands
| `fixed of float
]

let string_of_size_spec = function
  | `tight -> "tight"
  | `fills -> "fills"
  | `expands -> "expands"
  | `fixed f -> sprintf "fixed %.0g" f

type visibility = [ `invisible | `translucent | `opaque ]

type layout = [
| `hpack of halign spacing * valign
| `vpack of valign spacing * halign
| `overlay
]
and halign = [`left | `center | `right ]
and valign = [`top | `center | `bottom ]
and 'a spacing = [ `packed of 'a | `justified | `spread]

let string_of_halign = function
  | `left -> "left"
  | `right -> "right"
  | `center -> "center"

let string_of_valign = function
  | `top -> "top"
  | `bottom -> "bottom"
  | `center -> "center"

let string_of_spacing f = function
  | `packed x -> sprintf "packed(%s)" (f x)
  | `justified -> "justified"
  | `spread -> "spread"

let string_of_layout = function
  | `hpack (hsp, valign) ->
    sprintf "hpack(%s,%s)" (string_of_spacing string_of_halign hsp) (string_of_valign valign)
  | `vpack (vsp, halign) ->
    sprintf "vpack(%s,%s)" (string_of_spacing string_of_valign vsp) (string_of_halign halign)
  | `overlay -> "overlay"

let min_visibility x y = match x, y with
  | `invisible, _ -> `invisible
  | `translucent, `invisible -> `invisible
  | `translucent, _ -> `translucent
  | `opaque, x -> x

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

module Make(B : Backend) = struct

  type elt_id = int

  type element = {
    id : elt_id ;
    w : size_spec signal ;
    h : size_spec signal ;
    visibility : visibility signal ;
    position : box signal ;
    kind : kind ;
  }
  and kind = 
  | Label of label
  | Button of button
  | Div of div
  and label = {
    lb_text : string signal ;
    lb_style : B.Label.style signal ;
  }
  and button = {
    bt_text : string signal ;
    bt_style : B.Button.style signal ;
    bt_shortcut : B.Key.t option ;
    bt_alt_shortcut : B.Key.t option ;
    bt_fired : unit event ;
    bt_pressed : bool signal ;
  }
  and div = {
    div_children : element list ;
    div_layout : layout signal ;
    div_margin : box signal ;
    div_background : B.Div.background option signal ;
    div_framed : bool signal ;
  }
  type window = {
    win_id : B.Window.id ;
    win_contents : element signal ;
  }

  type mouse_event = [
  | `move
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

  let string_of_ui_event = function
    | `layout _ -> "layout"
    | `mouse_event _ -> "mouse_event"
    | `key_event _ -> "key_event"
    | `focus (wid,_) -> sprintf "focus %s" (B.Window.string_of_id wid)
    | `unfocus -> "unfocus"

  type t = {
    input : ui_event list event ;
    send_input : ui_event list -> unit ;
    windows : window list ;
    window_stack : B.Window.id list signal ;
    focus : (B.Window.id * element option) option signal ;
    outdated_layout : bool signal ;
  }

  module ElementEq = struct
    type 'a t = element
    let equal n n' = n.id = n'.id
  end
  module ES = React.S.Make(ElementEq)

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

  let newid = 
    let i = ref 0 in
    fun () -> incr i ; i.contents
      
  let k x = S.const x

  let wmap assoc default x =
    try snd (List.find (fun (k,v) -> x.id = k.id) assoc) 
    with Not_found -> default

  let wmapi assoc default x =
    try snd (List.find (fun (k,v) -> x = k.id) assoc) 
    with Not_found -> default

  let find_window ui wid =
    try List.find (fun w -> w.win_id = wid) ui.windows
    with Not_found -> Core.failwithf "Frui.find_window: unknown window %s" (B.Window.string_of_id wid) ()


  let toolkit ?initial_focus ui_input =
    let module Tk = struct

      let some = function
	| [] -> None
	| l -> Some l

      let fold f init input =
	S.fold (List.fold_left f) init input

      let keyboard_events =
      	let f = function
      	  | `key_event e -> Some e
      	  | _ -> None
      	in
	let g evts = match Core.List.filter_map evts ~f with
	  | [] -> None
	  | l -> Some l
	in
      	E.fmap g ui_input

      let key_presses ~on =
      	let f = function
      	  | ((key,`press), Some elt) -> 
	    if elt.id = on then Some key else None
      	  | _ -> None
      	in
      	E.fmap 
	  (fun x -> some (Core.List.filter_map x f))
	  keyboard_events

      let key_releases ~on =
      	let f = function
      	  | ((key,`release), Some elt) -> 
	    if elt.id = on then Some key else None
      	  | _ -> None
      	in
      	E.fmap 
	  (fun x -> some (Core.List.filter_map x f))
	  keyboard_events

      let mouse_events =
      	let f = function
      	  | `mouse_event e -> Some e
      	  | _ -> None
      	in
	let g evts = some (Core.List.filter_map evts ~f) in
      	E.fmap g ui_input

      let left_presses ~on =
      	let f = function
      	  | (_,_,`press `left,Some elt) -> elt.id = on
      	  | _ -> false
      	in
      	E.filter (List.exists f) mouse_events
	
      (* let left_presses = *)
      (* 	let f = function *)
      (* 	  | `left_press _ -> true *)
      (* 	  | _ -> false *)
      (* 	in *)
      (* 	E.filter (List.exists f) ui_input *)

      let all_left_releases =
      	let f = function
      	  | (_,_,`release `left,_) -> true
      	  | _ -> false
      	in
      	E.filter (List.exists f) mouse_events

      let elt_under_mouse id =
      	let upd old = function
      	  |(_,_,_,Some elt) -> elt.id = id
      	  | _ -> false
      	in
      	fold upd false mouse_events

      let fold_update f init update_event =
	let upd old x = match f x with
	  | Some y -> y
	  | None -> old
	in
	S.fold 
	  (List.fold_left upd)
	  init
	  update_event

      let position id =
	let upd old = function 
	  | `layout l -> wmapi l outside_box id
	  | _ -> old
	in
	fold upd outside_box ui_input

      let window_focus =
	let f = function
	  | `focus (wid,_) -> Some (Some wid)
	  | `unfocus -> Some None
	  | _ -> None
	in
	fold_update f initial_focus ui_input

      let label 
          ?(w = k `tight)
	  ?(h = k `tight)
          ?(visibility = k `opaque)
          ?(style = k B.Label.default)
          text =
	let id = newid () in

	{
	  id ; w ; h ; visibility ;

	  kind = Label {
	    lb_text = text ;
	    lb_style = style ;
	  } ;
	  
	  position = position id ;
	}

      let button 
          ?(w = k `tight) ?(h = k `tight) 
          ?(visibility = k `opaque)
          ?(style = k B.Button.default)
	  ?shortcut ?alt_shortcut
          label =
	let id = newid () in

	let mouse_pressed =
	  E.select [
            E.stamp (left_presses ~on:id) true ;
            E.stamp all_left_releases false ;
	  ]
	  |> S.hold false
	in
	let mouse_fired =
	  S.diff (fun after before -> before && not after) mouse_pressed
	  |> E.fmap (fun b -> if b then Some () else None)
	  |> E.when_ (elt_under_mouse id)
	in

	let key_pressed =
	  E.select [
	    E.stamp (key_presses ~on:id) true ;
	    E.stamp (key_releases ~on:id) false ;
	  ]
	  |> S.hold false
	in
	let key_fired =
	  S.diff (fun after before -> before && not after) key_pressed
	  |> E.fmap (fun b -> if b then Some () else None)
	in

	let bt_pressed = S.Bool.(mouse_pressed || key_pressed) in
	let bt_fired = E.select [ mouse_fired ; key_fired ] in
	{
	  id ; w ; h ; visibility ;

	  kind = Button {
	    bt_text = label ;
	    bt_style = style ;
	    bt_pressed ;
	    bt_fired ;
	    bt_shortcut = shortcut ;
	    bt_alt_shortcut = alt_shortcut ;
	  } ;
	  
	  position = position id ;
	},
	bt_pressed,
	bt_fired
   
      let div 
	  ?(layout = k (`vpack (`justified, `center)))
          ?(w = k `tight) ?(h = k `tight)
          ?(visibility = k `opaque) 
          ?(margin = k null_box)
	  ?(background = k None)
	  ?(framed = k false)
	  children =
	let id = newid () in

	{
	  id ; w ; h ; visibility ;
	  kind = Div {
	    div_children = children ;
	    div_layout = layout ;
	    div_margin = margin ;
	    div_background = background ;
	    div_framed = framed ;
	  } ;
	  position = position id ;
	}

      let hpack
	  ?(spacing = k `justified)
	  ?(valign = k `center)
	  ?w ?h ?visibility
	  ?margin ?background ?framed
	  children =
	div
	  ~layout:(S.l2 (fun x y -> `hpack (x, y)) spacing valign)
	  ?w ?h ?visibility ?margin ?background ?framed
	  children

      let vpack
	  ?(spacing = k `justified)
	  ?(halign = k `center)
	  ?w ?h ?visibility
	  ?margin ?background ?framed
	  children =
	div
	  ~layout:(S.l2 (fun x y -> `vpack (x, y)) spacing halign)
	  ?w ?h ?visibility ?margin ?background ?framed
	  children

      let overlay ?w ?h ?visibility children =
	div
	  ~layout:(k `overlay)
	  ?w ?h ?visibility children

      let void ?w ?h () =
	div ~visibility:(k `invisible) ?w ?h []

      let window win_id win_contents = {
	win_id ; win_contents
      }
	
    end
    in
    (module Tk : Toolkit)


  let collect xs f =
    List.fold_left (fun accu x -> (f x) @ accu) [] xs

  let collect2 xs ys f =
    List.fold_left2 (fun accu x y -> (f x y) @ accu) [] xs ys

  (* **************************************************************************************
     *** LAYOUT FUNCTIONS
     ************************************************************************************** *)
  let point_over (x, y) box =
    box.left <= x && x <= box.right && 
    box.top <= y && y <= box.bottom

  let center w x1 x2 =
    let scale = if w <= x2 -. x1 then 1. else (x2 -. x1) /. w in
    let space = (x2 -. x1 -. w *. scale) /. 2. in
    (x1 +. space, x2 -.space)

  let fix w h (x,y) = 
    (match w with `fixed x -> x | _ -> x),
    (match h with `fixed y -> y | _ -> y)

  let store_box walign halign w h box (wmin,hmin) =
    let w = match w with
	`tight -> wmin
      | `fills | `expands -> box.right -. box.left
      | `fixed w -> w
    and h = match h with
	`tight -> hmin
      | `fills | `expands -> box.bottom -. box.top
      | `fixed h -> h in
    let left, right = 
      match walign with 
	| `center -> center w box.left box.right
	| `left -> box.left, min (box.left +. w) box.right
	| `right -> max (box.right -. w) box.right, box.right
    and top, bottom = 
      match halign with 
	| `center -> center h box.top box.bottom
	| `top -> box.top, min (box.top +. h) box.bottom
	| `bottom -> max (box.bottom -. h) box.bottom, box.bottom
    in { left ; right ; top ; bottom }

  let div_vertical_fill_layout_ypos nyexpands box ws hs dims =
    let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. dims in
    let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
    let yspace = (box.bottom -. box.top -. total_h *. yscale) /. nyexpands in
    List.fold_right2
      (fun h (_,dim_h) (cur_y, accu) ->
  	let dy = dim_h *. yscale +. if h = `expands then yspace else 0. in
  	cur_y -. dy,
  	(cur_y -. dy, cur_y) :: accu)
      hs dims
      (box.bottom, [])
    |> snd

  let div_vertical_packed_layout_ypos valign box children_w children_h children_dims =
    let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. children_dims in
    let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
    let yspace = (box.bottom -. box.top -. total_h *. yscale) in
    let voffset = box.bottom -. match valign with
      | `top -> yspace
      | `center -> yspace /. 2.
      | `bottom -> 0.
    in
    List.fold_right
      (fun (_,h) (cur_y, accu) ->
  	cur_y -. h *. yscale,
  	(cur_y -. h *. yscale, cur_y) :: accu)
      children_dims
      (voffset, [])
    |> snd

  let div_vertical_justified_layout_ypos box ws hs dims =
    match List.length dims with
    | 0 -> []
    | 1 -> [ box.top, box.bottom ]
    | n ->
      let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. dims in
      let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
      let yspace = (box.bottom -. box.top -. total_h *. yscale) /. float (n - 1) in
      List.fold_right2
	(fun h (_,dim_h) (cur_y, accu) ->
  	  let pos_y = cur_y -. dim_h *. yscale in
  	  pos_y -. yspace,
  	  (pos_y, cur_y) :: accu)
	hs dims
	(box.bottom, [])
      |> snd

  let div_vertical_spread_layout_ypos box ws hs dims =
    let n = List.length dims in
    let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. dims in
    let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
    let yspace = (box.bottom -. box.top -. total_h *. yscale) /. float n in
    List.fold_right2
      (fun h (_,dim_h) (cur_y, accu) ->
  	let pos_y = cur_y -. dim_h *. yscale in
  	pos_y -. yspace,
  	(pos_y, cur_y) :: accu)
      hs dims
      (box.bottom -. yspace /. 2., [])
    |> snd

  let div_vertical_layout layout align bbox ws hs dims =
    let nyexpands = Core.List.count hs ~f:(( = ) `expands) in
    let ypositions =
      if nyexpands > 0 then
        div_vertical_fill_layout_ypos (float nyexpands) bbox ws hs dims
      else
        match layout with
        | `packed valign -> div_vertical_packed_layout_ypos valign bbox ws hs dims
	| `justified -> div_vertical_justified_layout_ypos bbox ws hs dims
	| `spread -> div_vertical_spread_layout_ypos bbox ws hs dims
    and xpositions =
      let pos = match align with
        | `center -> (fun w -> center w bbox.left bbox.right)
        | `left -> (fun w -> bbox.left, bbox.left +. w)
        | `right -> (fun w -> bbox.right -. w, bbox.right) in
      Core.List.map3_exn ws hs dims ~f:(fun w _ (dim_w,_) ->
	match w with
	| `expands | `fills -> bbox.left, bbox.right
	| `tight | `fixed _ -> pos dim_w
      )
    in
    List.map2
      (fun (left, right) (top,bottom) -> { left ; right ; top ; bottom })
      xpositions ypositions

  let div_horizontal_fill_layout_xpos nxexpands box ws hs dims =
    let total_w = List.fold_left (fun accuw (w,_) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) /. nxexpands in
    List.fold_right2
      (fun w (dim_w,_) (cur_x, accu) ->
  	let dx = dim_w *. xscale +. if w = `expands then xspace else 0. in
  	cur_x -. dx,
  	(cur_x -. dx, cur_x) :: accu)
      ws dims
      (box.right, [])
    |> snd

  let div_horizontal_packed_layout_xpos halign box children_w children_h children_dims =
    let total_w = List.fold_left (fun accuw (w,_) -> accuw +. w) 0. children_dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) in
    let hoffset = box.right -. match halign with
      | `left -> xspace
      | `center -> xspace /. 2.
      | `right -> 0.
    in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
  	cur_x -. w *. xscale,
  	(cur_x -. w *. xscale, cur_x) :: accu)
      children_dims
      (hoffset, [])
    |> snd

  let div_horizontal_justified_layout_xpos box ws hs dims =
    match List.length dims with
    | 0 -> []
    | 1 -> [ box.left, box.right ]
    | n ->
      let total_w = List.fold_left (fun accuw (w,_) -> accuw +. w) 0. dims in
      let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
      let xspace = (box.right -. box.left -. total_w *. xscale) /. float (n - 1) in
      List.fold_right2
	(fun w (dim_w,_) (cur_x, accu) ->
  	  let posx = cur_x -. dim_w *. xscale in
  	  posx -. xspace,
  	  (posx, cur_x) :: accu)
	ws dims
	(box.right, [])
      |> snd

  let div_horizontal_spread_layout_xpos box ws hs dims =
    let n = List.length dims in
    let total_w = List.fold_left (fun accuw (w,_) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) /. float n in
    List.fold_right2
      (fun w (dim_w,_) (cur_x, accu) ->
  	let posx = cur_x -. dim_w *. xscale in
  	posx -. xspace,
  	(posx, cur_x) :: accu)
      ws dims
      (box.right -. xspace /. 2., [])
    |> snd

  let div_horizontal_layout layout align bbox ws hs dims =
    let nxexpands = Core.List.count ws ~f:(( = ) `expands) in
    let xpositions =
      if nxexpands > 0 then
        div_horizontal_fill_layout_xpos (float nxexpands) bbox ws hs dims
      else
        match layout with
        | `packed halign -> div_horizontal_packed_layout_xpos halign bbox ws hs dims
	| `justified -> div_horizontal_justified_layout_xpos bbox ws hs dims
	| `spread -> div_horizontal_spread_layout_xpos bbox ws hs dims
    and ypositions =
      let pos = match align with
        | `center -> (fun h -> center h bbox.top bbox.bottom)
        | `top -> (fun h -> bbox.top, bbox.top +. h)
        | `bottom -> (fun h -> bbox.bottom -. h, bbox.bottom) in
      Core.List.map3_exn ws hs dims ~f:(fun _ h (_,dim_h) -> 
	match h with
	| `expands | `fills -> bbox.top, bbox.bottom
	| `tight | `fixed _ -> pos dim_h
      )
    in
    List.map2
      (fun (left, right) (top,bottom) -> { left ; right ; top ; bottom })
      xpositions ypositions

    

  let div_layout layout margin pos children_w children_h children_dims =
    let pos = box_crop ~by:margin pos in
    match layout with
    | `hpack (h, v) ->
      div_horizontal_layout h v pos children_w children_h children_dims
    | `vpack (v, h) ->
      div_vertical_layout v h pos children_w children_h children_dims
    | `overlay ->
      List.map (fun _ -> pos) children_dims

  let dimension_fun assoc = wmap assoc (0., 0.)
  let position_fun assoc = wmap assoc null_box

  let rec dimension_map elt = match elt.kind with
    | Button bt ->
      let dim = B.Button.size (v bt.bt_style) (v bt.bt_text) |> fix (v elt.w) (v elt.h) in
      [ elt, dim ]

    | Label lb ->
      let dim = B.Label.size (v lb.lb_style) (v lb.lb_text) |> fix (v elt.w) (v elt.h) in
      [ elt, dim ]

    | Div d ->
      let children_dim_map = collect d.div_children dimension_map in
      let children_dims = List.map (dimension_fun children_dim_map) d.div_children in
      let raw_dim =
	let fw, fh = 
	  match v d.div_layout with
	  | `hpack _ -> ( +. ), max
	  | `vpack _ -> max, ( +. )
	  | `overlay -> max, max
	in
      	List.fold_left
          (fun (accuw,accuh) (w,h) -> fw accuw w, fh accuh h)
          (0.,0.)
      	  children_dims
      in
      let dim =
	let (w,h) = raw_dim in
  	let mg = v d.div_margin in
  	fix (v elt.w) (v elt.h) (w +. mg.left +. mg.right, h +. mg.top +. mg.bottom)
      in
      (elt, dim) :: children_dim_map

  let rec position_map dim bbox elt =
    if v elt.visibility = `invisible then []
    else (
      let pos = store_box `center `center (v elt.w) (v elt.h) bbox (dim elt) in
      let children = match elt.kind with
	| Div d ->
  	  let children_w = List.map (fun elt -> v elt.w) d.div_children in
  	  let children_h = List.map (fun elt -> v elt.h) d.div_children in
  	  let children_dims = List.map dim d.div_children in
  	  let children_bboxes = 
	    div_layout (v d.div_layout) (v d.div_margin) pos children_w children_h children_dims
  	  in
	  collect2 children_bboxes d.div_children (position_map dim)

	| Button _ | Label _ -> []
      in
      (elt, pos) :: children
    )

  let fullscreen () =
    let (w,h) = v B.screen_size in
    { left = 0. ; right = w ; top = 0. ; bottom = h }

  let update_layout ui =
    let fullscreen = fullscreen () in
    let positions = 
      collect ui.windows (fun win ->
	let elt = v win.win_contents in
	let dims = dimension_map elt in
	let positions = position_map (dimension_fun dims) fullscreen elt in
	positions
      )
    in
    ui.send_input [`layout positions]

  let outdated_layout_signal ui_input windows =
    let true_ _ _ = true in
    let event_of_element e =
      E.select [
	S.diff true_ e.w ;
	S.diff true_ e.h ;
	S.diff true_ e.visibility ;
      ]
    in
    let event_of_window w =
      E.select [
	E.stamp (S.diff true_ w.win_contents) true ;
	bind_e w.win_contents event_of_element ;
      ]
    in
    E.select (
      List.append
	(List.map event_of_window windows)
	[ E.fmap 
	    (Core.List.find_map ~f:(function `layout _ -> Some false | _ -> None))
	    ui_input ]
    )
    |> S.hold true

(* **************************************************************************************
   *** MOUSE EVENT FUNCTIONS
   ************************************************************************************** *)

  let rec widget_under_mouse_signal pos mouse_pos widget =
    let accu x y = if x = None then y else x in
    let children_signals = collect_children_wum pos mouse_pos widget in
    let children_wum = S.merge accu None children_signals in
    S.l3
      (fun children mouse_pos pos ->
        if children <> None then children
        else if point_over mouse_pos pos then Some widget.id
        else None)
      children_wum mouse_pos (pos widget)
  and collect_children_wum pos mouse_pos widget = match widget.kind with
    | Label _ | Button _ (*| Void*) -> []
    | Div d ->
      List.map (widget_under_mouse_signal pos mouse_pos) d.div_children

  let element_under_mouse ui mouse_pos =
    let open Core in
    let is_visible e =
      S.value e.visibility <> `invisible 
      &&
      (match e.kind with
      | Div d -> v d.div_background <> None
      | _ -> true)
    in
    let rec traverse e = 
      let is_one_of_my_children = match e.kind with
	| Div d -> List.find_map d.div_children traverse
	| Button _ | Label _ -> None
      in
      if Option.is_some is_one_of_my_children then is_one_of_my_children
      else (
	if is_visible e && point_over mouse_pos (S.value e.position) then (
	  Some e
	)
	else
	  None
      )
    in
    List.find_map (S.value ui.window_stack) (fun wid ->
      let window = List.find_exn ui.windows ~f:(fun w -> w.win_id = wid) in
      traverse (S.value window.win_contents) 
      |> Core.Option.map ~f:(fun x -> wid, x)
    )

(* **************************************************************************************
   *** KEYBOARD INPUT FUNCTIONS
   ************************************************************************************** *)
  let find_key_event_target window key =
    let rec aux elt = match elt.kind with
      | Button b -> 
	if b.bt_shortcut = Some key || b.bt_alt_shortcut = Some key 
	then Some elt else None
      | Label _ -> None
      | Div d ->
	Core.List.find_map d.div_children aux
    in
    aux (v window.win_contents)


  let make ?initial_focus f =
    let input, send_input = E.create () in
    let windows, ui_events, output, window_focus =
      let define ui_events =
	let tk = toolkit ?initial_focus (E.merge ( @ ) []  [ input ; ui_events ]) in
	let module Tk = (val tk) in
	let windows, ui_events', output = f tk in
	ui_events', (windows, E.merge ( @ ) [] [ input ; ui_events' ], output, Tk.window_focus)
      in
      E.fix define
    in
    let window_stack =
      let update old = 
	function
	| `focus (wid,_) -> wid :: (List.filter ((<>) wid) old)
	| _ -> old
      in
      S.fold
	(List.fold_left update)
	(List.map (fun w -> w.win_id) windows)
	input
    in
    { windows ; input ; send_input ; window_stack ;
      outdated_layout = outdated_layout_signal input windows ;
      focus = S.map (Core.Option.map ~f:(fun x -> x, None)) window_focus },
    ui_events,
    output

  let rec print_element n =
    let space n = String.make n ' ' in
    let rec aux tab n = match n.kind with
      | Div d ->
	printf "%s+--div[%d,%s,%s,%s]\n" 
	  (space tab) n.id
	  (string_of_layout (v d.div_layout))
	  (string_of_size_spec (v n.w)) (string_of_size_spec (v n.h));
	List.iter (aux (tab + 5)) d.div_children ;
      | Button bt ->
	printf "%s+--button[%d] |%s|\n" (space tab) n.id (S.value bt.bt_text);
      | Label lb ->
	printf "%s+--label[%d] |%s|\n" (space tab) n.id (S.value lb.lb_text);
    in
    aux 0 n ;
    flush stdout
	  
  let rec draw_node n = 
    if S.value n.position == outside_box then ()
    else (
      match n.kind with
      | Div d -> 
	B.Div.draw (v d.div_background) (v d.div_framed) (v n.position) ;
	List.iter draw_node d.div_children
      | Button b ->
	B.Button.draw 
	  (v b.bt_style) 
	  (v b.bt_text) 
	  (v b.bt_pressed) 
	  (v n.position)
      | Label l ->
	B.Label.draw 
	  (v l.lb_style) 
	  (v l.lb_text) 
	  (v n.position)
    )

  let draw ui =
    if v ui.outdated_layout then update_layout ui ;
    List.iter
      (fun wid -> 
	draw_node (S.value (List.find (fun w -> w.win_id = wid) ui.windows).win_contents))
      (List.rev (S.value ui.window_stack))

  let mouse_input ui m_x m_y event =
    let elt_um = element_under_mouse ui (float m_x, float m_y) in
    (* Core.Std.Option.iter elt_um (fun (wid,elt) -> print_element (v (find_window ui wid).win_contents)) ; *)
    (* print_endline ( *)
    (*   Core.Std.Option.value_map elt_um  ~default:"none" ~f:(fun (wid, elt) -> *)
    (* 	Printf.sprintf "elt_um: %s %d" (B.Window.string_of_id wid) elt.id *)
    (*   ) *)
    (* ) ; *)
    ui.send_input (
      add_opt
	(
	  match event, elt_um with
	  | `press _, None -> Some `unfocus
	  | `press _, Some (wid,elt) -> Some (`focus (wid, Some elt))
	  | _ -> None
	)
	[
	`mouse_event (m_x,m_y,event,Core.Option.map elt_um ~f:snd) ;
	]
    ) ;
    elt_um <> None

  let keyboard_input ui key state =
    let target = 
      match v ui.focus with
      | None -> None
      | Some (wid, _) -> 
	let window = find_window ui wid in
	find_key_event_target window key
    in
    ui.send_input [ `key_event ((key,state), target) ] ;
    if target = None then false
    else true

end
