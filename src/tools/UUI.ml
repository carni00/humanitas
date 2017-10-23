open React
open Batteries
open Printf

type mouse_state = {
  m_left : bool ;
  m_middle : bool ;
  m_right : bool ;
  m_x : int ;
  m_y : int
}

(* BOX DATATYPE *)
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

let print_box b = 
  print_string (string_of_box b)

(* STATE DATATYPE *)
type state = [ `active | `frozen | `invisible | `inactive ]
let state_clip ~(parent : state) (x : state) = match parent, x with
  | `active, _ -> x
  | `frozen, `active -> `frozen
  | `frozen, _ -> x
  | `invisible, `inactive -> `inactive
  | `invisible, _ -> `invisible
  | `inactive, _ -> `inactive

let visible_state = function
  | `active | `frozen -> true
  | `invisible | `inactive -> false

(* alternative STATE DATATYPE 
type state = [ 
| `alive     (* pleine existence : visible et réactif *)
| `invisible  (* invisible, mais réactif clavier *)
| `frozen     (* non réactif, mais visible *)
| `glass      (* non réactif et transparent *)
| `nil        (* aucune existence physique *)
]

let state_clip ~(parent : state) (x : state) = match parent, x with
  | `alive, _ -> x
  | `invisible, `alive -> `invisible
  | `invisible, `frozen -> `glass
  | `invisible, _ -> x
  | `frozen, `alive    -> `frozen
  | `frozen, `invisible -> `glass
  | `frozen, _ -> x
  | `glass, `nil -> `nil
  | `glass, _ -> `glass
  | `nil, _ -> `nil

let visible_state = function
  | `alive | `frozen -> true
  | `invisible | `glass | `nil -> false

*)

(* ALIGNMENT DATATYPE *)
type valign = [`top | `bottom | `centered]
type halign = [`left | `right | `centered]

type hcentering = [
  `left_aligned 
| `right_aligned
| `centered 
| `centered_with_space
| `justified
| `justified_with_space
]

type vcentering = [
  `top
| `bottom
| `centered 
| `centered_with_space
| `justified
| `justified_with_space
]


(* SIZE SPECIFICATION *)
type size_spec = [
  `tight
| `fills
| `expands
| `fixed of float
]

let fills = function 
  | `expands | `fills -> true
  | _ -> false




let newid = 
  let i = ref 0 in
  fun () -> incr i ; i.contents

let c x = S.const x

let bind_s s f = S.switch (S.map f s)

let bind_e s f = 
  let s' = S.map ~eq:( == ) f s in
  let init = S.value s'
  and changes = S.changes s' in
  E.switch init changes

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

module Make(B : Backend) = struct


  (* GENERIC WIDGET PROPERTIES *)
  type props = {
    w : size_spec ;
    h : size_spec ;
    state : state ;
  }



  (* Custom type for primitive events/signals *)
  type 'a event_ = < event : 'a event ; send : 'a -> unit >
  type 'a signal_ = < signal : 'a signal ; send : 'a -> unit >
  let event_create () : 'a event_ = 
    let e, f = E.create () in
    object
      method event = e
      method send x = f x
     end

  let signal_create (x : 'a) : 'a signal_ = 
    let s, f = S.create x in
    object
      method signal = s
      method send x = f x
    end

  (* WIDGET DATATYPE *)
  type widget = {
    id : int ;
    props : props signal ;
    kind : kind ;
    dim : (float * float) signal ;
    bbox : box signal ;
    pos : box signal ;
  }
  and kind = 
    | Void
    | Label of label
    | Button of button
    | Frame of frame
    | HPack of hpack
    | VPack of vpack
    | Overlay of widget list
    | WSignal of widget signal
  and button = {
    bt_text : string signal ;
    bt_style : B.Button.style signal ;
    bt_shortcut : B.Key.t option ;
    bt_fire : unit event_ ;
    bt_pressed : bool signal_ ;
  }
  and hpack = {
    hp_layout : hcentering ;
    hp_align : valign ;
    hp_children : widget list ;
  }
  and vpack = {
    vp_layout : vcentering ;
    vp_align : halign ;
    vp_children : widget list ;
  }
  and frame = {
    fr_child : widget ;
    fr_style : B.Frame.style signal ;
    fr_focus_request : bool signal ;
  }
  and label = {
    lb_text : string signal ;
    lb_style : B.Label.style signal ;
  }

  type t = {
    topwidget : widget ;
    focus : int option signal ;
  }

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
	| `centered -> center w box.left box.right
	| `left -> box.left, min (box.left +. w) box.right
	| `right -> max (box.right -. w) box.right, box.right
    and top, bottom = 
      match halign with 
	| `centered -> center h box.top box.bottom
	| `top -> box.top, min (box.top +. h) box.bottom
	| `bottom -> max (box.bottom -. h) box.bottom, box.bottom
    in { left ; right ; top ; bottom }

  let store_pack_box w h bbox contents_pos = 
    let left, right = match w with
	`tight -> contents_pos.left, contents_pos.right
      | `fills | `expands -> bbox.left, bbox.right
      | `fixed w -> center w bbox.left bbox.right
    and top, bottom = match h with
	`tight -> contents_pos.top, contents_pos.bottom
      | `fills | `expands -> bbox.top, bbox.bottom
      | `fixed h -> center h bbox.top bbox.bottom in
    { left ; right ; top ; bottom }

  let rec adjust_props parent_props w = match w.kind with
    | WSignal ws ->
      let ws = S.map (adjust_props parent_props) ws in
      let props = bind_s ws (fun w -> w.props) in
      { w with kind = WSignal ws ; props }

    | Button _ | Void | Label _ ->
      { w with props = adjust_props_aux parent_props w.props }

    | Overlay children ->
      let props = adjust_props_aux parent_props w.props in
      let children = List.map (adjust_props props) children in
      { w with 
	  kind = Overlay children ;
	  props }

    | Frame fr ->
      let props = adjust_props_aux parent_props w.props in
      let fr_child = adjust_props props fr.fr_child in
      { w with 
	  kind = Frame { fr with fr_child } ;
	  props }

    | HPack hp ->
      let props = adjust_props_aux parent_props w.props in
      let hp_children = List.map (adjust_props props) hp.hp_children in
      { w with 
	  kind = HPack { hp with hp_children } ;
	  props }

    | VPack vp ->
      let props = adjust_props_aux parent_props w.props in
      let vp_children = List.map (adjust_props props) vp.vp_children in
      { w with 
	  kind = VPack { vp with vp_children } ;
	  props }

  and adjust_props_aux parent_props child_props = 
    S.l2
      (fun parent child -> 
	{
	  child with state = state_clip ~parent:parent.state child.state 
	})
      parent_props
      child_props

  let rec dimension w = match w.kind with
    | WSignal ws -> 
      let ws = S.map dimension ws in
      let dim = bind_s ws (fun w -> w.dim) in
      { w with kind = WSignal ws ; dim }

    | Overlay children ->
      let children = List.map dimension children in
      let dim =
	S.merge
	  (fun (accu_w,accu_h) (w,h) -> max w accu_w, max accu_h h)
	  (0.,0.)
	  (List.map (fun w -> w.dim) children) 
        |> S.l2 (fun props dim -> fix props.w props.h dim) w.props
      in
      { w with kind = Overlay children ; dim }

    | Frame fr ->
      let fr_child = dimension fr.fr_child in
      let dim = 
	S.l2
	  (fun style (w_child,h_child) ->
	    let border = B.Frame.border_size style in
	    (border.left +. border.right +. w_child, border.top +. border.bottom +. h_child))
	  fr.fr_style fr_child.dim 
        |> S.l2 (fun props dim -> fix props.w props.h dim) w.props
      in
      { w with dim ; kind = Frame { fr with fr_child } }

    | Button bt ->
      let dim = 
	S.l3 
	  (fun props style text -> B.Button.size style text |> (fix props.w props.h)) 
	  w.props bt.bt_style bt.bt_text in
      { w with dim }

    | Void ->
      let dim = S.l1 (fun props -> fix props.w props.h (0.,0.)) w.props in
      { w with dim }

    | Label lb ->
      let dim = 
	S.l3 
	  (fun props style text -> B.Label.size style text |> (fix props.w props.h))
	  w.props lb.lb_style lb.lb_text in
      { w with dim }

    | HPack hp ->
      let hp_children = List.map dimension hp.hp_children in
      let dim = 
	S.merge 
          (fun (accuw,accuh) (w,h) -> accuw +. w, max accuh h) 
          (0.,0.) 
	  (List.map (fun c -> c.dim) hp_children)
        |> S.l2 (fun props dim -> fix props.w props.h dim) w.props
      in { w with dim ; kind = HPack { hp with hp_children } }

    | VPack vp ->
      let vp_children = List.map dimension vp.vp_children in
      let dim = 
	S.merge 
          (fun (accuw,accuh) (w,h) -> max accuw w, accuh +. h) 
          (0.,0.) 
	  (List.map (fun c -> c.dim) vp_children)
        |> S.l2 (fun props dim -> fix props.w props.h dim) w.props
      in { w with dim ; kind = VPack { vp with vp_children } }



  let hpack_centered_with_spaces_layout box dims = 
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims 
    and n = List.length dims in
    let nspaces = float (n + 1) in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) /. nspaces in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
	cur_x -. w *. xscale -. xspace,
	(cur_x -. w *. xscale -. xspace /. 2., cur_x -. xspace /. 2.) :: accu)
      dims
      (box.right -. xspace /. 2., [])
    |> snd

  let hpack_centered_layout box dims = 
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
	cur_x -. w *. xscale,
	(cur_x -. w *. xscale, cur_x) :: accu)
      dims
      (box.right -. xspace /. 2., [])
    |> snd

  let hpack_justified_with_spaces_layout box = function
    | [] -> []
    | h :: [] -> [ box.left, box.right ]
    | dims ->
      let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims 
      and n = List.length dims in
      let nspaces = float (n - 1) in
      let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
      let xspace = (box.right -. box.left -. total_w *. xscale) /. nspaces in
      List.fold_right
	(fun (w,_) (cur_x, accu) ->
	  cur_x -. w *. xscale -. xspace,
	  (cur_x -. w *. xscale, cur_x) :: accu)
	dims
	(box.right, [])
      |> snd

  let hpack_centered_layout box dims = 
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
	cur_x -. w *. xscale,
	(cur_x -. w *. xscale, cur_x) :: accu)
      dims
      (box.right -. xspace /. 2., [])
    |> snd

  let hpack_left_layout box dims = 
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
	cur_x -. w *. xscale,
	(cur_x -. w *. xscale, cur_x) :: accu)
      dims
      (box.right -. xspace, [])
    |> snd


  let hpack_right_layout box dims = 
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
	cur_x -. w *. xscale,
	(cur_x -. w *. xscale, cur_x) :: accu)
      dims
      (box.right, [])
    |> snd

  let hpack_justified_layout box dims =
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims in
    let xscale = (box.right -. box.left) /. total_w in
    List.fold_right
      (fun (w,_) (cur_x, accu) ->
	cur_x -. w *. xscale,
	(cur_x -. w *. xscale, cur_x) :: accu)
      dims
      (box.right, [])
    |> snd
    
  let hpack_fill_layout nxexpands box props dims =
    let total_w = List.fold_left (fun accuw (w,h) -> accuw +. w) 0. dims in
    let xscale = if total_w <= box.right -. box.left then 1. else (box.right -. box.left) /. total_w in
    let xspace = (box.right -. box.left -. total_w *. xscale) /. nxexpands in
    List.fold_right2
      (fun props (w,_) (cur_x, accu) ->
	let dx = w *. xscale +. if props.w = `expands then xspace else 0. in
	cur_x -. dx,
	(cur_x -. dx, cur_x) :: accu)
      props dims
      (box.right, [])
    |> snd

    


  let hpack_layout layout align (bbox : box) props dims = 
    let nxexpands = List.fold_left (fun accu c -> if c.w = `expands then accu + 1 else accu) 0 props in
    let xpositions = 
      if nxexpands > 0 then 
	hpack_fill_layout (float nxexpands) bbox props dims
      else 
	match layout with 
	  | `centered -> hpack_centered_layout bbox dims
	  | `centered_with_space -> hpack_centered_with_spaces_layout bbox dims
	  | `left_aligned -> hpack_left_layout bbox dims
	  | `right_aligned -> hpack_right_layout bbox dims
	  | `justified -> hpack_justified_layout bbox dims
	  | `justified_with_space -> hpack_justified_with_spaces_layout bbox dims
    and ypositions = 
      let pos = match align with
	| `centered -> (fun h -> center h bbox.top bbox.bottom)
	| `top -> (fun h -> bbox.top, bbox.top +. h)
	| `bottom -> (fun h -> bbox.bottom -. h, bbox.bottom) in
      List.map2 
	(fun props (_,h) -> if fills props.h then bbox.top, bbox.bottom else pos h) 
	props dims
    in
    ((List.enum xpositions, List.enum ypositions) |> Enum.combine )
    /@ (fun ((left,right), (top,bottom)) -> { left ; right ; top ; bottom })
    |> List.of_enum


  let vpack_centered_with_spaces_layout box dims = 
    let total_h = List.fold_left (fun accuw (_,h) -> accuw +. h) 0. dims 
    and n = List.length dims in
    let nspaces = float (n + 1) in
    let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
    let yspace = (box.bottom -. box.top -. total_h *. yscale) /. nspaces in
    List.fold_right
      (fun (_,h) (cur_y, accu) ->
	cur_y -. h *. yscale -. yspace,
	(cur_y -. h *. yscale -. yspace /. 2., cur_y -. yspace /. 2.) :: accu)
      dims
      (box.bottom -. yspace /. 2., [])
    |> snd

  let vpack_centered_layout box dims = 
    let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. dims in
    let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
    let yspace = (box.bottom -. box.top -. total_h *. yscale) in
    List.fold_right
      (fun (_,h) (cur_y, accu) ->
	cur_y -. h *. yscale,
	(cur_y -. h *. yscale, cur_y) :: accu)
      dims
      (box.bottom -. yspace /. 2., [])
    |> snd

  let vpack_justified_layout box dims =
    let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. dims in
    let yscale = (box.bottom -. box.top) /. total_h in
    List.fold_right
      (fun (_,h) (cur_y, accu) ->
	cur_y -. h *. yscale,
	(cur_y -. h *. yscale, cur_y) :: accu)
      dims
      (box.bottom, [])
    |> snd
    
  let vpack_fill_layout nyexpands box props dims =
    let total_h = List.fold_left (fun accuh (_,h) -> accuh +. h) 0. dims in
    let yscale = if total_h <= box.bottom -. box.top then 1. else (box.bottom -. box.top) /. total_h in
    let yspace = (box.bottom -. box.top -. total_h *. yscale) /. nyexpands in
    List.fold_right2
      (fun c (_,h) (cur_y, accu) ->
	let dy = h *. yscale +. if c.h = `expands then yspace else 0. in
	cur_y -. dy,
	(cur_y -. dy, cur_y) :: accu)
      props dims
      (box.bottom, [])
    |> snd

  let vpack_layout layout align bbox props dims = 
    let nyexpands = List.fold_left (fun accu c -> if c.h = `expands then accu + 1 else accu) 0 props in
    let ypositions = 
      if nyexpands > 0 then 
        vpack_fill_layout (float nyexpands) bbox props dims
      else 
        match layout with 
          | `centered -> vpack_centered_layout bbox dims
          | `centered_with_space -> vpack_centered_with_spaces_layout bbox dims
          | `top -> assert false
          | `bottom -> assert false
          | `justified -> vpack_justified_layout bbox dims
          | `justified_with_space -> assert false
    and xpositions = 
      let pos = match align with
        | `centered -> (fun h -> center h bbox.left bbox.right)
        | `left -> (fun h -> bbox.left, bbox.left +. h)
        | `right -> (fun h -> bbox.right -. h, bbox.right) in
      List.map2 
        (fun props (w,_) -> if fills props.w then bbox.left, bbox.right else pos w) 
        props dims
    in
    ((List.enum xpositions, List.enum ypositions) |> Enum.combine)
    /@ (fun ((left, right), (top,bottom)) -> { left ; right ; top ; bottom })
    |> List.of_enum

  let rec position parent_bbox w = 
    let pos = 
      S.l3 
	(fun props dim bbox -> store_box `centered `centered props.w props.h bbox dim) 
	w.props w.dim parent_bbox 
    in
    match w.kind with
      | Button _ | Label _ | Void ->
	{ w with pos ; bbox = parent_bbox } 

      | VPack vp ->
	let children_dims = S.merge (fun l d -> d :: l) [] (List.rev_map (fun c -> c.dim) vp.vp_children) 
	and children_props = S.merge (fun l p -> p :: l) [] (List.rev_map (fun c -> c.props) vp.vp_children) in
	let merged_bboxes = 
	  S.l3 (vpack_layout vp.vp_layout vp.vp_align) pos children_props children_dims in
	let vp_children =
	  List.mapi 
	    (fun i w -> 
	      let parent_bbox = S.map (fun l -> List.nth l i) merged_bboxes in
	      position parent_bbox w)
	    vp.vp_children in
	{ w with bbox = parent_bbox ;
	         pos ; 
	         kind = VPack { vp with vp_children } }

      | HPack hp ->
	let children_dims = S.merge (fun l d -> d :: l) [] (List.rev_map (fun c -> c.dim) hp.hp_children) 
	and children_props = S.merge (fun l p -> p :: l) [] (List.rev_map (fun c -> c.props) hp.hp_children) in
	let merged_bboxes = 
	  S.l3 (hpack_layout hp.hp_layout hp.hp_align) pos children_props children_dims in
	let hp_children =
	  List.mapi 
	    (fun i w -> 
	      let parent_bbox = S.map (fun l -> List.nth l i) merged_bboxes in
	      position parent_bbox w)
	    hp.hp_children in
	{ w with bbox = parent_bbox ;
	         pos ; 
	         kind = HPack { hp with hp_children } }

      | Frame fr ->
	let adjust_bbox borders box =  (* FIXME: vérifier que la boite produite est valide (left <= right etc) *)
  	  { left  = box.left  +. borders.left ;
  	    right = box.right -. borders.right ;
  	    top = box.top +. borders.top ;
  	    bottom = box.bottom -. borders.bottom }
	in
	let fr_child =
  	  position
  	    (S.l2 (fun style box -> adjust_bbox (B.Frame.border_size style) box) fr.fr_style pos)
  	    fr.fr_child in
	{ w with bbox = parent_bbox ; pos ;
  	         kind = Frame { fr with fr_child } }

      | Overlay children ->
	let children = List.map (position pos) children in
	{ w with bbox = parent_bbox ; pos ;
	         kind = Overlay children }

      | WSignal ws ->
	let ws = S.map (position parent_bbox) ws in
	{ w with kind = WSignal ws ; bbox = parent_bbox ;
  	         pos  = bind_s ws (fun w -> w.pos) }

  let null_box = { left = 0. ; right = 0. ; top = 0. ; bottom = 0. }

  (* **************************************************************************************
     *** WIDGET CONSTRUCTORS
     ************************************************************************************** *)
  let widget ~w ~h ~state kind = {
    id = newid () ;
    kind ; 
    props = (
      S.l3
	(fun w h state -> { w ; h ; state })
	w h state
    ) ;
    dim = S.const (0.,0.) ;
    bbox = S.const null_box ;
    pos = S.const null_box ;
  }
      
  let void ?(w = c `tight) ?(h = c `tight) () =
    widget ~w ~h ~state:(c`active) Void

  let label ?(w = c `tight) ?(h = c `tight) ?(state = c`active) ?(style = c B.Label.default) lb_text =
    widget ~w ~h ~state (Label { lb_text ; lb_style = style })

  let button ?(w = c `tight) ?(h = c `tight) 
             ?(state = c`active)
             ?(style = c B.Button.default) 
	     ?shortcut bt_text = 
    let bt_fire = event_create ()
    and bt_pressed = signal_create false in
    let kind = Button { bt_text ; bt_style = style ; bt_fire ; bt_pressed ; bt_shortcut = shortcut } in
    widget ~w ~h ~state kind,
    bt_pressed#signal,
    bt_fire#event

  let frame 
      ?(w = c `tight) ?(h = c `tight)
      ?(state = c`active)
      ?(focus = S.const false)
      ?(style = c B.Frame.default)
      fr_child = 
    widget ~w ~h ~state (Frame { fr_child ; fr_style = style ; fr_focus_request = focus })

  let hpack ?(w = c `fills) ?(h = c `fills) 
            ?(state = c`active)
            ?(layout = `justified) ?(align = `centered) 
	    hp_children = 
    widget ~w ~h ~state (
      HPack { hp_layout = layout ;
	      hp_align = align ;
	      hp_children ; }
    )


  let vpack ?(w = c `fills) ?(h = c `fills) 
            ?(state = c`active)
            ?(layout = `justified) ?(align = `centered) 
	    vp_children = 
    widget ~w ~h ~state (
      VPack { vp_layout = layout ;
	      vp_align = align ;
	      vp_children ; }
    )


  let overlay ?(w = c `fills) ?(h = c `fills) ?(state = c`active) children =
    widget ~w ~h ~state (Overlay children)

  let wsignal ws = 
    widget ~w:(c `tight) ~h:(c `tight) ~state:(c`active) (WSignal ws)

  let stateful f init =
    let define input =
      let widget, updates = f input in
      let input' = S.accum updates init in
      input', (widget, input')
    in
    S.fix init define 

  let map_s _ _ = assert false

  let map_s_e f s = 
    let s' = S.map ~eq:( == ) f s in
    let widget = wsignal (S.map ~eq:( == ) fst s') 
    and events = S.map ~eq:( == ) snd s' in
    let init_event = S.value events 
    and event_changes = S.changes events in
    widget, E.switch init_event event_changes

  let map_e _ _ = assert false

  let loop_e _ _ = assert false

    
  (* **************************************************************************************
     *** DRAWING 
     ************************************************************************************** *)
  let string_of_dim = fun (w,h) -> sprintf "(%d,%d)" (int_of_float w) (int_of_float h)

  let has_focus w = function
    | Some id when id = w.id -> true
    | _ -> false

  let rec draw_widget focus w =
    if visible_state (S.value w.props).state then (
      match w.kind with
        | Void -> 
	  () (*B.draw_rect Color.green (S.value w.pos)*)
	| Label lb ->
	  B.Label.draw (S.value lb.lb_style) (S.value lb.lb_text) (S.value w.pos)
        | Button b ->
	  B.Button.draw (S.value b.bt_style) (S.value b.bt_text) (S.value b.bt_pressed#signal) (S.value w.pos)
	| WSignal ws -> 
	  draw_widget focus (S.value ws)
	| Overlay children
	| HPack { hp_children = children } 
	| VPack { vp_children = children } ->
      	  List.iter (draw_widget focus) children	  
	| Frame fr ->
	  B.Frame.draw (S.value fr.fr_style) (has_focus w focus) (S.value w.pos) ;
	  draw_widget focus fr.fr_child
    )

  (* **************************************************************************************
     *** I/O EVENTS
     ************************************************************************************** *)
  let rec broadcast_mouse_button_event w b s ~x ~y used = 
    if (S.value w.props).state = `active then (
      match w.kind, b, s, point_over (x,y) (S.value w.pos), used with 
	| Button bt, `left, `pressed, true, false -> 
	  bt.bt_pressed#send true ; true
	| Button bt, `left, `released, true, false -> 
	  bt.bt_pressed#send false ; 
	  bt.bt_fire#send () ;
	  true
	| Button bt, `left, `released, wum, used when used || not wum -> 
	  bt.bt_pressed#send false ; 
	  used
	| HPack { hp_children = children },_,_,_,_ 
	| VPack { vp_children = children },_,_,_,_
	| Overlay children,_,_,_,_ ->
	  List.fold_right 
	    (fun w accu -> broadcast_mouse_button_event w b s ~x ~y accu)
	    children used
	| WSignal child,_,_,_,_ -> 
	  broadcast_mouse_button_event (S.value child) b s ~x ~y used
	| Frame fr,_,_,_,_ -> (* FIXME click is used if it falls on the border *)
	  broadcast_mouse_button_event fr.fr_child b s ~x ~y used
	| _ -> used
    )
    else used

  let shortcut w k = match w.kind with
    | Button bt -> bt.bt_shortcut = Some k
    | _ -> false

  let rec broadcast_keyboard_event w focus k event selected_ancestor_frame used =
    if (S.value w.props).state = `active then (
      match w.kind, event with 
	| Button bt, `press when shortcut w k  && not used && selected_ancestor_frame ->
	  bt.bt_pressed#send true ; true
	| Button bt, `release when shortcut w k && (used || not selected_ancestor_frame) ->
	  bt.bt_pressed#send false ; used
	| Button bt, `release when shortcut w k && (not used && selected_ancestor_frame) ->
	  bt.bt_pressed#send false ;
	  bt.bt_fire#send () ;
	  true
	  
	| Frame fr, _ when focus = Some w.id -> 
	  broadcast_keyboard_event fr.fr_child focus k event true used
	| Frame fr, _ -> 
	  broadcast_keyboard_event fr.fr_child focus k event selected_ancestor_frame used
	| HPack { hp_children = children },_
	| VPack { vp_children = children },_
	| Overlay children,_ ->
	  List.fold_right 
	    (fun w accu -> broadcast_keyboard_event w focus k event selected_ancestor_frame accu)
	    children used
	| WSignal ws, _ ->
	  broadcast_keyboard_event (S.value ws) focus k event selected_ancestor_frame used
	| _ -> used
    )
    else used

  (* **************************************************************************************
     *** UI type definition
     ************************************************************************************** *)
  let print_r = (function (`focus,x) -> printf "(foc %d) " x | (`unfocus, x) -> printf "(unfoc %d)" x)
  let print_l xs = 
    List.iter print_r xs ;
    print_newline ()

  let rec focus_signal widget = match widget.kind with
    | Frame fr ->
      S.l2 
	(fun req below -> if req then Some widget.id else below) 
	fr.fr_focus_request 
	(focus_signal fr.fr_child)
	
    | HPack { hp_children = children }
    | VPack { vp_children = children }
    | Overlay children ->
      S.merge (fun accu x -> if accu <> None then accu else x) None (List.map focus_signal children)
	
    | WSignal ws -> bind_s ws focus_signal

    | _ -> S.const None

  let fullscreen = 
    S.map (fun (w,h) -> { left = 0. ; right = w ; top = 0. ; bottom = h }) B.screen_size

  let make topwidget = 
    let topprops = S.const { w = `expands ; h = `expands ; state = `active } in
    let topwidget = position fullscreen (dimension (adjust_props topprops topwidget)) in
    { 
      topwidget ;
      focus = focus_signal topwidget;
    }

  let draw ui = draw_widget (S.value ui.focus) ui.topwidget

  let mouse_button_event ui b s ~x ~y =
    broadcast_mouse_button_event ui.topwidget b s ~x:(float x) ~y:(float y) false

  let keyboard_event ui k event =
    broadcast_keyboard_event ui.topwidget (S.value ui.focus) k event false false

end



