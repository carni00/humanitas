(*

  ****************************** src/devel_ui/gui.ml ******************************


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
open Humanitas_orbis 
open Humanitas_game 
open Std
module W    = WindowID
module K    = Sdlkey
module RE   = React.E
module RS   = React.S
module SA   = Status.Atelier
module Win  = Window
module Ws   = Windows

module Make (Draw : Video.Draw) = struct

  module D    = Draw
  module UI   = Frui.Make(Draw) 
  type t      = UI.t
  module Gen(T : UI.Toolkit) = struct
  
let truc = RS.trace (Std.Opt.smap "none" Draw.Window.string_of_id |- Printf.printf "tk: %s\n%!"  ) T.window_focus
  (*	(Core.Option.value_map ~default:"none" ~f:Draw.Window.string_of_id |- Printf.printf "tk: %s\n%!")*)
  (* remplacement parce que core.option n’est plus connu au make *)
let window_focus_signal wid = RS.map (fun x -> x = Some wid) truc (* T.window_focus *)
  (* évolution du focussage d’une window *)

let window_visibility = function
  | W.Alive
  | W.Frozen    -> `opaque
  | W.Invisible
  | W.Nil
  | W.Glass     -> `invisible

let window_visibility_signal wid status_s =
  let f = (fun s->
    let windows = Status.windows s in
    let is_hidden_by_stack = match WindowID.duty wid, Windows.windowPos windows wid with
      | W.Sheet, W.Left -> List.mem (Windows.leftStackStatus windows) W.([ Glass ; Invisible ; Nil ])
      | W.Sheet, W.Right -> List.mem (Windows.rightStackStatus windows) W.([ Glass ; Invisible ; Nil ])
      | _ -> false in
    if is_hidden_by_stack then `invisible
    else window_visibility (Ws.windowState windows wid)) in
  RS.map f status_s
  (* état courant de la window *)

let collect es = RE.merge (fun l x -> x @ l) [] es
(* concaténation de list de React.event *)

let map_s_e s f =
  let s' = RS.map ~eq:( == ) f s in
  let init = RS.value s' in
  let changes = RS.changes s' in
  RS.hold ~eq:( == ) (fst init) (RE.map fst changes),
  RE.switch (snd init) (RE.map snd changes)
(* map signal element, je suppose *)

let modulate_tasks list = fun _button_up_event -> Handler.modulate_tasks list
(* mise à jour liste de tâches en fonction des mods ctrl, alt etc. *)


type button = (UI.element * bool React.signal * unit React.event) * Task.t list
type frame  =  UI.element * (Task.t list) React.event
type widget =  Element of  UI.element | Button  of  button | Frame   of  frame


let rsm      = RS.map
let k        = RS.const  
let kx       = RS.const `expands
let ews    r = rsm (fun w -> `fixed (r *. w)) D.ewip  (* element width signal *)
let uws      = rsm (fun w -> `fixed       w ) D.ewip  (* unity element width signal *)
let ehs    r = rsm (fun h -> `fixed (r *. h)) D.ehip
let uhs      = rsm (fun h -> `fixed (     h)) D.ehip
let margin x = k   { Frui.top = x ; bottom = x ; left = x ; right = x }

let void     w              = Element (T.void ~w:w      ~h:(ehs 1.) ())
let cStrn    s              = Element (T.label   (k s) )
let strn     s              = Element (T.label      s  )
let line    ss              = Element (T.div ~background:(k (Some Ci.wsb)) ~w:(ews 14.) ~h:(ehs 1.) [T.label ss])
let _button ~w (k, s, task) = Button (T.button ~w:(ews w) ~h:(ehs 1.) ~shortcut:k (         s), task )
let cButton ~w (k, s, task) = Button (T.button ~w:(ews w) ~h:(ehs 1.) ~shortcut:k (RS.const s), task )
let rect     w  h  _c       = Button (T.button ~w:(ews w) ~h:(ehs h) (k " "), [] )
let box_of_element w h ha element = Element ( T.div ~w:(ews w) ~h:(ehs h) ~layout:(k (`vpack (`packed `center, ha))) [element] )

let uie_of_widget_list dir widget_list =
  let rec split = function 
  | [] -> ([], [])
  | Element( w        )::q -> let (wq, eq) = split q in (w::wq,    eq) 
  | Button ((w,_c,e),t)::q -> let (wq, eq) = split q in (w::wq, (RE.map (modulate_tasks t) e)::eq) 
  | Frame  ( w,   e   )::q -> let (wq, eq) = split q in (w::wq, e::eq) in
  let wList, eList = split widget_list in match dir with
  | Win.Columns       -> T.hpack ~spacing:(k (`packed `center)) ~w:kx                     wList, collect eList
  | Win.Lines (vc,ha) -> T.div    ~layout:(k (`vpack (vc, ha))) ~w:kx ~margin:(margin 5.) wList, collect eList
 (* liste de widget, disposés horizontalement ou verticalement *)

let rec widget_frame fra =
  let dir, list = fra in
  let rec widget = function
	| Win.Rect (w,h,c) -> rect w h c
	| Win.LB but   -> cButton ~w:12. but
	| Win.SB but   -> cButton ~w:2.7 but
	| Win.S string -> cStrn string
	| Win.Z strSnl -> line (*~w:16.*) strSnl
	| Win.List fra -> widget_frame fra 
	| Win.Box (w,h,_ha, Win.S s) -> Element (T.label ~w:(ews w) ~h:(ehs h) (k s))
	| Win.Box (w,h,_ha, Win.Z s) -> Element (T.label ~w:(ews w) ~h:(ehs h) (s)  )
	| Win.Box (w,_h,_ha, Win.SB but) -> cButton ~w but
	| Win.Box (w,h,ha,we) -> box_of_element w h ha 
  (match widget we with 
  | Element element -> element 
  | _ -> T.void() ) 
  in
  match (uie_of_widget_list dir (List.map widget list)) with (w,e) -> Frame(w,e)

(*type layout = [
| `hpack of halign spacing * valign
| `vpack of valign spacing * halign
| `overlay
]
and halign = [`left | `center | `right ]
and valign = [`top | `center | `bottom ]
and 'a spacing = [ `packed of 'a | `justified | `spread]*)

(***************************************** ui.element constructeurs *******************************************)

let uie_queen_titleBar id title =
  let tb = cButton ~w:1.  in
  uie_of_widget_list Win.Columns [
    tb (K.KEY_u       , "U"           , [`wUndo                   ]    );
    void (k `expands);
    strn title;
    void (k `expands);
    tb (K.KEY_x       , "X"           , [`wClose id               ]    );
  ]

let uie_sheet_titleBar side id title =
  let button key label f = match T.button ~w:uws ~h:uhs ~shortcut:key (k label) with elt,_,bt_fires -> elt, f bt_fires in
  let bt  key label task = button key label (RE.map ((modulate_tasks task)))
  and bt' key label f    = button key label (fun fires -> RS.sample (fun () s -> f s) fires side)
  and void w = T.void ~w ~h:uhs (), RE.never
  and strn s = T.label s, RE.never
  and columns cols = T.hpack ~spacing:(k (`packed `center)) ~w:kx (List.map fst cols),
                     collect (List.map snd cols) in
  let contents = [
    bt    K.KEY_m        "M"              [`wMove                       ] ;
    bt    K.KEY_PAGEUP   "pgup"           [`wPrevious                   ] ;
    bt    K.KEY_PAGEDOWN "pgdn"           [`wNext                       ] ;
    void kx;
    strn title;
    void kx;
    bt    K.KEY_u        "U"              [`sFocus None                ]  ;
    bt'   K.KEY_h        "H" (fun side -> [`sHide  side; `sFocus None  ]) ;
    bt    K.KEY_x        "X"              [`wClose id                  ]  ;
  ] in columns contents
(* la sheet_titleBar est le seul machin qui soit géré directement en T(oolkit) ;
   les widgets suivants sont faits de Gen.widget *)

let uie_frame pos status_s data =
  let id, (title, element)   = data in
  let fra = (Win.Columns, [element] ) in 
  let contents, cEvents  = match widget_frame fra with Frame (w,e) -> w,e | _ -> (T.void(), RE.never) in
  let titleBar, tbEvents = (match W.duty id with
	| W.Sheet -> uie_sheet_titleBar pos id title 
	| _       -> uie_queen_titleBar id title) in
  T.div
     ~layout:(k (`vpack (`justified, `center)))
     ~h:(match W.duty id with W.Sheet -> kx | _ -> k `tight)
     ~background:(k (Some Ci.wsb))
     ~visibility:(window_visibility_signal id status_s)
     ~framed:(window_focus_signal id)
     ~margin:(margin 1.)
     [ titleBar ; contents ; T.void ~h:kx () ], collect [cEvents; tbEvents]
(* construction d'un ui.element de type frame (sheet ou queen), sans contents *)

let uie_cadre_de_sheet spacing e = 
  let uhs = rsm (fun h -> `fixed ( h +. 2.)) D.ehip in (* + 2. pour laisser la place au focus jaune *)
  T.vpack ~w:kx ~h:kx [
     T.void ~h:uhs () ;
     T.hpack ~w:kx ~h:kx ~spacing [ e ] ;
     T.void ~h:uhs () ;
     ]
(* construction d'un ui.element de type cadre de sheet *)

(***************************************** ui.window constructeurs *******************************************)

let ui_window opt_cadre status_s pos_s wid = 
  let win_contents_s = Window.contents_s status_s wid	in
  let opt_uie_frame = function
  | Some win_content -> uie_frame pos_s status_s win_content
  | None             -> T.void (), RE.never in
  let element_s, output = map_s_e win_contents_s opt_uie_frame in match opt_cadre with
  | None   -> T.window wid            element_s  , output (* queen *)
  | Some f -> T.window wid ( RS.map f element_s ), output (* sheet *)
(* construction d’une ui_window (appels à moult code) *)
(* commun à uiw_queen et uiw_sheet *)

let uiw_queen status_s wid = ui_window None status_s (k W.Central) wid
(* Vu de Frui, une queen est une ui_window en position centrale *)

let uiw_sheet spacing status_s wid =
  let spacing = spacing wid in
  let win_pos_s = RS.map (Status.windows |- (flip Windows.windowPos) wid) status_s in
  ui_window (Some (uie_cadre_de_sheet spacing)) status_s win_pos_s wid

let uiw_sheet_list status_s =
  let left_stack_s  = RS.map (Status.windows |- Windows.leftStack)  status_s
  and right_stack_s = RS.map (Status.windows |- Windows.rightStack) status_s
  and which_stack left_stack _right_stack wid = if List.mem wid left_stack then `left else `right in
  let spacing wid = RS.l2 (fun left_stack right_stack -> `packed (which_stack left_stack right_stack wid)) left_stack_s right_stack_s in
  List.map (uiw_sheet spacing status_s) W.sheets

(***************************************** Towers *******************************************)

let topBar status_s = 
  let open Sdlkey in
  let open WindowID in
  let open Tfloat in
  let ews    r = RS.map (fun w -> `fixed (r * foi w / 13.75)) D.swip in (* width signal *)
  (* redefinition de ews : l'unité de base de largeur est la largeur d'un bouton (et non d'un élément) *)
  (* la largeur du bouton est fonction de la largeur de l'écran *)
  let space r = void ( ews r ) in 
  let b (key, title, task) = Button( T.button ~w:(ews 1.) ~h:(ehs 1.) ~shortcut:key (k title), task ) in

  let tower_list atelier_opt = 
	let left_towers = [
          b (KEY_g       , "Game"        , [`wOpen (Game   , Default)]    );
          space 0.25;
          b (KEY_F1      , "help"        , [`wOpen (Help   , Default)]    );
	]
	and middle_towers a = 
          let filter = Si.filter (Scene.filter (SA.scene a)) in
          let pov    = Game.Player.pov (SA.player a) in
          let sb bt  = if pov = Nid.none then space 1. else b bt in 
          [
            b (KEY_o       , "Orbis"       , [`wOpen (Orbis, Default)]    );
            b (KEY_t       , "Tabula"      , [`wOpen (Tabula, Default)]    );
            b (KEY_F4      , filter        , [`switch_filter           ]    );
            space 0.25;
            sb(KEY_F5      , Si.natioName pov, [ `wOpen (Polis  , Default)]    );
            sb(KEY_s       , "Stratiotikon",   [ `wOpen (Polis  , Default)]    );
            sb(KEY_j       , "Junctiones",     [ `wOpen (Tactics, Default)]    );
            sb(KEY_F8      , " ", [ `wOpen (Polis  , Default)]    );
          ]
	and right_towers a = 
          let turn   = (Game.orbis (SA.game a)).Orbis.turn in [
            void (k `expands) ;
			      b (KEY_F9      , " << "        , [`wOpen (Vetera    , Default)]    );
			      b (KEY_F10     , Si.date turn  , [`wOpen (Newspaper , Default)]    );
            b (KEY_RETURN  , " > "         , [`end_of_turn 1        ]    );
            b (KEY_F12     , " >> "        , [`next_event           ]    );
	] in
	let list = match atelier_opt with
	  | Some a -> left_towers @ (middle_towers a) @ (right_towers a)
    | _      -> left_towers @ [ void (k `expands) ] in
	uie_of_widget_list Win.Columns list
      in
      map_s_e (RS.map Status.atelier status_s) tower_list
    (* top bar *)

let bottomBar status_s =
  let open Sdlkey in
  let open Tfloat in
  let ews    r = RS.map (fun w -> `fixed (foi w * foi r / 100.)) D.swip in (* width signal *)
  (* redefinition de ews : l'unité de base de largeur est un centième de la largeur de l'écran *)
  let b w (key, title, task) = Button( T.button ~w:(ews w) ~h:(ehs 1.) ~shortcut:key (k title), task ) in

  let f s = 
	uie_of_widget_list Win.Columns ( 
	  let ao = Status.atelier s in match ao with 
	    | Some a ->( 
                   let module GP = Game.Player in
	                 let player = SA.player a in
			 [
(*		   b 10 (KEY_UNKNOWN , GP.name player                    , []    );*)
			   b 10 (KEY_UNKNOWN , "role : "^Si.role  (GP.role player)         , []    );
(*  	   b 15 (KEY_UNKNOWN , "pov  : "^Si.natio Si.Name (GP.pov player)  , []    );*)
                           void (k `expands) ;
			 ])
	    | None -> [ ] )
      in
      map_s_e status_s f
    (* bottom bar *)

let uies_towers status_s =
  let topBar   , topBarEvents    = topBar status_s in
  let bottomBar, bottomBarEvents = bottomBar status_s in
  let vpack tb bb = T.vpack  ~w:kx ~h:kx [ tb ; T.void ~h:kx () ; bb ] in
  UI.ES.l2 vpack topBar bottomBar, collect [ bottomBarEvents ; topBarEvents ]


(***************************************** Gen.make *******************************************)

let make status_s =
  let window_focus_changes = RS.changes T.window_focus in
      (* event to update frui focus*)
  let focus_e = RE.merge ( @ ) [] [
      RS.map (fun status -> match Windows.activeWindow (Status.windows status) with
      | Some wid -> [ `focus (wid, None) ]
      | None -> [ `focus (WindowID.Towers, None) ]) status_s |> RS.changes ;
  	  (* si le focus frui est à none, le ramener sur les towers *)
      RE.fmap (function None -> Some [ `focus (WindowID.Towers, None) ] | _ -> None) window_focus_changes
    	] in
      (* créer un evt frui si le focus change dans status *)
  let focus_task_list wid_opt status =
      let f = function
      | W.Towers -> None
      | wid -> Some (Windows.windowPos (Status.windows status) wid) in
      let win_pos = Core.Option.bind wid_opt ~f in
      [ `sFocus win_pos ] in
  let focus_tk2st = RS.sample focus_task_list window_focus_changes status_s in	  
  let towers, towersEvent = uies_towers status_s in
  let sheet_list          = uiw_sheet_list status_s in
  let queen_list          = List.map (uiw_queen status_s) W.queens in
  let windows= List.concat [ List.map fst queen_list ; List.map fst sheet_list ; [ T.window WindowID.Towers towers ] ; ] in
  let event  = collect (focus_tk2st :: towersEvent :: (List.map snd sheet_list) @ (List.map snd queen_list)) in
  windows, focus_e, event

end

(***************************************** Make.main *******************************************)

let main status_s = 
  let f (module Tk : UI.Toolkit) =
    let module X = Gen(Tk) in
    X.make status_s in
  let ui, _ui_events, output = UI.make ~initial_focus:WindowID.Towers f in
    (* let ui_events = RE.trace (List.iter (UI.string_of_ui_event |- print_endline)) ui_events in *)
    (* let output = RE.merge ( @ ) [] [ *)
    (*   output ; *)
    (*   RS.sample *)
    (* 	(fun evts status -> *)
    (* 	  let f = function *)
    (* 	    | `focus (wid,_) ->  *)
    (* 	      Some (Windows.windowPos (Status.windows status) wid) *)
    (* 	    | _ -> None *)
    (* 	  in *)
    (* 	  Core.Std.List.filter_map evts f) *)
    (* 	ui_events status_s *)
    (*   |> RE.fmap (function *)
    (* 	          | [] -> None *)
    (* 		  | l -> Some (List.map (fun x -> `sFocus (Some x)) l)) *)
    (* ] *)
    (* in *)
    ui, output
end

(* EOF *)
