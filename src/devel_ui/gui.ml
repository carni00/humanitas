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
module H    = Handler
module W    = WindowID
module K    = Sdlkey
module RE   = React.E
module RS   = React.S
module SA   = Status.Atelier
module Win  = Window
module Ws   = Windows




module Make (Draw : Video.Draw) = struct

  module UI = Frui.Make(Draw) 
  type t    = UI.t

  module Gen(T : UI.Toolkit) = struct

    module D    = Draw
    module RSAO = RS.Make(struct type 'a t = SA.t option let equal = ( == ) end)
  
    let k   = RS.const  
    let rsm = RS.map

    let collect es = RE.merge (fun l x -> x @ l) [] es
(* addition d’events de task *)
    let modulate_tasks list = fun _button_up_event -> H.modulate_tasks list
(* mise à jour liste de tâches en fonction des mods ctrl, alt etc. *)

    let margin x = k { Frui.top = x ; bottom = x ; left = x ; right = x }
 
    let ews r = rsm (fun w -> `fixed (r *. w)) D.ewip  (* element width signal *)
    let uws   = rsm (fun w -> `fixed       w ) D.ewip  (* unity element width signal *)
    let ehs r = rsm (fun h -> `fixed (r *. h)) D.ehip
    let uhs   = rsm (fun h -> `fixed (     h)) D.ehip
  
    let map_s_e s f =
      let s' = RS.map ~eq:( == ) f s in
      let init = RS.value s' in
      let changes = RS.changes s' in
      RS.hold ~eq:( == ) (fst init) (RE.map fst changes),
      RE.switch (snd init) (RE.map snd changes)


    let sheet_titleBar side id title =
      let button key label f = 
    	let elt,_,bt_fires = T.button ~w:uws ~h:uhs ~shortcut:key (k label) in
    	elt, f bt_fires in
      let bt  key label task = button key label (RE.map ((modulate_tasks task)))
      and bt' key label f    = button key label (fun fires -> RS.sample (fun () s -> f s) fires side)
      and void w = T.void ~w ~h:uhs (), RE.never
      and strn s = T.label s, RE.never
      and columns cols =
    	T.hpack
    	  ~spacing:(k (`packed `center))
    	  ~w:(k `expands)
    	  (List.map fst cols),
    	collect (List.map snd cols)
      in
      let contents = [
    	bt K.KEY_m       "M"           [`wMove                       ]    ;
    	bt K.KEY_PAGEUP    "pgup"           [`wPrevious                   ]   ;
    	bt K.KEY_PAGEDOWN  "pgdn"           [`wNext                       ] ;
    	void (k `expands);
    	strn title;
    	void (k `expands);
    	bt K.KEY_u       "U"           [`sFocus None                ]    ;
    	bt' K.KEY_h      "H"           (fun side -> [`sHide  side; `sFocus None  ])    ;
    	bt K.KEY_x       "X"           [`wClose id                   ]    ;
(*    	bt' K.KEY_x      "X"           (fun side -> [`sClose side; `sFocus None  ])    ;*)
      ]
      in
      columns contents
(* la sheet_titleBar est le seul machin qui soit géré directement en T(oolkit) ;
   les widgets suivants sont faits de Gen.widget *)

    type widget =
    | Widget of UI.element
    | Button of ((UI.element * bool React.signal * unit React.event) * Task.t list)
    | Frame  of UI.element * (Task.t list) React.event

    let void     w = Widget (T.void ~w:w      ~h:(ehs 1.) ())
    let cStrn    s = Widget (T.label   (k s) )
    let strn     s = Widget (T.label      s  )
    let line (*~w*) ss = Widget (T.div ~background:(k (Some Ci.wsb)) ~w:(ews 14.) ~h:(ehs 1.) [T.label ss])
    let _button ~w (key, title, task) = Button( T.button ~w:(ews w) ~h:(ehs 1.) ~shortcut:key (title), task )
    let cButton ~w (key, title, task) = Button( T.button ~w:(ews w) ~h:(ehs 1.) ~shortcut:key (k title), task )
    let rect w h _c = Button(T.button ~w:(ews w) ~h:(ehs h) (k " "), [] )

    let box w h ha widget =
	Widget ( T.div 
	  ~w:(ews w) (* ne fonctionne pas *)
	  ~h:(ehs h) (* ne fonctionne pas *)
	  ~layout:(k (`vpack (`packed `center, ha)))
	  [widget] )
      
     
    let liste dir wList =
      let rec split = function 
	| [] -> ([], [])
	| Widget( w        )::q -> let (wq, eq) = split q in (w::wq,    eq) 
	| Button((w,_c,e),t)::q -> let (wq, eq) = split q in (w::wq, (RE.map (modulate_tasks t) e)::eq) 
	| Frame ( w,   e   )::q -> let (wq, eq) = split q in (w::wq, e::eq) in
      let wList, eList = split wList in
      match dir with
      | Win.Columns       ->
	T.hpack
	  ~spacing:(k (`packed `center))
	  ~w:(k `expands)                       
	  wList, 
	collect eList
      | Win.Lines (vc,ha) ->
	T.div
	  ~layout:(k (`vpack (vc, ha)))
(*	  ~layout:(k (`vpack (`packed `top, ha)))*)
	  ~w:(k `expands)
	  ~margin:(margin 5.)
	  wList, 
	collect eList
  (* liste de widget, disposés horizontalement ou verticalement *)
(*type layout = [
| `hpack of halign spacing * valign
| `vpack of valign spacing * halign
| `overlay
]
and halign = [`left | `center | `right ]
and valign = [`top | `center | `bottom ]
and 'a spacing = [ `packed of 'a | `justified | `spread]*)

	
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
            b (KEY_t       , "Tabula"      , [`wOpen (Display, Default)]    );
            b (KEY_f       , filter        , [`switch_filter           ]    );
            b (KEY_r       , "Regio"       , [`secure_sr ; `wOpen (Regio  , Default)]    );
            space 0.25;
            sb(KEY_p       , "Polis"       , [             `wOpen (Polis  , Default)]    );
          ]
	and right_towers a = 
          let turn   = (Game.orbis (SA.game a)).Orbis.turn in [
            void (k `expands) ;
			      b (KEY_F9      , " << "        , [`wOpen (Vetera , Default)]    );
			      b (KEY_F10     , Si.date turn  , [`wOpen (Time   , Default)]    );
            b (KEY_RETURN  , " > "         , [`end_of_turn 1        ]    );
            b (KEY_F12     , " >> "        , [`next_event           ]    );
	] in
	let list = match atelier_opt with
	  | Some a -> left_towers @ (middle_towers a) @ (right_towers a)
    | _      -> left_towers @ [ void (k `expands) ] in
	liste Win.Columns list
      in
      map_s_e (RS.map Status.atelier status_s) tower_list
    (* top bar *)


    let bottomBar staSnl =
      let open Sdlkey in
      let open Tfloat in
      let ews    r = RS.map (fun w -> `fixed (foi w * foi r / 100.)) D.swip in (* width signal *)
      (* redefinition de ews : l'unité de base de largeur est un centième de la largeur de l'écran *)
      let b w (key, title, task) = Button( T.button ~w:(ews w) ~h:(ehs 1.) ~shortcut:key (k title), task ) in

      let f s = 
	liste Win.Columns ( 
	  let ao = Status.atelier s in match ao with 
	    | Some a ->( 
                   let module GP = Game.Player in
	                 let player = SA.player a in
			 [
			   b 10 (KEY_UNKNOWN , GP.name player                    , []    );
			   b 10 (KEY_UNKNOWN , "role : "^Si.role  (GP.role player)         , []    );
			   b 15 (KEY_UNKNOWN , "pov  : "^Si.natio Si.Name (GP.pov player)  , []    );
(*			   b 30 (KEY_SPACE   , Si.date turn          , [`wOpen (Time   , Default)]    );*)
                           void (k `expands) ;
			 ])
	    | None -> [ ] )
      in
      map_s_e staSnl f
    (* bottom bar *)


    let queen_titleBar id title =
      let tb = cButton ~w:1.  in
      liste Win.Columns [
	tb (K.KEY_u       , "U"           , [`wUndo                   ]    );
	void (k `expands);
	strn title;
	void (k `expands);
	tb (K.KEY_x       , "X"           , [`wClose id               ]    );
      ]

    let rec frame fra =
      let dir, list = fra in
      let rec widget = function
	| Win.Rect (w,h,c) -> rect w h c
	| Win.LB but   -> cButton ~w:12. but
	| Win.SB but   -> cButton ~w:2.7 but
	| Win.S string -> cStrn string
	| Win.Z strSnl -> line (*~w:16.*) strSnl
	| Win.List fra -> frame fra 
	| Win.Box (w,h,_ha, Win.S s) -> Widget (T.label ~w:(ews w) ~h:(ehs h) (k s))
	| Win.Box (w,h,_ha, Win.Z s) -> Widget (T.label ~w:(ews w) ~h:(ehs h) (s)  )
	| Win.Box (w,h,ha,e) -> box w h ha (match widget e with Widget w -> w | _ -> T.void() ) in
      match (liste dir (List.map widget list)) with (w,e) -> Frame(w,e)

    let truc = 
      RS.trace 
	(Std.Opt.smap "none" Draw.Window.string_of_id |- Printf.printf "tk: %s\n%!"  )
(*	(Core.Option.value_map ~default:"none" ~f:Draw.Window.string_of_id |- Printf.printf "tk: %s\n%!")*)
(* remplacement parce que core.option n’est plus connu au make *)
	T.window_focus

    let window_focus_signal wid =
      RS.map (fun x -> x = Some wid) truc (* T.window_focus *)
  (* évolution du focussage d’une window *)

    let window_visibility = function
      | W.Alive
      | W.Frozen    -> `opaque
      | W.Invisible
      | W.Nil
      | W.Glass     -> `invisible

    let window_visibility_signal wid status =
      RS.map 
	(fun s->
	  let windows = Status.windows s in
	  let is_hidden_by_stack = match WindowID.duty wid, Windows.windowPos windows wid with
	    | W.Sheet, W.Left -> List.mem (Windows.leftStackStatus windows) W.([ Glass ; Invisible ; Nil ])
	    | W.Sheet, W.Right -> List.mem (Windows.rightStackStatus windows) W.([ Glass ; Invisible ; Nil ])
	    | _ -> false
	  in
	  if is_hidden_by_stack then `invisible
	  else window_visibility (Ws.windowState windows wid))
	 status
  (* état courant de la window *)


    (* FIXME: pas optimal, pos devrait être un signal *)
    let window pos staSnl data =
      let id, (title, element)   = data in
      let fra = (Win.Columns, [element] ) in 
      let contents, cEvents  = match frame fra with Frame (w,e) -> w,e | _ -> (T.void(), RE.never) in
      let titleBar, tbEvents = (match W.duty id with
	| W.Sheet -> sheet_titleBar pos id title 
	| _       -> queen_titleBar id title) 
      in
      T.div
	~layout:(k (`vpack (`justified, `center)))
	~h:(match W.duty id with W.Sheet -> k `expands | _ -> k `tight)
	~background:(k (Some Ci.wsb))
	~visibility:(window_visibility_signal id staSnl)
	~framed:(window_focus_signal id)
	~margin:(margin 1.)
	[ titleBar ; contents ; T.void ~h:(k `expands) () ],
      collect [cEvents; tbEvents]
  (* construction d'un widget window *)


    let towers status =
      let topBar, topBarEvents = topBar status in
      let bottomBar, bottomBarEvents = bottomBar status in
      UI.ES.l2
	(fun topBar bottomBar ->
	  T.vpack  
	    ~w:(k `expands) ~h:(k `expands)  (* remplir l’écran *)
	    [ topBar ; T.void ~h:(k `expands) () ; bottomBar ])
	topBar bottomBar,
      collect [ bottomBarEvents ; topBarEvents ]

    let sheets status_s =
      let atelier_s = RSAO.map Status.atelier status_s in
      let left_stack_s = RS.map  (Status.windows |- Windows.leftStack)  status_s
      and right_stack_s = RS.map (Status.windows |- Windows.rightStack) status_s
      and which_stack left_stack _right_stack wid =
	if List.mem wid left_stack then `left else `right
      in
      let f wid =
	let win_pos_s = 
	  RS.map 
	    (Status.windows |- (flip Windows.windowPos) wid) 
	    status_s 
	and win_contents_s = match wid with
	  | W.Display ->
	    let atelier_s = RSAO.map Status.atelier status_s in
	    RS.map (function Some a -> Some(W.Display, Window.atelier a wid) | _ -> None) atelier_s
	  | W.Artes
	  | W.Chora
	  | W.Dx
	  | W.Fines
	  | W.Partitio
	  | W.Polis 
	  | W.Tactics 
	  | W.Pyramid -> (
	    let f = function
	      | Some a ->( 
                let nid = Game.Player.pov (SA.player a) in
                if nid = Nid.none then None 
                else Some (wid, Window.natio a nid wid)
                )
	      | _ -> None
	    in
	    RS.map f atelier_s 
          )
	  | W.Regio -> (
	    let f = function
	      | Some a -> (
		match Scene.sr (SA.scene a) with
		| Some rid -> Some (W.Regio, Window.regio a rid)
		| _ -> None
	      )
	      | _ -> None
	    in
	    RS.map f atelier_s
	  )
	  | W.Vetera
	  | W.Orbis -> (
	    let f = function
	      | Some a -> Some (wid, Window.atelier a wid)
	      | _ -> None
	    in
	    RS.map f atelier_s
	  )
	  | id -> k (Some (id, Window.data status_s id))
	in
	let element_s, output = map_s_e win_contents_s (function 
	  | Some w -> window win_pos_s status_s w
	  | None -> T.void (), RE.never
	)
	in
	let element_s' =
	  RS.map
	    (fun elt ->
	      let spacing = 
		RS.l2 
		  (fun left_stack right_stack -> `packed (which_stack left_stack right_stack wid))
		  left_stack_s right_stack_s
	      in
	      T.vpack ~w:(k`expands) ~h:(k`expands) [
		T.void ~h:uhs () ;
		T.hpack ~w:(k`expands) ~h:(k`expands) ~spacing [ elt ] ;
		T.void ~h:uhs () ;
	      ])		
	    element_s

	in
	T.window wid element_s', output
      in
      List.map f W.sheets

	

    let queens staSnl =
      let f wid =
	let win_contents_s = 
	  match wid with
	  | W.Display ->
	    let atelier_s = RSAO.map Status.atelier staSnl in
	    RS.map (function Some a -> Some(W.Display, Window.atelier a wid) | _ -> None) atelier_s
	  | id ->
	    k (Some (id, Window.data staSnl id))
	in
	let element_s, output = map_s_e win_contents_s (function 
	  | Some w -> window (k W.Central) staSnl w
	  | None -> T.void (), RE.never
	) 
	in
	T.window wid element_s, output
      in
      List.map f W.queens

    let make status_s =
      let window_focus_changes = RS.changes T.window_focus in
      (* event to update frui focus*)
      let focus_e =
	RE.merge ( @ ) [] [
	  (* créer un evt frui si le focus change dans status *)
	  RS.map 
	    (fun status -> 
	       match Windows.activeWindow (Status.windows status) with
	       | Some wid -> [ `focus (wid, None) ]
	       | None -> [ `focus (WindowID.Towers, None) ])
	    status_s
          |> RS.changes ;
	  (* si le focus frui est à none, le ramener sur les towers *)
	  RE.fmap
	    (function None -> Some [ `focus (WindowID.Towers, None) ] | _ -> None)
	    window_focus_changes
	]
      in
      let focus_tk2st =
        RS.sample
	  (fun wid_opt status ->
	    let f = function
	      | W.Towers -> None
	      | wid -> Some (Windows.windowPos (Status.windows status) wid) in
	    let win_pos = Core.Option.bind wid_opt ~f in
	    [ `sFocus win_pos ])
	  window_focus_changes
	  status_s
      in	  
      let queens = queens status_s in
      let towers, towersEvent = towers status_s in
      let sheets = sheets status_s in
      List.concat [
	List.map fst queens ;
	List.map fst sheets ;
	[ T.window WindowID.Towers towers ] ;
      ],
      focus_e,
      collect (focus_tk2st :: towersEvent :: (List.map snd sheets) @ (List.map snd queens))

  end

  let main status_s = 
    let f (module Tk : UI.Toolkit) =
      let module X = Gen(Tk) in
      X.make status_s
    in
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
