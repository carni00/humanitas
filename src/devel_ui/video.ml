(*

 ****************************** Video.ml ******************************


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

module Co = Color
module RS = React.S

type fontFun = (Font.id -> int -> Sdlttf.font)

type video_info = 
  {
  video_info : Sdlvideo.video_info;
  driver_name : string;
  video_mode_ok : int;
  }

let bitmap_path = Conf.string "BMP" "./bmp"

let video_info ss (*screenStatus*) = 
  {
  video_info = Sdlvideo.get_video_info();
  driver_name = Sdlvideo.driver_name();
  video_mode_ok = Sdlvideo.video_mode_ok (Screen.iwip ss) (Screen.ihip ss) 32 [`FULLSCREEN]
(*  video_mode_ok = Sdlvideo.video_mode_ok (Screen.frwip ss) (Screen.frhip ss) 32 [`FULLSCREEN]*)
  }

let print_info (video_info:video_info) = 
(*  print_endline("video_mem : "^soi(video_info.video_info.video_mem));*)
  print_endline("window_manager : "^string_of_bool(video_info.video_info.Sdlvideo.wm_available));
  print_endline("driver_name : "^video_info.driver_name);
  print_endline("full_screen_resolution_ok : "^soi(video_info.video_mode_ok) )

(*let load_image (strn) = Sdlloader.load_image (strn) *)

  (**************************** ABOUT FONTS ******************************)
  
let make_fontArray fontId =
  let f size =
    let fontId,existingSize = Font.valid fontId size in
    Sdlttf.open_font (Font.path fontId) (existingSize) in
  Array.init (Font.sizeMax+1) f
(* génère un tableau contenant les fonts de chaque taille pour un fontId donné *)

let make_fontFun fontIdList =
  let ilist = Ilist.init (make_fontArray) fontIdList in
  let legal size = cut 0 Font.sizeMax size in
  fun fontId size -> Array.get (Ilist.nth ilist fontId) (legal size)
  
module type VideoEnv = sig
(*  module PiCo : Picking.Color*)
  val bitmap : Bitmap.t
  val screen : Screen.t
  val font   : fontFun
end

(*********************************** MODULE DRAW *****************************************)

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

  val clear_screen : ?color:Color.t -> unit -> unit
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
 
module Draw(X : sig end) = struct

  module PiCo = Picking.Color
  module SV   = Sdlvideo
  type xAlign = Left | Middle | Right   | Left_or_right
  type yAlign = Top  | Medium | Bottom  | Top_or_bottom
  type box = Frui.box
  type t = int
  let fontFun = make_fontFun Font.idList
  let rsm     = RS.map 
  let rsv     = RS.value
  let screen_geometry, set_screen_geometry =
    let s, f = RS.create (Screen.create ()) in
    s, fun x -> f x

  let screenBS =
    let f ss =
      let w,h,flags = 
      if Screen.is_full ss
(*      then Screen.frwip ss, Screen.frhip ss, [`DOUBLEBUF; `FULLSCREEN]*)
      then Screen.iwip ss, Screen.ihip ss, [`DOUBLEBUF; `FULLSCREEN]
      else Screen.iwip ss, Screen.ihip ss, [`DOUBLEBUF] in
      SV.set_video_mode ~w:w ~h:h ~bpp:32 flags in
    rsm ~eq:( == ) f screen_geometry
  (* screen bitmap signal *)

  let pickingBS =
    let f screenB =
      let i = SV.surface_info screenB in
      SV.create_RGB_surface_format screenB [] ~w:i.SV.w  ~h:i.SV.h in
    rsm ~eq:( == ) f (screenBS) 
  (* picking bitmap signal *)

  let sInfo= rsm SV.surface_info screenBS
  let swip = rsm (fun i->i.SV.w) sInfo
  let ship = rsm (fun i->i.SV.h) sInfo
  let screen = screen_geometry
  let ycorratio = rsm (Screen.ycorratio) screen
  let swie = rsm (Screen.wie) screen
  let shie = rsm (Screen.hie) screen
  let ewip = rsm (Screen.ewip) screen
  let ehip = rsm (Screen.ehip) screen
  let ewipi= rsm (iof) ewip
  let ehipi= rsm (iof) ehip
  let sWie = rsm (Screen.wie) screen
  let sHie = rsm (Screen.hie) screen
  let clear_screen ?(color=Co.black) () = SV.fill_rect (rsv screenBS)  (Co.to_sdlvideo color )
  let clear_picking() = SV.fill_rect (rsv pickingBS) (PiCo.to_int32 (PiCo.none) )
  
  type blit_param =
  | SDLvideo of Co.t * (SV.surface -> int32 -> unit)
  | SDLgfx   of float option * Co.t * (SV.surface -> int32 -> unit)
  | SDLttf   of SV.surface * (s:SV.surface -> d:SV.surface -> unit)

  let blit ?pe blit_param =
    let coe (*color of element*) e = Int32.of_int ((PiCo.of_element e):>int) in
    (* conversion de l’id de l’élément en couleur SDL (int32) *)
    let fill su e = SV.fill_rect su (coe e) in
    (* remplissage d’une surface précise (=de la taille voulue) avec la couleur SDL de l’élément souhaité *)
    ( match blit_param with
      | SDLvideo (co, f)    -> f (rsv screenBS) (Co.to_sdlvideo co)
      | SDLgfx   (a, co, f) -> f (rsv screenBS) (Co.to_sdlgfx ?a co)
      | SDLttf   (su, f)    -> f ~s:su ~d:(rsv screenBS) );
    (* blit sur le bitmap screen *)
    ( match pe, blit_param with 
      | None  , _                    -> ()
      | Some e, SDLvideo (_ , f)     ->  f (rsv pickingBS) (coe e)
      | Some e, SDLgfx   (_ , _ , f) ->  f (rsv pickingBS) (coe e)
      | Some e, SDLttf   (su, f)     -> (fill su e; f ~s:su ~d:(rsv pickingBS)) )
    (* blit sur le bitmap picking *)
    (** fonction générale de blittage sur l’écran et le cas échéant le bitmap picking *)

  let strnDim fontId size string = Sdlttf.size_text (fontFun fontId size) string
  let strnWip fontId size string = fst (strnDim fontId size string)
  let strnHip fontId size string = snd (strnDim fontId size string)
 
  let strnExtrema ~xd ~yd ~xAlign ~yAlign ~font ~size string x y =
    let w, h = strnDim font size string in
    let xmin, xmax = match xAlign with
    | Left   -> x+xd, x+xd+w
    | Middle -> x-w/2, x+w/2
    | Right  -> x-xd-w, x-xd
    | Left_or_right -> if x+xd+w<(rsv swip) then x+xd, x+xd+w else x-xd-w, x-xd
    and ymin, ymax = match yAlign with
    | Top    -> y+yd, y+yd+h
    | Medium -> y-h/2, y+h/2
    | Bottom -> y-yd-h, y-yd
    | Top_or_bottom -> if y+h+yd<(rsv ship) then y+yd, y+yd+h else y-yd-h, y-yd in
    xmin, xmax, ymin, ymax
(* bords d’une string *)

  let strn ?pe ?(xd=0) ?(yd=0) ?(xAlign=Left) ?(yAlign=Medium) ?(font=Font.Default) ?(size=(rsv ehipi)) ?(co=Co.white) string x y =
    let su = Sdlttf.render_text_blended (fontFun font size) string ~fg:(Co.to_sdlttf co) in
    let blitx, _, blity, _ = strnExtrema ~xd ~yd ~xAlign ~yAlign ~font ~size string x y in
    let dst_rect   = SV.rect blitx blity 0 0 in
    let draw ~s ~d = SV.blit_surface ~src:s ~dst:d ~dst_rect:dst_rect () in
    blit ?pe (SDLttf (su, draw))
(** Draws on screen a string. The picking bitmap is eventually updated *)

  let strn_in_wh ?pe ?(font=Font.Default) ?(size=(rsv ehipi)) ?(co=Co.white) ~w ~h string x y =
      let bfhip = min (h*9/10) size in 
      (*base font hip : la font doit tenir en hauteur ds l’espace imparti*)
      let rec f ahip (* actual hip *) = 
        let swip = strnWip font ahip string in (*string width in pixels*)
        if swip <= w || ahip<=6 then ahip else f (ahip-1) in
      let size = f bfhip in (*font hip*)
    strn ~xAlign:Middle ~font ~size ~co string x y

  let get_contrastColor posList =
    let lumList = List.map (fun (x,y) -> (Co.lumina (Co.of_sdlvideo (SV.get_pixel (rsv screenBS) x y)))) posList in
    if (Tlist.mean lumList) <Co.lMax/2 then Co.white else Co.black
(** Returns white if the screen location is dark, black if is light. *)
    

  let antiStrn ?pe ?(xd=0) ?(yd=0) ?(xAlign=Left) ?(yAlign=Medium) ?(font=Font.Default) ?(size=(rsv ehipi)) string x y =
    let xmin, xmax, ymin, ymax = strnExtrema ~xd ~yd ~xAlign ~yAlign ~font ~size string x y in
    let w,h = xmax-xmin, ymax-ymin in
    let relativePosList = [ (2,2); (4,2); (6,2); (8,2); (2,4); (4,4); (6,4); (8,4); 
                            (2,6); (4,6); (6,6); (8,6); (2,8); (4,8); (6,8); (8,8)  ] in
    let posList = List.map (fun (a,b) -> xmin+w*a/10, ymin+h*b/10) relativePosList in
    strn ~xd ~yd ~xAlign ~yAlign ~font ~size ~co:(get_contrastColor posList) string x y
(** Draws on screen a string. The color is white if the screen location is dark, black if is light. The picking bitmap is eventually updated *)

  let resolution () =
    let f s dy dx = antiStrn ~xAlign:Right ~yAlign:Bottom ~font:Font.Sans s (rsv swip-dx) (rsv ship-dy*rsv ehipi) in
    let g dy s = f s dy (0) in
    g 0 ((soi (rsv swip))^" x "^(soi (rsv ship)));
    g 1 ((soi (rsv ewipi))^" x "^(soi (rsv ehipi)));
    g 2 ((soi (rsv sWie))^" x "^(soi (rsv sHie)))
   
  let scale s =
    Ext.iter (rsv ship/s+1) (fun y-> strn ~size:(rsv ewipi/2) ~co:(Co.gray 500) (soi (y*s)) 0 (y*s))
  
  let get_element x y =
    let co=PiCo.of_int32(SV.get_pixel (rsv pickingBS) x y) in
    PiCo.to_element co
  (** renvoie l’id de l’élement de l’écran pointé à la position (x,y) de l’écran *)

  let line ?pe ?a co x0 y0 x1 y1 =
    let draw = fun su int32 -> Sdl_gfx.line su x0 y0 x1 y1 int32 in
    blit ?pe (SDLgfx (a, co, draw))
  (** Draws on screen a line of the given color. The picking bitmap is eventually updated *)

  let horizLine ?pe co x y w = line ?pe co x y (x+w-1) y
  
  let vertiLine ?pe co x y h = line ?pe co x y x (y+h-1)

  let viseur x y =
    let w = rsv ewipi in
    let hw = w/2 in
    let co = get_contrastColor [(x-w,y); (x+w,y); (x,y-w); (x,y+w)] in (
    horizLine co (x-hw*3) (y     ) w;
    horizLine co (x+hw  ) (y     ) w;
    vertiLine co (x     ) (y-hw*3) w;
    vertiLine co (x     ) (y+hw  ) w )


  let fill_rect ?pe ?(a=1.) co w h x y = 
    let dst_rect = SV.rect ~x:x ~y:y ~w:w ~h:h in
    let draw = if a=1. 
      then fun su int32 -> SV.fill_rect ~rect:dst_rect su int32
      else fun su int32 ->
      let src_rect = SV.rect ~x:0 ~y:0 ~w:w ~h:h in
      let recSu    = SV.create_RGB_surface_format (su) [] ~w:w ~h:h in
      let _        = SV.fill_rect ~rect:src_rect recSu int32 in
      let _        = SV.set_alpha recSu (iof(a *. 255.)) in
      SV.blit_surface ~src:recSu ~dst:su ~dst_rect:dst_rect () in
    blit ?pe (SDLvideo (co, draw))
  (** Draws on screen a filled rectangle of the given color. The picking bitmap is eventually updated *)
  (** pour un alpha de 1., on utilise la fun  SV.fill_rect, bcp plus rapide, mais qui ne gère pas l’alpha  *)

(*  let load_bmp strn = Sdlloader.load_image "bmp/"^strn^".bmp"*)
  let load_bmp strn = Sdlvideo.load_BMP ( bitmap_path^"/"^strn^".bmp")

  let fill_bmp ?(a=1.) ?(sw=2560) ?(sh=2048) ?(sx=0) ?(sy=0) bmp x y =
    let src_rect = SV.rect ~x:sx ~y:sy ~w:sw ~h:sh in
    let dst_rect = SV.rect ~x:x  ~y:y  ~w:0  ~h:0 in
    let draw = fun su int32 ->
      SV.set_alpha bmp (iof(a *. 255.));
      SV.set_color_key bmp Int32.zero;
      SV.blit_surface ~src:bmp ~src_rect:src_rect ~dst:su ~dst_rect:dst_rect () in
    blit (SDLvideo (Co.black, draw))

  let thick_line ?pe ?a co w x0 y0 x1 y1 = 
    let draw = fun su int32 -> 
      let dl x0 y0 x1 y1 = Sdl_gfx.line su x0 y0 x1 y1 int32 in
      let dmin=(-w/2) in
      for d=dmin to dmin+w-1
      do
      if abs(x1-x0)<abs(y1-y0) 
      then dl (x0+d) y0 (x1+d) y1
      else dl x0 (y0+d) x1 (y1+d)
      done in
    blit ?pe (SDLgfx (a, co, draw))

  let rect ?pe ?a co e w h x y =
    thick_line ?pe ?a co e  x     y    (x+w)  y   ;
    thick_line ?pe ?a co e  x     y     x    (y+h);
    thick_line ?pe ?a co e (x+w)  y    (x+w) (y+h);
    thick_line ?pe ?a co e  x    (y+h) (x+w) (y+h)
  (* dessiner un rectangle (non plein) dont le tracé est d’épaisseur e *) 

  (****************** Frui.Backend ********************)

  let whxy_of_box {Frui.left ; right ; top ; bottom} = 
    (iof (right -. left)), (iof (bottom -. top)), (iof left), (iof top)
  (* dimensions d’une Frui.box et coordonnées du coin haut gauche d’une Frui.box *)

  let center_of_box {Frui.left ; right ; top ; bottom} = (iof ((right +. left)/. 2.)), (iof ((bottom +. top)/. 2.))
  (* coordonnées du centre d’une Frui.box *)


  (*unused début*)
  type font = Sdlttf.font
  let text_size font msg =
    let x, y = Sdlttf.size_text font msg in
    float x, float y
  let rect_of_box {Frui.left ; right ; top ; bottom} = 
    SV.rect (iof left) (iof top) (iof (right -. left)) (iof (bottom -. top))
  let draw_rect ?alpha co box =
    let open SV in 
    let dst = RS.value screenBS in
    fill_rect ~rect:(rect_of_box box) dst (Color.to_sdlvideo co)
  let screen_size = RS.map (fun g -> (Screen.iwip |- float) g,  (Screen.ihip |- float) g) screen_geometry
(*  let default_font = Sdlttf.open_font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 15*)
  let default_font = Sdlttf.open_font (Font.path Font.Sans) 15
  let draw_text ?(pos = `centered) font msg box = ()
  (*unused fin*)
 
  
  module Key = struct
    type t = Sdlkey.t
  end
  

  module Button = struct
    (*unused début*)
    let size style strn = text_size default_font strn (*largeur et hauteur du texte*)
    let default = ()
    (*unused fin*)
    type style = unit
    let font = Font.Sans


  let draw style string pressed box =
      let w, h, x, y = whxy_of_box box in
      let () = 
        fill_rect  ~a:0.8 (if pressed then Ci.bpb else Ci.bsb)   w h x y;
        rect ~a:0.8 (if pressed then Ci.bpl else Ci.bsl) 1 w h x y in
      let x,y = center_of_box box in
      let fhd = if pressed then (-1) else 0 in
      strn_in_wh 
        ~font ~size:(iof(rsv ehip *. 0.55) - fhd) 
        ~co:(if pressed then Co.brown 500 else Co.gray 800)
        ~w ~h string x y
  end

  module Label = struct
    type style = unit
    let font, size = Font.Serif, iof( rsv ehip *. 0.72 )
    let draw style string box =
      let w, h, x, y = whxy_of_box box in
      let x,y = center_of_box box in
      strn_in_wh 
        ~font ~size 
        ~co:(Co.gray 800)
        ~w ~h string x y

    let size style string = Couple.foi (strnDim font size string) (*largeur et hauteur (voulue) du texte*)
    let default = ()
  end

  module Div = struct
    type background = Color.t
    let border_size _ = { Frui.left = 0. ; right = 0. ; top = 0. ; bottom = 0. }

    let draw background focus box = 
      let w, h, x, y = whxy_of_box box in
      (match background, focus with
      | Some co, true  -> fill_rect ~a:0.8 co     w h x y
      | Some _ , false -> fill_rect ~a:0.6 Ci.wib w h x y
      | _ -> ());
      if focus then rect Ci.focus 1 (w+2) (h+2) (x-1) (y-1);
  end

  module Window = struct
    type id = WindowID.t
    type style = unit
    let default = ()
    let string_of_id = Si.window
  end

(* Video.Make prérequis *)
  let mouse_state () = 
    let open Sdlmouse in
    let (x,y,bs) = get_state () in
    { UUI.m_x = x ; m_y = y ;
      m_left = List.mem BUTTON_LEFT bs ;
      m_middle = List.mem BUTTON_MIDDLE bs ;
      m_right = List.mem BUTTON_RIGHT bs ; }

  let flip () = SV.flip (RS.value screenBS) ; (* flippage de screenBS sur l’écran *)


end

  (*
  let draw_image strn x y =
    let surface = load_image strn in
    blit_surface surface x y 
  *)
   (*
  let fill_inv_rect co w h x y =
    let z = Int32.zero in
    let surface = SV.create_RGB_surface [] w h 32 z z z z in
    let _ = SV.fill_rect surface (Co.to_sdlvideo co) in
    blit_surface surface x (y-h) (*rectangle dessiné jusqu'à son côté bas *)
  
  let put_pixel co x y = SV.put_pixel Screen.v x y (Co.to_sdlgfx co)
  *)
  
 (* 
  let draw_losange co x0 y0 x1 y1 x2 y2 x3 y3 =
    draw_line co x0 y0 x1 y1;
    draw_line co x2 y2 x1 y1;
    draw_line co x2 y2 x3 y3;
    draw_line co x0 y0 x3 y3;
    draw_line co x0 y0 x3 y3
  
  
  let draw_rect co w h x y =
    draw_horiz co x y w;
    draw_horiz co x (y+h-1) w;
    draw_verti co x y h;
    draw_verti co (x+w-1) y h
  
  
  let draw_triangle co x0 y0 x1 y1 x2 y2 = Sdl_gfx.triangle Screen.v x0 y0 x1 y1 x2 y2 (Co.to_sdlgfx co)
  
  let fill_triangle ?(seed = None) ?(gran = 0) co x0 y0 x1 y1 x2 y2 = 
    match gran,seed with 
      | 0,_ 
      | _,None->
  	Sdl_gfx.filled_triangle Screen.v x0 y0 x1 y1 x2 y2 (Co.to_sdlgfx co)
      | _,Some s -> 
  	Sdl_gfx.text_filled_triangle Screen.v x0 y0 x1 y1 x2 y2 (Co.to_sdlgfx co) s gran
  
  let fill_losange ?(seed = None) ?(gran = 0) co x0 y0 x1 y1 x2 y2 x3 y3 =
    match gran,seed with 
      | 0,_ 
      | _,None->
  	Sdl_gfx.filled_triangle Screen.v x0 y0 x1 y1 x2 y2 (Co.to_sdlgfx co);
  	Sdl_gfx.filled_triangle Screen.v x0 y0 x3 y3 x2 y2 (Co.to_sdlgfx co)
      | _,Some s -> 
  	Sdl_gfx.text_filled_triangle Screen.v x0 y0 x1 y1 x2 y2 (Co.to_sdlgfx co) s gran;
  	Sdl_gfx.text_filled_triangle Screen.v x0 y0 x3 y3 x2 y2 (Co.to_sdlgfx co) s gran
  
 
  *)
 

