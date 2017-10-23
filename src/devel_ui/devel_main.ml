(*

 ****************************** devel_main.ml ******************************


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
(*
let rec cycle taskList status bitmap font =
  let status = Status.update status taskList in
  let bitmap = Bitmap.make (Status.screen status) in
  let module Draw = Video.Draw    (struct let bitmap=bitmap;; 
                                          let screen=Status.screen status;;
                                          let font  =font end) in
  let display_atelier atelier taskList =
    let module SA = Status.Atelier in
    let game   = SA.game atelier in
    let espace = Orbis.espace (Game.orbis game) in
    let tabula = SA.tabula atelier in
    match tabula with
    | Tabula.Qtree qtree ->
      let module Display = Scene2D.Display (struct module Draw=Draw ;;
                                                   let scene =SA.scene atelier ;;
                                                   let espace=espace;; end) in
      let _ = Display.regiones qtree in
      let effets_de_bord = function
    	| `describe_element (x,y) -> Display.element x y
    	| _ -> () in
      let _ = Display.regiones qtree in
      List.iter effets_de_bord taskList
  in
  Draw.clear_screen () ;
  let _ = match Status.atelier status with
  | None -> ()
  | Some atelier -> display_atelier atelier taskList in

  let _ = Draw.resolution () in
(*  let _ = Draw.scale 100 in*)
  let _ = Sdlvideo.flip (bitmap.Bitmap.screen) in
  let module NewTask = Handler.NewTask (Draw) in
  match (NewTask.get ()) with
  | [`quit ] -> print_endline "Rest, Avatar. You will need it!"
  | taskList -> cycle taskList status bitmap font

(****************************************************************************)

let run () =
  let _ =
    Sdl.init( [`VIDEO ; `EVENTTHREAD] ) ;
    at_exit Sdl.quit ;
    Sdlttf.init ();
    at_exit Sdlttf.quit;
    (*Sdlkey.enable_key_repeat () ~delay:500 ~interval:50*) in
  let status = Status.create() in
  let _      = Video.print_info (Video.video_info (Status.screen status)) in
  let bitmap = Bitmap.make (Status.screen status) in
  let font = Video.make_fontFun Font.idList in
  let taskList = [`load_game (Game.create(), Game.Player.admin)] in
  cycle taskList status bitmap font
*)
