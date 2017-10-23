(*

 ****************************** Screen.ml ******************************


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
open React

type param = {
  fullscreen : bool;
  scrwip : int;
  scrhip : int;
  frwip : int;
  frhip : int;
  wmiwip : float;
  wmihip : float;
  }

type t = {
  p : param;
  scrBmpUpToDate : bool;
  iwip : int;
  ihip : int;
  wie  : int;
  hie  : int;
  ewip : float;
  ehip : float;
  ycorratio : float;
  }


let compute p = 
  let snratio = (foi p.scrwip) /. (foi p.scrhip) (*screen native ratio*) in
  let frratio = (foi p.frwip) /. (foi p.frhip) in (*screen native ratio*)
  let iwip  = (if p.fullscreen then p.frwip else iof p.wmiwip) in
  let ihip  = (if p.fullscreen then p.frhip else iof p.wmihip) in
  let ycorratio = (snratio /. frratio) in (*ratio de correction en y/x à appliquer *)
  let cwip  = iof ( (foi iwip) *. (ycorratio ** 0.5) ) in
  let wie   = 40 + (cwip-800)/80 in (* width in elements *)
  (* l’amélioration de la résolution est partiellement utilisée pour augmenter le nombre d’éléments, pour le reste pour augmenter la résolution des éléments *)
  let ewip  = (foi iwip /. foi wie) in (* element width in pixels *)
  let hie   = iof (Tfloat.arrondi (foi ihip) /. (ewip *. ycorratio )) in (* heigth in elements *)
  let ehip  = foi ihip /. foi hie in (* element high in pixels *)
  {
  p = p;
  scrBmpUpToDate = true;
  iwip = iwip;
  ihip = ihip;
  wie  = wie;
  hie  = hie;
  ewip = ewip;
  ehip = ehip;
  ycorratio = ycorratio (*ratio de correction en y/x à appliquer *)
  }


let init_param () = 
  let scrwip = Conf.int "SCRWIP" 1280 in
  let scrhip = Conf.int "SCRHIP" 800 in
  let frwip = Conf.int "FRWIP" scrwip in
  let frhip = Conf.int "FRHIP" scrhip in
  let fullscreen = Conf.bool "FULLSCREEN" false in
  let wmiwip = foi (Conf.int "WMIWIP" (frwip - 52)) in
  let wmihip = foi (Conf.int "WMIHIP" (frhip - 28)) in
    {
    scrwip;
    scrhip;
    frwip;
    frhip;
    fullscreen;
    wmiwip;
    wmihip;
    }


let alter_param p = function
| `switch_fullscreen -> print_endline("switch_fullscreen") ; { p with fullscreen = not p.fullscreen }
| `screenSizeAlter f when p.fullscreen=false -> 
                        { p with wmiwip = p.wmiwip *. f;
                                 wmihip = p.wmihip *. f  }
(* en mode fulscreen, wmiwip et wmihip ne sont ni utilisés, ni modifiables *)
| _ -> p

(************************************************)

let create () = compute (init_param ())


let alter screen task = compute (alter_param screen.p task)


let is_upToDate s = s.scrBmpUpToDate
let is_full     s = s.p.fullscreen
let ycorratio   s = s.ycorratio (*ratio de correction en y/x à appliquer *)
let frwip       s = s.p.frwip
let frhip       s = s.p.frhip


let iwip  s = s.iwip
let ihip  s = s.ihip
let wie   s = s.wie
(* l’amélioration de la résolution est partiellement utilisée pour augmenter le nombre d’éléments, pour le reste pour augmenter la résolution des éléments *)
let ewip  s = s.ewip
let hie   s = s.hie
let ehip  s = s.ehip


(* l’écran contient un nombre entier d’éléments approximativement carrés *)
(* 40 * 30 de 20*20 en 800*600 *)
(* 42 * 32 de 24*24 en 1024*768 *)
(* 48 * 30 de 30*30 en 1440*900 *)
(* 50 * 37 de 32*32 en 1600*1200 *)
(* 51 * 32 de 32*32 en 1680*1050 *)
(* 54 * 34 de 35*35 en 1920*1200 *)

let ratio s = foi (iwip s) /. foi (ihip s)
let omega = atan (104. /. 253.)


type box = float * float * float * float

let wip = Conf.int "WIP" 1024
let hip = Conf.int "HIP" 768
let full_area = 0., 0., float wip, float hip
(*** FIXME ***)


(* let game_intro () = *)
(*   let open UI in  *)
(*   (new panel *)
(*      (new button) *)
(*      (S.const screen_box)  *)
(*      :> UI.t) *)
