(*

 ****************************** ColorMap.ml ******************************


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

module Co=Color
module N=Co.Nuance

let lMax = Co.lMax
let iMax = Co.iMax

module Display = functor (Draw : Video.Draw) -> struct

  let screen=Draw.screen
  let ehip=iof (Screen.ehip screen)
  let sw=Screen.iwip screen
  let sh=Screen.ihip screen

  let champ () = 
    let cw=2 in
    let degres = sw/cw in
    let ch=2 in
    iter (degres) (fun n-> iter (sh/ch) (fun l->
    let nua = N.custom (N.circ *. (foi n) /. (foi  degres)) in
    let co = (Co.nil nua iMax (l*ch*lMax/sh)) in
    Draw.fill_rect co cw ch (n*cw) (l*ch) ))
  (*  ignore(wait_next_event [Key_pressed])*)
  
  
  let nuances () =
    let degres = N.degres*2 in
    let cw=sw/degres in
    let lumDegres = 10 in
    let ch=sh/(lumDegres+1) in
  
    iter (degres+1) (fun n-> iter (lumDegres+1) (fun l->
    let x,y = n*cw-cw*2/5 , l*ch in
    let co = (Co.gray (lMax*l/lumDegres)) in
    Draw.fill_rect co cw ch x y ));
  
    iter (degres+1) (fun n-> iter (lumDegres+1) (fun l->
    let x,y = n*cw-cw*2/5 , l*ch in
    let nua = N.custom (foi n *. 0.5) in
    let lum = lMax*l/lumDegres in
    let co =  (Co.nil nua iMax lum) in
    let drawStrn = Draw.antiStrn ~size:(ehip/2) (*~lum:lum*) in
    (Draw.fill_rect co (cw*4/5) (ch*4/5) x y ;
    if (n=degres/2) then drawStrn (soi lum) x y
    else if (l mod (lumDegres/4))=0 then drawStrn (N.to_strn nua)  x y )))
  
  let brown () = 
    let degres = N.degres in
    let cw=sw/degres in
    let lumDegres = 10 in
    let ch=sh/(lumDegres+1) in
  
    iter (21) (fun n-> iter (lumDegres+1) (fun l->
    let x,y = n*cw-cw*2/5 , l*ch in
    let nua = N.add N.orange ((foi (n-10))*. 0.2) in
    let int = 300 + l*25 in
(*    let lum = lMax*l/lumDegres in*)
    let co =  (Co.brown ~n:nua ~i:(int) 200) in
    let drawStrn = Draw.strn ~size:(ehip/2) in
    (Draw.fill_rect co (cw*4/5) (ch*4/5) x y ;
    if (n=degres/2) then drawStrn (soi int) x y
    else if (l mod (lumDegres/4))=0 then drawStrn (N.to_strn nua)  x y )))
 
  end
  
(*EOF*)
