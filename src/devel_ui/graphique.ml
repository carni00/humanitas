(*

 ****************************** Scene2D.ml ******************************


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
module RS = React.S
module Co = Color

  
module Display = functor (Draw : Video.Draw) -> struct

module D = Draw
open Tfloat

let rsm  = RS.map
let rsv  = RS.value
let rsf  = rsv |- foi

 

let display_graphique game player =
  let orbis = Game.orbis game in
  let e     = orbis.Orbis.espace in
  let _ = Draw.clear_screen ~color:(Color.black)  () in

(*  let _ = D.antiStrn ~xAlign:D.Right ~yAlign:D.Top ("wik="^wik^"("^scale^")") (rsv D.swip) (iof(rsv D.ehip)) in*)
   ()

end

(*EOF*)
