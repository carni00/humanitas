(*

 ****************************** Graphique.ml ******************************


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
module RS = React.S
module Co = Color
module SA = Status.Atelier
module F  = Flexurae.Flexura

  
module Display = functor (Draw : Video.Draw) -> struct

module D = Draw
open Tfloat

(*let rsm  = RS.map*)
let rsv  = RS.value
(*let rsf  = rsv |- foi*)

let swip = rsv D.swip
let ship = rsv D.ship

   
let display_graphique atelier =

  let _     = Draw.clear_screen ~color:(Color.black)  () in
  let graph = SA.graph  atelier in
  let orbis = Game.orbis      ( SA.game   atelier ) in
  let pov   = Game.Player.pov ( SA.player atelier ) in
  if  pov  == Nid.none then () 
  else
    let flexurae = Nid.Nil.nth orbis.Orbis.flexuraeList pov in

    let x turn  =         iof ( (foi turn) * Graph.xppt graph ) in
    let y value = ship -- iof (  value     * Graph.yppu graph ) in

    let draw_flexura key (flexura : Flexurae.Flexura.t) = 

      let x_shift = max 0 (F.len flexura -- swip) in
      let x turn  = x turn -- x_shift in

      let color   = Ci.natioKey key in
      let g n s e = Draw.line color (x n) (y s) (x (n ++ 1)) (y e) ; e in 
      (* affiche un segment *)

      let yList   = List.rev (F.yList flexura) in
      let _ = match yList with
      | []            -> 0.
      | _y0 :: []      -> 0.
      | y0 :: y1 :: q -> 
        Draw.strn ~yAlign:Draw.Top ~co:color (Si.natioKey key) 0 (y y0) ;
        Tlist.fold_lefti g y0 (y1 :: q) in () in
    (* affiche d’une courbe *)

    let _ = Ilist.iteri draw_flexura flexurae in ()
    (* affiche de toutes les courbes d’une natio *)


end

(*EOF*)
