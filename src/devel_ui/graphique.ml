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

open Std
module RS = React.S
module Co = Color
module SA = Status.Atelier
module F  = Flexurae.Flexura

  
module Display = functor (Draw : Video.Draw) -> struct

module D = Draw
open Tfloat

let rsm  = RS.map
let rsv  = RS.value
let rsf  = rsv |- foi

let swip = rsf D.swip
let ship = rsf D.ship

(*type pos = { x : int; y : int }*)
   
let display_graphique atelier =

let _     = Draw.clear_screen ~color:(Color.black)  () in
let graph = SA.graph  atelier in
let game  = SA.game   atelier in
let orbis = Game.orbis game in
let pov   = (Game.Player.pov ( SA.player atelier )) in
if pov == Nid.none then () 
else
  let flexurae = Nid.Nil.nth orbis.Orbis.flexuraeList pov in

(*  let pos graph = fun turn value ->*)
(*    let y = iof ( ship - value * Graph.yppu graph ) in ( x, y ) in*)

(*  let draw_segment color orig fin = Draw.line  color  orig.x  orig.y  fin.x  fin.y in*)


  let x turn  = iof ( (foi turn) * (Graph.xppt graph) ) in
  let y value = iof ( ship - value * Graph.yppu graph ) in

  let draw_flexura key (flexura : Flexurae.Flexura.t) = 
    let color = Ci.natioKey key in
(*    let color = Color.white in*)
    let g n s e = Draw.line color (x n) (y s) (x (n ++ 1)) (y e) ; e in
    let yList = List.rev (F.yList flexura) in
    let _ = match yList with
    | []            -> 0.
    | y0 :: []      -> 0.
    | y0 :: y1 :: q -> Tlist.fold_lefti g y0 (y1 :: q) in

(*  val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a*)

    () in
    
(*  val iter     : ('k->'a-> unit) -> ('k, 'a) t -> unit*)


(*  let orbis = Game.orbis game in*)
(*  let e     = orbis.Orbis.espace in*)
  let _ = Ilist.iteri draw_flexura flexurae in

(*  let _ = D.antiStrn ~xAlign:D.Right ~yAlign:D.Top ("wik="^wik^"("^scale^")") (rsv D.swip) (iof(rsv D.ehip)) in*)
   ()

end

(*EOF*)
