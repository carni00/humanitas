(*

 ****************************** eventus.ml ******************************


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


module Actio = struct

type t =
| Civitas    of Civitas.t (* fondation de cité *)
| Inventio   of Ars.t (* découverte d’ars *)
| Propagatio of Ars.t (* connaissance d’ars par diffusion  *)
| Mutatio    of Politeia.summa (* changement de régime *)
| Bellum     of Nid.t (* déclaration de guerre à ..*)
| Offensive  of Nid.t (* passage à l’offensive *)
| Pax        of Nid.t (* paix conclue avec .. *)

let of_inventio cognitio ars = match cognitio with
| `inventio -> Inventio ars
| `propagatio -> Propagatio ars

end


type eventus = {
  actio   : Actio.t;
  date    : Date.t;
  acteur  : Nid.t;
  spectateurs : Nid.t list;
}

type t = eventus

let make_eventus nl actio date acteur = {
  actio   ;
  date    ;
  acteur  ;
  spectateurs = NatioList.pil nl acteur ;
}

(*
let rec concat date nl actio_fun (nil:> (nid * 'a list) list) = 
  let rec of_list actio_fun acteur = function
    | []     -> []
    | e :: q -> make_eventus nl (actio_fun e) date acteur :: of_list actio_fun acteur q in
  let rec f = function
    | []              -> []
    | (nid,list) :: q -> of_list actio_fun nid list :: of_nil q in
  List.concat (f nil)
(* concaténation de data annuelle en liste d’eventus *)
*)

module Vetera = struct

  type t = (int*eventus) list
  let make = []
  let add ilist eventus = match (ilist:> (int*eventus) list) with
  | []       ->  (    0, eventus) :: []
  | (i,e)::q ->  ((i+1), eventus) :: q
  


(*  val make : (Ars.cognitio * Ars.t) list Nid.Nil.t -> t*)

end
