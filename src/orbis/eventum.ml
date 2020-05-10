(*

 ****************************** eventum.ml ******************************


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
  | Propagatio of Ars.t (* acquisition d’ars par diffusion  *)
  | Mutatio    of Politeia.summa (* changement de régime *)
  | Bellum     of Nid.t (* déclaration de guerre à ..*)
  | Offensive  of Nid.t (* passage à l’offensive contre ...*)
  | Pax        of Nid.t (* paix conclue avec .. *)


let of_inventio cognitio ars = match cognitio with
| `inventio -> Inventio ars
| `propagatio -> Propagatio ars

end


type eventum = {
  actio   : Actio.t;
  date    : Date.t;
  acteur  : Nid.t;
  spectateurs : Nid.t list;
}

type t = eventum

let make_eventus actio date acteur = {
  actio   ;
  date    ;
  acteur  ;
  spectateurs=[];
}
(*
let make_eventus nl actio date acteur = {
  actio   ;
  date    ;
  acteur  ;
  spectateurs ;
(*  spectateurs = NatioList.pil nl acteur ;*)
}
*)

module List = struct

  let create date inl = 
    let eventus_list nid list = List.map (fun (cognitio,ars) -> make_eventus (Actio.of_inventio cognitio ars) date nid) list in 
    List.concat ( Nid.Nil.mapi_to_list eventus_list inl ) 
 
end

(*  val mapi_to_list : (nid->'a->'b) -> 'a t -> 'b list*)

(*  type t = (int*eventus) list*)
(*  let make = []*)
(*  let add ilist eventus = match (ilist:> (int*eventus) list) with*)
(*  | []       ->  (    0, eventus) :: []*)
(*  | (i,e)::q ->  ((i+1), eventus) :: q*)
  


(*  val make : (Ars.cognitio * Ars.t) list Nid.Nil.t -> t*)
(*let rec concat date nl to_actio (nil:> (Nid.t * 'a list) list) = *)
(*
let concat date nl to_actio nil =
  let rec list_compute to_actio acteur = function
    | []     -> []
    | e :: q -> make_eventus nl (to_actio e) date acteur :: list_compute to_actio acteur q in
  let rec nil_compute = function
    | []              -> []
    | (nid,list) :: q -> list_compute to_actio nid list :: nil_compute q in
  List.concat (nil_compute nil)
*)
(* concaténation de data annuelle en liste d’eventus *)



