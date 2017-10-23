(*

 ****************************** Typed_ID.ml ******************************


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

(*type 'a id = int*)

open Std

type 'a t  = int
type 'a id = 'a t

let unus  = 1

let oi (i) = i

module Til =
  struct
  type ('a) t  = ('a id * 'a) list
  let empty    = []
  let len      = List.length
  let nth    l i = try (List.assoc i l) with Not_found -> raise (Failure ("Til.nth : unknown id") )
  let snth a l i = try (List.assoc i l) with Not_found -> a
  let to_list  l =  List.map  (fun (nid,a)-> a  ) l
  let key_list l =  List.map  (fun (nid,a)-> nid) l
  let iter f l = List.iter(fun (i, a)-> f   a ) l
  let map  f l = List.map (fun (i, a)-> (i, f a)) l
  let mapi f l = List.map (fun (i, a)-> (i, f i a) ) l
  let add l a    = 
    let key_list = List.map (fun (i,a) -> i) l in
    let rec choose_id i = if not (List.mem i key_list) then i else choose_id (i+1) in
    (choose_id 0, a) :: l
  let assoc    i (l: ('a)t) = try (List.assoc i l) with Not_found -> raise (Failure ("Tid.Til.assoc : unknown id") )

  let rec find p = function
  | [] -> raise (Failure ("Tid.Til.find : unknown id") )
  | (i,a) :: q -> if p a then a else find p q

  let rec search p = function
  | [] -> None
  | (i,a) :: q -> if p a then Some a else search p q

  let rec filter p = function
  | [] -> []
  | (i,a) :: q -> if p a then a::(filter p q) else filter p q

  let rec alter (l: ('a)t) i f = match l with
    | [] -> failwith "Tid.Til.alter : unknown id"
    | (k,a)::q when k=i -> (k, f a)::q
    | a::q -> a::alter q i f



  let first_id (l: ('a)t)   = match l with (i,a)::q -> i | _ -> raise (Failure ("Tid.Til.first_id : empty list") )

  end



