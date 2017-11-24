(*

 ****************************** Natio_id.ml ******************************


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

type t   = int
type nid = t

let none = 0     (* Color.Nuance.celadon *)
(* valeur nécessaire au fonctionnement du sous module List *)
let size = 20
(* nombre maximum de nations + 1 pour (none==0) = taille des matrices de type Nix *)
(* attention : veiller à ce que les données des noms de nations soient en nombre suffisant dans le module Si *)
let nb   = size - 1
(* nombre maximum de nations, none exclue *)

let oi (i)  = i
let ti (n)  = n
let tf (n)  = foi n
let compare = compare
let sort list = List.sort compare list


module Nil =
  struct
  type 'a t = (nid*'a) list
  let empty  = [] 
  let nth l n    = try (List.assoc n l) with Not_found -> failwith ("Nil.nth : nation with nid = "^(Std.soi n)^" not found")
  let snth a l n = try (List.assoc n l) with Not_found -> a
  let len = List.length
  let max_id = function [] -> 0 | (nid,a)::q -> List.fold_left (fun maxFound (newNid,a) -> max maxFound newNid) nid q +1
  let to_list  l =  List.map  (fun (nid,a)-> a  ) l
  let key_list l =  List.map  (fun (nid,a)-> nid) l
  let init f nil =  List.map  (fun nid-> (nid,f nid)) nil
  let of_list  l = Tlist.mapi (fun i a -> (i, a)) l
  let map    f l = List.map (fun (nid, a)->(nid, f a)) l
  let mapi   f l = List.map (fun (nid, a)->(nid, f nid a)) l
  let mapi_to_list f l = List.map (fun (nid, a)->(f nid a)) l
  let iter   f l = List.iter(fun (nid, a)-> f a ) l
  let fold_left f s l = List.fold_left (fun s (nid,a) -> f s a) s l
  let nfilter  f = List.filter (fun (k,_)->(f k))
  let filter   f = List.filter (fun (_,a)->(f a))
  let sort list  = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) list
  let map2 f al bl = List.map2 (fun (k,a) (k,b) ->(k, f a b)) al bl
  let smap2 a0 b0 f al bl = 
    let rec g al bl = match al,bl with
    | (ak,a)::aq,(bk,b)::bq when ak=bk -> (ak,f ak a  b ) ::g aq bq 
    | (ak,a)::aq,[]                    -> (ak,f ak a  b0) ::g aq bl
    | (ak,a)::aq,(bk,_)::_  when ak<bk -> (ak,f ak a  b0) ::g aq bl
    | []        ,(bk,b)::bq            -> (bk,f bk a0 b ) ::g al bq
    | (ak,_)::_ ,(bk,b)::bq when ak>bk -> (bk,f bk a0 b ) ::g al bq
    | _                                -> [] in
    g al bl
  (** mappage de deux Nil.t possiblement incomplètes (des couples clé/élément manquent). Les valeurs a0 et b0 sont retenues dans
  ces cas en input *)
  (** smap2 suppose que al et bl sont classées dans le même ordre (selon les k(eys)) *)
  let map3  f l1 l2 l3 = Tlist.map3 (fun (nid,a) (nid,b) (nid,c) ->(nid, f a b c)) l1 l2 l3
  let map4  f l1 l2 l3 l4 = Tlist.map4 (fun (nid,a) (nid,b) (nid,c) (nid,d) ->(nid, f a b c d)) l1 l2 l3 l4
  let map5  f l1 l2 l3 l4 l5 = Tlist.map5 (fun (nid,a) (nid,b) (nid,c) (nid,d) (nid,e) ->(nid, f a b c d e)) l1 l2 l3 l4 l5
  let rec set l (nid,x) = match l with
  | []                  -> [ (nid,x) ]
  | (n,a)::q when n=nid -> (n,x) :: q
  | (n,a)::q            -> (n,a) :: set q (nid,x)
  let add l (nid,x) = (nid,x) :: l
  end 


module Nim =
  struct
  type 'a t = ((nid*nid)*'a) list
  let empty  = []
  let b f y x = if y<=x then f y x else f x y
  let  nth   map y x = try (List.assoc (y,x) map) with Not_found -> raise (Failure ("Nim.nth : couple of nid not found"))
  let snth a map y x = try (List.assoc (y,x) map) with Not_found -> a
  let bnth a map z w = b (snth a map) z w 
(** à utiliser sur la map équivalente à une matrice symétrique (c-à-d ou nth y x == nth x y, mais ou seule nth y x est encodé *)
  let init  f nil = List.concat (List.map (fun y -> List.map (fun x-> ((y,x),f y x)) nil) nil) (* OK *)
  let iteri f map = List.iter (fun ((y,x),a) -> f y x a) map
  let mapi  f map = List.map  (fun ((y,x), a)->((y, x), f y x a)) map
  let filter = Nil.filter
  let nfilter= Nil.nfilter
  let  sort  = Nil.sort
  let norm       map = List.map (fun ((y,x),a) -> (b Couple.make y x,a)) map
  let sym        map = List.map (fun ((y,x),a) -> ((x,y),a)) map
  let sym_mapi f map = List.map (fun ((y,x),a) -> ((x,y),(f y x a))) map
  let smap2 = Nil.smap2
  (** mappage de deux Nil.t possiblement incomplètes (des couples clé/élément manquent). Les valeurs a0 et b0 sont retenues dans
  ces cas en input *)
  (** smap2 suppose que al et bl sont classées dans le même ordre (selon les k(eys)) *)
  let rec line map j = match map with
  | [] -> []
  | ((y,x), a)::aq when y=j -> (x,a) :: line aq j
  | ((y,x), a)::aq          -> line aq j


  let of_ll ll =
    let yList, ll = List.split ll in
    List.concat (List.map2 (fun y l -> (List.map (fun (x,a) -> (y,x),a) l) ) yList ll)
  let rec to_ll map = 
    let rec f z cl map = match map with
    | []            when cl <> [] -> (z,cl) :: []
    | ((y,x),a)::aq when z=y -> f z ((x,a)::cl) aq
    | ((y,x),a)::aq when z<y && cl<>[] -> (z,cl) :: f (z+1) [] map
    | ((y,x),a)::aq when z<y -> f (z+1) [] map
    | _ -> [] in
  (f 0 [] map)
  (* to_ll suppose que les éléments de la map sont ordonnés, par y, puis par x *)
  (* donc que la nil transmise à init soit ordonnée *)
  (* to_ll renverse l’ordre en x ( (x,a)::cl ), et conserve l’ordre en y *)
  end
(* une map est une liste dont la clé est un couple, ici de nid *)
(* les fonctions commençant par la lettre b concernent les maps symétriques *)
(* ( l’élément (1,2) peut être présent, l’élément (2,1) non )*)

module Nia = 
  struct
  include Array
  include Tarray
  type 'a t = 'a array
  let length = size (* Nid.size = le nb max de nationes + 1 *)
  let make x = Array.make size x
  let init x = Array.init size x
  let blit a1 a2 = Array.blit a1 0 a2 0 (Array.length a1)
  let map_of_nil a f l = Array.init (Nil.max_id l) (fun i->f (Nil.snth a l i))
  let to_array a = a
  let to_nil a =
    let rec f i list = if i < 0 then list else f (i - 1) ((i,Array.get a i) :: list) in
    f (size - 1) []
  let fsum = BatArray.fsum
  end 

module Nix =
  struct
  include Tmatrix
  let empty_mx    = Tmatrix.make size size None
(*  type 'a t = 'a array array*)
(*  let length = nb*)
(*  let surface = nb*nb*)
  let make      x = Tmatrix.make size size x
  let empty ()    = Tmatrix.make size size None
  let column mx x = Nil.of_list (Tmatrix.column mx x)
(*  let init f = Tmatrix.init nb nb f*)
(*  let get mx y x = mx.(y).(x)*)
(*  let line mx y = mx.(y)*)
(*  let set mx y x a = mx.(y).(x)<-a*)
  end


module List =
  struct
  type t = nid list
  let make = Tlist.init ~b:1 (nb) (fun i->i)
  let random nil = List.nth nil (Random.int(List.length nil))
  let sort   nil = List.sort compare nil
  end
(* liste de natio id, pas forcément complète, quoique make en créée des complètes *)



