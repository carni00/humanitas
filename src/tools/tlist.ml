(*

 ****************************** tlist.ml ******************************


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

include List

let len = length

let  sum = List.fold_left ( +  ) 0
let fsum = List.fold_left ( +. ) 0.

let  prod = List.fold_left ( *  ) 1
let fprod = List.fold_left ( *. ) 1.

let snth a list n = try (List.nth list n) with Failure _ -> a

let rec last = function 
  | [] -> failwith "Tlist.last"
  | last :: [] -> last
  | t    :: q  -> last q
(* dernier élément d’une list *)

let rec optLast = function 
  | [] -> None
  | last :: [] -> Some last
  | t    :: q  -> optLast q
(* dernier élément d’une list *)

let min = function
  | [] -> raise (Invalid_argument "List.min")
  | e::q -> List.fold_left min e q
(* plus petite valeur d’une liste *)

let max = function
  | [] -> raise (Invalid_argument "List.max")
  | e::q -> List.fold_left max e q
(* plus grande valeur d’une liste *)

let mean = function
  | []   -> raise (Invalid_argument "List.mean")
  | e::q -> match fold_left (fun (s,n) a -> s+a, n+1) (e,1) q with
            | s,n -> s/n
(* moyenne des éléments de l *)

let init ?(b=0) n f =
  let rec g i =
  if i=b+n then []
  else f i :: g (i+1) in
  g b


let int n = init n (fun i->i)


let make n a = init n (fun i->a)

let mapi g l =
  let rec f n = function
  | [] -> []
  | e :: q -> g n e :: f (n + 1) q in
  f 0 l
(* comme List.map g l, sauf que g utilise le numéro des éléments de la liste *)


let rec fold_lefti g s l =
  let rec f n s = function
  | [] -> s
  | e::q -> f (n+1) (g n s e) q in
  f 0 s l


let iteri g l =
  let rec f n = function
  | [] -> ()
  | e :: q -> g n e ; f (n + 1) q in
  f 0 l
(* comme List.iter g l, sauf que g utilise le numéro des éléments de la liste *)

let map3 f al bl cl = 
  if len al<>len bl || len al<>len cl then raise (Invalid_argument "Tlist.map3")
  else let rec g al bl cl = match al,bl,cl with
  | a::aq,  b::bq, c::cq -> f a b c :: g aq bq cq 
  | _ -> [] in
  g al bl cl

let map4 f al bl cl dl = 
  if len al<>len bl || len al<>len cl || len al<>len dl then raise (Invalid_argument "Tlist.map4")
  else let rec g al bl cl dl = match al,bl,cl,dl with
  | a::aq,  b::bq, c::cq, d::dq -> f a b c d :: g aq bq cq dq
  | _ -> [] in
  g al bl cl dl

let map5 f al bl cl dl el = 
  if len al<>len bl || len al<>len cl || len al<>len dl || len al<>len el then raise (Invalid_argument "Tlist.map5")
  else let rec g al bl cl dl el = match al,bl,cl,dl,el with
  | a::aq,  b::bq, c::cq, d::dq, e::eq -> f a b c d e :: g aq bq cq dq eq
  | _ -> [] in
  g al bl cl dl el
(*  fixme*)


let rec fold_left3 f arg l1 l2 l3 =
  match l1, l2, l3 with
  | h1 :: t1, h2 :: t2, h3 :: t3 -> fold_left3 f (f arg h1 h2 h3) t1 t2 t3
  | [], [], [] -> arg
  | _ -> raise (Invalid_argument "Tlist.fold_left3")

let rec applique a = function
  | [] -> a
  | f::q -> applique (f a) q
(* applique une liste de fonction à 'a' *)

let assRev list = 
  let rec f = function
  | [] -> []
  | (a,b)::q -> (b,a)::f q in
  rev (f list)
(* reverse la clé et l’élément associé, conserve l’ordre initial *)

let rec optAssoc a = function
  | [] -> None
  | (k,b)::q when k=a -> Some b
  | (k,b)::q          -> optAssoc a q
(* comme assoc, mais renvoie une option plutôt que possiblement une exception *)

let keyList   list = List.map (fun (a,b) -> a) list
let valueList list = List.map (fun (a,b) -> b) list

let champ l a n = 
  let rec f i l = match i,l with
  | i, e::q when i<a -> f (i+1) q
  | i, e::q when i<(a+n) -> e::f (i+1) q
  | _ -> [] in
  f 0 l
(* sélection des éléments a à a+n-1 de la liste l *)

let census f l = List.fold_left (fun s b -> s+Std.iob(f b)) 0 l
(*nombre d’élément dans la liste l satisfaisant le prédicat f *)

let rec remove list e = match list with
  | [] -> []
  | t :: q when t = e -> q
  | t :: q -> t :: remove q e
(* retrait de l'élément e d'une liste, s'il existe *)

let rec nRemove list n = match list with
  | [] -> []
  | e :: q when n=0 -> q
  | e :: q -> e :: nRemove q (n-1)
(* retrait du x-iéme élément d'une liste, s'il existe *)


let nFilter g l =
  let rec f n = function
  | [] -> []
  | e :: q when (g n) -> e :: f (n + 1) q
  | e :: q -> f (n + 1) q in
  f 0 l
(* filter en fonction du rang dans la liste *)


let filter_n g l =
  let rec f n = function
  | [] -> []
  | e :: q when (g e) -> n :: f (n + 1) q
  | e :: q -> f (n + 1) q in
  f 0 l
(* liste des quantiemes des éléments satisfaisant g *)

let mem_n a l =
  let rec f n = function
  | [] -> raise Not_found
  | e :: q when e = a -> n
  | e :: q -> f (n + 1) q in
  f 0 l
(* quantieme de l’élément satisfaisant g *)

let rec following a l = match l with
  | e :: f :: q when e = a -> f
  | e :: f :: q -> following a (f::q)
  | _ -> raise (Failure "List.following") (*Not_found*)
(* suivant de a dans la liste l *) 

let following_or_first a list = 
  let rec g a l = match l with
  | e :: f :: q when e = a -> f
  | e :: [] when e = a -> List.hd list
  | e :: f :: q -> g a (f::q)
  | _ -> raise (Failure "List.following_or_first") in (*Not_found*)
  g a list
(* suivant de a dans la liste l, avec tour complet *) 

let rec following_or_last a l = match l with
  | e :: f :: q when e = a -> f
  | e :: [] when e = a -> e
  | e :: f :: q -> following_or_last a (f::q)
  | _ -> raise (Failure "List.following_or_last") (*Not_found*)
(* suivant de a dans la liste l ou a si c’est le dernier *) 

let previous_or_first a l = following_or_last a (rev l)
(* précédent de a dans la liste l ou a si c’est le premier *) 

let previous_or_last  a l = following_or_first a (rev l)
(* précédent de a dans la liste l, avec tour complet *)



let rec uniques = function
  | [] -> []
  | e :: q when List.mem e q -> uniques q
  | e :: q -> e :: uniques q
(** list of uniques elements of l *)



(****************************** Once used and not actually used *******************************)

(*let rec search f = function
  | [] -> None
  | a::q when (f a) -> Some a
  | a::q -> search f q
(* recherche d’un élément satisfaisant un prédicat, renvoie une option *)*)


(*let wMean al bl = match al,bl with
  | []   , _
  | _    , []    -> raise (Invalid_argument "List.wMean")
  | a::aq, b::bq -> match (fold_left2 (fun (pas,bs) a b -> pas+a*b, bs+b) (a*b,b) aq bq) with
                    | pas,bs -> pas/bs
(* moyenne des éléments de al pondéré par les valeurs du meme rang de bl *)*)


(*let elements el al = fold_left (fun el a-> if mem a el then el else a::el) el al
(* renvoie el (éléments list) augmentée des éléments nouveaux trouvés dans al *)*)


(*let rec complement il jl = match il,jl with 
  | i::iq, j::jq when i=j -> complement iq jq
  | i::iq, jl -> i :: complement iq jl
  | _ -> []
(* complement ordonné d'une liste ordonnée relativement à une troisième *)*)

(*let instances l = elements [] (List.map (fun a->(a,instanceNb a l)) l)
(*nombre d’appartion de chacun des éléments de l dans l*)
(*renvoie une liste de couple (élément, nb d’apparition*)*)

(*let instanceSort l = map fst (fast_sort (fun (a,n) (b,o) -> o-n) (instances l))
(* renvoie la liste des elements de l classés par ordre décroissant d’apparition *)*)


(*let eCompare l a b = 
  let rec f i = function 
  | [] -> raise  Not_found
  | e::q when e=a && i<>None && value i=b -> 1
  | e::q when e=a -> f (Some a) q
  | e::q when e=b && i<>None && value i=a -> (-1)
  | e::q when e=b -> f (Some b) q
  | e::q -> f i q in f None l
(* renvoie (-1) si a est avant b dans l, (1) si a est après b dans l *)*)
