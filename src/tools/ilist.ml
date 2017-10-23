(*

 ****************************** src/tools/ilist.ml ******************************


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

  type ('k, 'a) t = ('k*'a) list
  let null  = [] 
  let nth    l n = List.assoc n l
  let snth a l n = try (List.assoc n l) with Not_found -> a
  let to_list l  = List.map (fun (key,a)-> a) l
  let init f nil = List.map (fun key-> (key,f key)) nil
  let nMap f l = List.map (fun (key, a)->(key, f key a)) l
  let iter f l = List.iter(fun (key, a)-> f key a ) l
  let fold_left f s l = List.fold_left (fun s (key,a) -> f s a) s l
(*  let add l e    = e :: l*)
  let rec remove list a = match list with
    | [] -> []
    | (_,t) :: q when t = a -> q
    | t :: q -> t :: remove q a
  let rec nRemove list n = match list with
    | [] -> []
    | (t,_) :: q when t = n -> q
    | t :: q -> t :: nRemove q n
  let nFilter f = List.filter (fun (k,_)->(f k))
  let filter f = List.filter (fun (_,a)->(f a))
