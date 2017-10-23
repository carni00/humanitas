(*

 ****************************** src/tools/tmap.ml ******************************


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

  type ('b, 'c, 'a) t = (('b*'c)*'a) list
  let null  = []
(*  let b f y x = (y<=x => f y x) (f x y)*)
  let  nth map y x = List.assoc (y,x) map
  let snth a map y x = try (List.assoc (y,x) map) with Not_found -> a
(*  let bnth a map y x = b (snth a map) y x *)
  let init f bl cl = List.concat (List.map (fun b -> List.map (fun c-> ((b,c),f b c)) cl) bl)
  let nMap f l = List.map (fun ((b, c), a)->((b, c), f b c a)) l
