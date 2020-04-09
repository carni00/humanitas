(*

 *************************** StrategicaList.ml ***************************


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

module Nil = Nid.Nil
module Nim = Nid.Nim
module Nl  = NatioList
module S   = Strategica

type nid = Nid.t
type t = Strategica.t Nid.Nil.t

let make nil = Nil.init (fun _nid -> S.make) nil 

let update (sl : t) (nl : NatioList.t) prll sd = 
(*  let nil = NatioList.nil nl in*)
(*  let f nid n s = Strategica.update nid n (N.*)
(*let update nid n prl s =*)


(*  Nil.map2 f (nl :> Natio.t Nil.t) prll sl*)
  Nil.map3 (S.update sd) (nl :> Natio.t Nil.t) prll sl

let get_strategica (sl : t) nid = Nil.snth S.make (sl :> Strategica.t Nil.t) nid


(*
module Data = struct

  type d = {
    stratiotikon_list : S.Stratiotikon.t Nil.t;
    rogatio_map   : Junctiones.rogatio Nim.t;
    tactic_map    : Junctiones.tactic Nim.t;
    }
  (* l'ensemble des données stratégiques, classées par thème pour application *)
  

  let rogatio d i j = Nim.snth Junctiones.N_rogatio d.rogatio_map i j
  let tactic  d i j = Nim.snth Junctiones.Defensive d.tactic_map i j
  
  
  let of_nil (nil) =
  ( {
    stratiotikon_list = Nil.init (fun nid->S.Stratiotikon.make) nil;
    tactic_map    = Nim.null;
    rogatio_map   = Nim.null;
    } : d )
  (* Pour donner une valeur initiale à humanitas.strategics *)
  
  
  let make (sl: t) =
    {
    stratiotikon_list = Nil.map (fun s->S.stratiotikon s) sl;
    tactic_map    = Nim.of_ll (Nil.map (fun s->S.tactic_list  s) sl);
    rogatio_map   = Nim.of_ll (Nil.map (fun s->S.rogatio_list s) sl);
    } 
  (* créations des stratégies IA puis
  reclassement par types de données de la liste des stratégies par nation *)
                                    
  let jStrategies d = ( {
    Junctiones.rogatio_map   = d.rogatio_map;
    Junctiones.tactic_map    = d.tactic_map;
    } : Junctiones.strategies )
    
end
*)
