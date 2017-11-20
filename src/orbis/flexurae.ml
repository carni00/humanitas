(*

 ****************************** flexurae.ml ******************************


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
module N=Natio



module Flexura = struct

  type t = {
    len    : int;
    x0     : int;
    yList  : float list;
    }
(** une courbe. le nombre de points de la courbe est len
    les coordonnées du point nº n sont ( x0 + n, List.nth list (len - n) ) *)

  let len f   = f.len
  let x0  f   = f.x0 
  let yList f = f.yList

  let make = 
    {
    len = 0;
    x0  = 0;
    yList = [];
    }

  let update n key f = 
    {
    f with
    len   = f.len + 1;
    yList = ( N.value n key ) :: f.yList;
    }


end



type t = (Natio.key, Flexura.t) Ilist.t
(** les courbes d’une natio *)

let key_list =

[
(*  N.Facultas     ;*)
(*  N.Plebs        ;*)
(*  N.Hospitalitas ;*)
  N.Instrumentum ;
(*  N.Efficientia  ;*)
  N.Famine       ;
  N.Copia        ;
  N.Tfg          ;
  N.Isf          ;
  N.DxVar        ;
  N.Alimonium_ratio ;
  N.Facultas_ratio  ;
  N.Sophia       ;
  N.Fides        ;
(*  N.Libertas     ;*)
(*  N.AgriCopia    ;*)
(*  N.Densitas     ;*)
]
(** la liste des id des courbes *)


let make = Ilist.init (fun k -> Flexura.make) key_list

let update natio flexurae = Ilist.nMap (fun key flexura -> Flexura.update natio key flexura) flexurae


