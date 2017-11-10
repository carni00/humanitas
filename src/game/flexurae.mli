(*

 ****************************** flexurae.mli ******************************


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

(** Synthèse de l’évolution dans le temps des caractères de la natio *)


module Flexura : sig
  
  type t = {
    len   : int;
    x0    : int;
    yList  : float list;
    }
  (** une courbe. le nombre de points de la courbe est len
      les coordonnées du point nº n sont ( x0 + n, List.nth list n ) *)

end


type t = (Natio.key, Flexura.t) Ilist.t
(** les courbes d’une natio *)


val key_list : Natio.key list
(** la liste des id des courbes *)

val make     : t


