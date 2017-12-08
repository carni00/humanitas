(*

 ****************************** Aedificium.mli ******************************


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

type t

type natio = {
  k       : t;
  g      : G.Natio.t;
  plebs  : float;
  pArtes : Ars.t list;
  cArtes : Ars.t list;
  pp : Partitio.t;
  pr : Partitio.Record.t;
  luc: Partitio.t;
  }

val sophia   : t -> float
val fides    : t -> float
val libertas : t -> float
val seditio  : t -> float
val ususList : t -> Partitio.UsusList.t
val vis      : t -> float


val null : t

val make : sophia:float -> fides:float -> seditio:float ->
ususList:(Partitio.UsusList.t) -> vis:float -> t 

val update : natio -> t
(*let create k pArtes cArtes pp p e = *)

