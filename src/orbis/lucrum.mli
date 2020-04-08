(*

 ****************************** Lucrum.mli ******************************


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

type t = {
  factumList  : Partitio.t Nid.Nil.t; (* produits réels des nationes *) 
  damnumMap   : Partitio.t Nid.Nim.t;  (* pertes et tributs donnés, par incola(donneur), puis dominus(receveur) *)
  damnumList  : Partitio.t Nid.Nil.t; (* pertes et tributs donnés, par incola *)
  tributumMap : Partitio.t Nid.Nim.t;  (* tributs réels, par dominus, puis incola *)
  tributumList: Partitio.t Nid.Nil.t; (* tributs réels, par dominus *)
  lucrumList  : Partitio.t Nid.Nil.t; (* revenus réels *)
  }

val compute : G.t -> Junctiones.t -> Nid.t list -> Partitio.Record.t Nid.Nil.t -> t
(** calcul des revenus en fonctions des produits, de la géographie des territoires occupés, et des relations diplomatiques *)

val null : t

val factum : t -> Nid.t -> Partitio.t

val damnum : t -> Nid.t -> Nid.t -> Partitio.t
(** damnum subi par nid1 et du à nid2 *)

val damSum : t -> Nid.t -> Partitio.t
(** damnum total subi par nid1 *)

val tributum : t -> Nid.t -> Nid.t -> Partitio.t
(** tributum versé à nid1 par nid2 *)

val tribuSum : t -> Nid.t -> Partitio.t
(** tributum total versé à nid1 *)

val lucrum   : t -> Nid.t -> Partitio.t
(** revenu national *)
