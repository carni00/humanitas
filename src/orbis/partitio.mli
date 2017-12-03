(*

 ****************************** Partitio.mli ******************************


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

(** répartition du produit (ou du revenu) national *)

type t = {
  labor     : float;
  sapientia : float;
  religio   : float;
  militaria : float;
  oppressio : float;
  luxus     : float;
  otium     : float;
  }

type attributio = 
| LAB
| SAP
| REL
| MIL
| OPP
| LUX
| OTI

type usus = attributio*float
(** expérience des spécialistes *)

type natio = {
  g           : G.Natio.t;
  pyramid : Dx.Pyramid.t;
  artes : Ars.t list;
  politeia : Politeia.t;
  plebs : float;
  fides : float;
  libertas : float;
  efficientia : float;
  facultas : float;
  agriCopia : float;
  ususList : usus list;
  pp : t;
  }
(** données de la natio qui déterminent le calcul d’une partitio *)

(** {5 Lecture des données d’une partitio } *)

val attrib  : t -> attributio -> float

val lab     : t -> float
val sap     : t -> float
val mil     : t -> float
val rel     : t -> float
val opp     : t -> float
val lux     : t -> float
val oti     : t -> float

val cibus        : t -> float (* nourriture *)
val periti       : t -> float (* les spécialistes *)
val spolium      : t -> float
val stratiotikon : t -> float
val alienatio    : t -> float
val humanitas    : t -> float (* la civilisation *)
val servitium    : t -> float
val summa        : t -> float

(** {5 Création de partitio } *)

val null  : t
val debug : t

val make  : labor:float -> sapientia:float -> religio:float -> militaria:float -> oppressio:float -> luxus:float ->
otium:float -> t

val primary : natio -> t
val compute : natio -> Stratiotikon.t -> t


(** {5 Calcul de partitio à partir de partitio préexistantes } *)

type fun_cons =
| ADD_PL of t Nid.Nil.t
| ADD_P  of t 
| SBS_P  of t 
| MUL_I  of float
| DIV_I  of float
| FUN    of (attributio -> float -> float)
(** identifiant d’une fonction d’altération d’une partitio.t *)

val alter  : t -> fun_cons list -> t

val listSum : t Nid.Nil.t -> t (* non bugué *)
(** calcule la partitio somme d’une liste de partitio *)

val actio   : t -> natio -> t
(** attributiones -> natio -> actio *)

val fructus : t -> natio -> t
(** attributiones -> natio -> fructus *)

val factum  : t -> natio -> t
(** attributiones -> natio -> factum *)

val fructus_of_factum : t -> float -> t
(** factum -> plebs -> fructus *)

val damnum_of_factum : t -> (G.chora*G.chora) -> Junctiones.relatio -> t
(** factum -> g.natio -> factum *)
(*val tributum_of_damnum : t -> Junctiones.relatio -> t*)
(** factum -> relatio -> factum *)

module Record : sig
  type partitio = t
  type t
  val compute : partitio -> natio -> t
  val attrib  : t -> partitio
  val actio   : t -> partitio
  val fructus : t -> partitio
  val factum  : t -> partitio
  end

