(*

 ****************************** R.mli ******************************


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

(** Caractères physiques d’une portion de l’espace *)

type t

type river = {
  dir    : Espace.direction; (** sens du courant *)
  dest   : Rid.t;            (** regio vers lequel coule le flumen *)
  fluxus : int;              (** débit en m3/s *)
  }
(** caractères d’une portion de flumen situé sur une regio *)

type hydros =
| Inlandsis
| SeaIce
| Ocean
| Sea
| Lake
| River of river
| Dry
(** hydrographie d’une regio *)

type climat =
| Arid
| Semiarid
| Tropical
| Equatorial
| Temperate
| Subarctic
| Arctic
| Alpine
| Subalpine


type forest=
| Taiga
| TropicalF
| RainF
| Deciduous
| Coniferous
| OtherF

type grassland=
| Savanna
| OtherG

type tundra=
| ArcticT
| AlpineT

type climax =
| Desert
| Steppe
| Tundra of tundra
| Grassland of grassland
| Woodland  of forest
| Forest of forest
(** végétation climacique *)

val altMax : int 
(** degré d’altitude maximum *)

val inlandsisTemp : int
(** temp maximum supportant inlandsis *)

(** {5 calculs physiques }*)

val altitudeFun: int -> int
(** associe à un degré d’altitude l’altitude réelle en mètres *)

val hugrosFun  : h:hydros -> p:int -> int
(** associe à l’hydrographie et la pluviométrie l’humidité globale *)

val is_glacier : t:int -> bool
val is_seaIce  : alt:int -> t:int -> bool
val hospitalitasFun : int -> hydros -> int -> float
val physisValue  : int -> t -> int


(** {5 lecture des caractères d’une regio }*)

val none : t
val rid  : t -> Rid.t 
val alt  : t -> int
val area : t -> float 
(** superficie en km2 *)

val hydros : t -> hydros
val fluxus : t -> int
val coast : t -> bool
val mountain: t -> bool
val altitude: t -> int
val thermos : t -> int
val pluvia  : t -> int
val hugros  : t -> int
val ariditas: t -> int
val climat  : t-> climat
val climax  : t-> climax
val physis  : t -> int
val hospitalitas    : t -> float


(** {5 fonctions de création pas à pas d’une regio }*)

val make        : Rid.t -> alt:int -> area:float -> h:hydros -> t:int-> p:int-> coast:bool-> mountain:bool-> t
val riverUpdate : t -> int -> Espace.direction -> Rid.t -> t


