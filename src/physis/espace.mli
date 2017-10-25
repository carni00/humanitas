(*

 ****************************** E.mli ******************************


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

(** Propriétés géométriques de l’espace sur lequel orbis repose *)

open Std

type t 

type direction =
| Nord
| NE
| East
| SE
| Sud
| SW
| West
| NW

val vecteur_of_direction : direction -> int*int
(** associe à Nord le couple (0,-1) etc. *)

val dir_includes : direction -> direction -> bool


type web =
| Quadral
| Octal

type forme =
| Cylinder of web
| Sphere

type resolution = 
| Higher
| High    
| Low   
| Lower
| Agglo   (*résolution optimale pour l’agglomération initiale de continents*)


(** {5 constantes propres à tout espace } *)

val lonMax   : float
(** longitude maximum de l’espace :  180. = 180°W *)

val lonMin   : float
(** longitude minimum de l’espace : -180. = 180°E *)

val legalLon : float -> float
(** ensures lonMin <= lon <= lonMax *)


val latMax   : float
val latMin   : float

val hid      : float
(** hauteur de l’espace en degrés : 160 *)
val wid      : float
(** largeur de l’espace en degrés : 360 *)


val hik      : float
(** hauteur de l’espace en km : env 17.800 *)
val wik      : float
(** largeur de l’espace en km : 40.000 *)
val adik     : float
(** average degree in kms : largeur moyenne en km d’un degré de longitude *)


val yxr      : int -> float
(** latitude (0..90) -> ratio entre la hauteur et la largeur de la regio de cette latitude *)
val latdid   : float -> float -> float
(** distance en degré de latitude entre deux latitudes *)
val lonEquals : float -> float -> float -> bool
(** teste l'égalité de deux longitudes (modulo la largeur du monde).
    lonEquals p a b est vraie si a=b à p degrés près *)


(** {5 caractères d’un espace } *)

val resolution : t -> resolution
val forme : t -> forme
val sir   : t -> int
(** size in regiones : nombre de regiones composant l’espace *)
val wir   : t -> int
(** width in regiones *)
val hir   : t -> int
(** height in regiones *)
val dimir : t -> int*int*int
(** dimensions in regiones : size, width, height *)
val rw    : ?lat:float -> t -> float
(** regio width in km *)
val rh    : t -> float
(** regio height in km *)
val rs    : ?lat:float -> t -> float
(** regio superficies in km2 *)
val rdim  : ?lat:float -> t -> float*float*float

val centerRid : t -> Rid.t
(** regio centrale de l’espace *)
val distance : (float*float) -> (float*float) -> float
(** distance en km entre 2 points définis par lat et lon *)


(** {5 création d’espace } *)

val create : forme -> resolution -> t
val dilate : t -> forme -> t
val projNb : resolution -> resolution -> int

(** {5 sélection et manipulation de regio } *)

module Regio : sig

val superficie : t -> Rid.t -> float
(** superficie d'une regio en km2*)

val latitude : t -> Rid.t -> float
val longitude: t -> Rid.t -> float
val coords   : t -> Rid.t -> float*float
(** coordonnées (latitude, longitude) d’une regio *)

val proximae : t -> Rid.t -> Rid.t list
(** liste des regiones frontalières d’une regio *)

val lesQuatre: t -> Rid.t -> Rid.t list
(** liste des quatre regiones frontalières d’une regio (espace plan) *)

val lesHuit  : t -> Rid.t -> Rid.t list
(** liste des huit regiones frontalières d’une regio (espace plan) *)

val proxima  : t -> Rid.t -> direction -> Rid.t
(** regio voisine d’une regio dans une direction *)

val distance : t -> Rid.t -> Rid.t -> float
(** distance en km entre 2 points définis par lat et lon *)

end

(** {5 Espaces spécifiques } *)

module Cylinder : sig
(** propriétés spécifiques aux espaces cylindriques *)

  val latMax   : float
  
  val  rid_of_ryx : resolution -> int -> int -> Rid.t
  val srid_of_ryx : resolution -> int -> int -> Rid.t
  val yx_of_rrid : resolution -> Rid.t -> int*int
  (** relation linéaire entre la position (x,y) sur le plan-cylindre et l’indentifiant (rid) de la regio *)

  val is_polar   : resolution -> Rid.t -> bool
  (** vrai lorsque la regio touche les bords haut et bas du plan-cylindre *)

  val randomRid  : ?polarExclusion:float -> resolution -> Rid.t
  (** renvoie une regio au hasard, avec exclusion éventuelle d’une proportion de latitudes polaires *)


  val regioWid   : resolution -> float
  (** largeur d’une regio en degré de longitude *)
  val regioHid   : resolution -> float
  (** hauteur d’une regio en degré de latitude *)
  val regioWik   : resolution -> float -> float
  (** largeur d’une regio en km, en fonction de la latitude *)
  val regioHik   : resolution -> float
  (** hauteur d’une regio en km *)

  val wir : resolution -> int
  val hir : resolution -> int
  end

