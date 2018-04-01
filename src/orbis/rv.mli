(*

 ****************************** Rv.mli ******************************


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

type nid = Nid.t 

type tegmen =
  | Hydros   of R.hydros
  | Desertum of R.climax (* aucune mise en valeur courante *)
  | Fields_and_woods
  | Fields_and_pasture
  | Pasture
  | Fields
  | Irrigation
  | Tmine
  | Turbs
(* couvert regional effectif = produit cartésien des conditions naturelles et de la mise en valeur humaine *)

module Incola : sig

  type oikos =
    | Saltus   (* terre peu mise en valeur (meilleures terres uniquement, élevage) *)
    | Ager     (* terre intégralement mise en valeur *)
    | Mine     (* exploitation des ressources du sous-sol *)
    | Urbs     (* grande cité *)
  (* forme et intensité de la mise en valeur de la regio par l’homme *)

  type dominium =
    | Mir           (* communauté paysanne *)
    | Latifundium   (* grande propriété privée *)
    | Minifundium   (* petites propriétés privées *)
  (* organisation sociale *)

  type t

  val make         : float -> nid -> oikos -> dominium -> float -> t      
  val create       : R.t   -> nid -> oikos -> int   -> t
  val colonus      : nid -> float -> float -> t

  val plebs        : t -> float
  val densitas     : float -> t -> float
  val nid          : t -> nid
  val oikos        : t -> oikos
  val dominium     : t -> dominium
  val instrumentum : t -> float
  val tegmen       : R.climax -> oikos -> tegmen
  val facultas     : float -> float -> float -> tegmen -> float

  module Next : sig
    val instrumentum : float -> Ars.t list -> float -> float
    val oikos        : R.t -> oikos -> tegmen -> float -> float -> float -> oikos * bool
    val dominium     : float -> float -> float -> float -> tegmen -> float -> dominium -> dominium
  end

end

module Brouillards : sig
  type t
  val null : t
  val init : nid -> t
  val mark : t -> nid -> t
  val read : t -> nid -> bool
  (** la natio nid connaît-elle la regio ? *)
end

type contents =
  | Desertum_for of int
  | Incol        of Incola.t

type t

val null : t
val make : nid -> contents -> t
val update : t -> nid -> contents -> t
val set_oikos_urbs : t -> t
(** mise à jour de l’oikos en urbs pour les rid fournies *)

val dominus         : t -> nid
val contents        : t -> contents
val incola          : t -> Incola.t option
val incola_id       : t -> nid
val plebs           : t -> float
val contents        : t -> contents
val brouillards     : t -> Brouillards.t
val instrumentum    : t -> float
val tegmen          : ?rv:t -> R.t -> tegmen
val silva           : ?rv:t -> R.t -> int
val is_farmable     : ?rv:t -> R.t -> bool
val facultas        : R.t -> t -> float
val densitas        : R.t -> t -> float

type passability =
| Passable
| Navigable
| Not_passable

val is_passable     : R.t -> t -> passability

module Fun : sig
(*  val tegmen     : R.hydros -> R.climax -> contents -> tegmen*)
  val chorability: ?inst:float -> tegmen -> float
  val isFarmable : ?inst:float -> tegmen -> bool
  val efficientia: float -> float -> float
end

type gRegio = {
  superficies    : float;
  facultas       : float;
  hospitalitas   : float;
  instrumentum   : float;
  plebs          : float;
  latifundium    : float;
}

val gRegio : float -> R.t -> Incola.t -> gRegio


