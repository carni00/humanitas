(*

 ****************************** G.mli ******************************


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

open Humanitas_physis
(*open Std*)

type nid   = Nid.t
type t
(** ensemble de données relatives à la terre entière *)

type chora = Rv.gRegio
(** ensemble de données relatives à une chora *)

val contactus : t -> nid -> nid -> bool
(** les nationes nid et nid sont-elles en contact *)

val facultas  : t -> nid -> float
(** potentiel de production de la chora, tegmen compris *)

val hospitalitas  : t -> nid -> float
(** hospitalitas de la chora *)

val instrumentum  : t -> nid -> float
(** instrumentum moyen de la chora *)

val plebs     : t -> nid -> float
(** plèbe totale de la chora *)

val imperium  : t -> nid -> float
(** superficie de l’imperium en km *)

val fines     : t -> nid -> nid -> chora
(** données de la frontière peuplée par nid1 et occupée par nid2 *)

val chora     : t -> nid -> chora
(** données de la chora de nid *)

val finesAmp  : t -> nid -> nid -> float
(** amplitude en km2 des frontière *)

val choraAmp  : t -> nid -> float
(** amplitude en km2 de la chora *)

val pil : nid list -> t -> nid -> nid list
(** [pil nil g n] renvoie la liste des nationes, par leur id, avec lesquelles la nation n est en contact *)

(*val warRatio : t -> J.t -> nid -> int*)

(*val lostRatio : t -> J.t -> nid -> int*)

(*val finesRatioMap : t -> nid list -> int Nid.Nim.t *)

val create : Espace.t -> Rm.t -> Im.t -> t



module Natio : sig
  type g = t
  type t 
  val make           : g -> nid list -> nid -> t
  val null           : t
  val imperium       : t -> float
  (** superficie de l’imperium en km2 *)

  val facultas       : t -> float  
  (** potentiel de production de la chora, tegmen compris *)

  val hospitalitas   : t -> float  
  (** hospitalitas de la chora *)

  val instrumentum   : t -> float 
  (** instrumentum moyen de la chora *)

  val latifundium    : t -> float 
  (** taux de latifundium de la chora = rapport plèbe latifundiaire / plèbe totale *)

  val plebs          : t -> float 
  (** plèbe totale de la chora *)

  val chora          : t -> chora 
  (** toutes les données de la chora *)

  val centrum        : t -> chora
  (** toutes les données du centrum : chora sous notre controle *)

  val funusList      : t -> chora Nid.Nil.t
  (** toutes les données des pertes : chora sous controle étranger ;
      la longueur de la liste est égale au nombre des nations étrangères réellement occupantes *)

  val finesList      : t -> chora Nid.Nil.t
  (** toutes les données des frontières : chora étrangères sous notre controle *)

  val choraAmp       : t -> float  
  (** amplitude en km2 de la chora *)

  val centrAmp       : t -> float 
  (** amplitude en km2 du centre *)

  val funusAmp       : t -> float Nid.Nil.t
  (** amplitude en km2 des frontière *)

  val pil            : t -> nid list 
  (** liste des id des poleis voisines *)

  val funuSumAmp     : t -> float
  (** amplitude totale en km2 de la chora perdue *)

  val funuSumPle     : t -> float
  (** plèbe totale de la chora perdue *)

  val fineSumAmp     : t -> float
  (** amplitude totale en km2 de la chora occupée *)

  val fineSumPle     : t -> float
  (** plèbe totale de la chora occupée *)

  val impAmp         : t -> float
  (** amplitude totale en km2 de l’imperium habité *)

  val impPle         : t -> float
  (** plèbe totale de l’imperium *)
end


val natioList : t -> nid list -> Natio.t Nid.Nil.t
(* liste des G.Natio.t *)

