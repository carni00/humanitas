(*

 ****************************** flumen.mli ******************************


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

(** Appréhension des cours d’eaux et de leurs affluents comme des arbres *)

type regio = {
  rid : Rid.t;
  direction : Espace.direction; 
  (** position relative de l’aval*)
  dest : Rid.t; (** regio située en aval *)
  fluxus : int; 
  (** flux/débit*)
  }
(** portion de fleuve située sur une regio *)

type t = 
  {
  fid : int; 
  (** identifiant : sert à retrouver le fleuve dans lequel un affluent se jette, dans la génération des
  fleuves*)
  cours : regio list; 
  (** liste des regiones traversée par le fleuve : haut de la pile = aval *)
  origine : origine;
  fin : fin;
  }
(* un flumen.t est en réalité un fleuve ou un segment de fleuve (cas d’un fleuve ayant des affluents*)

and origine =
| Source
| Affluents of t list
(** les affluents s’entendent de tous les cours d’eaux, sans exception, qui se jette dans le confluent. On ne
distingue pas le « fleuve principal » de ses « affluents » au sens commun *)

and fin =
| Estuaire  of Rid.t (** la case d’océan dans laquelle un fleuve se jette*)
| Confluent of Rid.t (** la première case du fleuve dans lequel un affluent se jette, soit graphiquement la regio de
confluent*)
| NoF (** situation initiale lors de la génération d’un fleuve*)

val null : t
(** fleuve inexistant *)

val len : t -> int
(** longueur du fleuve en regiones *)

val iter : (regio -> unit) -> t -> unit
(** iter sur chaque Flumen.regiones du flumen et de ses affluents *)



(** {5 fonctions de création pas à pas d’un fleuve complet dans sa définition } *)

val make : int -> Rid.t-> Espace.direction -> Rid.t -> t 
(** associe au numéro du fleuve, sa regio-source, sa direction initiale un Flumen.t (d’une case) *)

val add  : t   -> Rid.t-> Espace.direction -> Rid.t -> t 
(** ajoute au fleuve t la regio rid, en précisant l’orientation nle du fleuve à cette regio là *)

val setEstuaire : t -> Rid.t -> t 
(** redéfinit la {!fin} du fleuve comme étant un estuaire situé sur cette regio *)

val setConfluent: t -> Rid.t -> int -> t 
(** redéfinit la {!fin} du fleuve comme étant un confluent situé sur cette regio *)

val cat : t -> t -> t 
(** insère un nouvel affluent à un flumen existant, à la rid confluent *)

val fluxusUpdate : (Rid.t->int) -> (Rid.t->int) ->(Rid.t->int) -> t -> t
(** calcule le débit pour tout le cours d’un fleuve et de ses affluents *)

(* EOF *)
