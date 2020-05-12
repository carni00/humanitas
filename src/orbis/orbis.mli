(*

 ****************************** orbis.mli ******************************


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

type t = (* monde : ne change qu'en cas de end_of_turn ou new_game ou load_game *)
  {
  espace : Espace.t;
  regioMap : Rm.t;
(* données constantes du monde *)
  turn : Date.t;
  imperiumMap : Im.t;
  geographia : G.t;
  lucrum : Lucrum.t;
  natioIdList : Nid.t list;
  civitasList : CivitasList.t;
  natioList : NatioList.t;
  proximaeList : Proxima.t Nid.Nil.t Nid.Nil.t;
  junctiones : Junctiones.t;
  sd : Strategica.Data.d;
  addendum     : Eventum.t list; (*eventi de l’année*)
  vetera       : Eventum.t list;
  flexuraeList : Flexurae.t Nid.Nil.t;
(* données variable du monde *)
  }

val espace   : t -> Espace.t
val regioMap : t -> Rm.t

val create : Espace.forme -> Espace.resolution -> t
val update : t -> StrategicaList.t -> t

