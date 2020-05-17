(*

 ****************************** eventus.mli ******************************


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


module Actio : sig
  type t =
  | Civitas    of Civitas.t (* fondation de cité *)
  | Inventio   of Ars.t (* découverte d’ars *)
  | Propagatio of Ars.t (* acquisition d’ars par diffusion  *)
(*  | Mutatio    of Politeia.summa (* changement de régime *)*)
  | Bellum     of Nid.t (* déclaration de guerre à ..*)
  | Offensive  of Nid.t (* passage à l’offensive contre ...*)
  | Pax        of Nid.t (* paix conclue avec .. *)
end

type t
type eventum = t

val  actio   : t -> Actio.t
val  date    : t -> Date.t
val  acteur  : t -> Nid.t
val  spectateurs : t -> Nid.t list


module List : sig

  type t = eventum list
  val create : Date.t -> inl:(Ars.cognitio * Ars.t) list Nid.Nil.t -> ncl:Civitas.t list -> t
  val concat : vetera:t -> addendum:t -> t

end

