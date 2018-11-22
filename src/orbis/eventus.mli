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


type actio =
| Civitas   of Civitas.t (* fondation de cité *)
| Inventio  of Ars.t (* découverte d’ars *)
| Cognitio  of Ars.t (* connaissance d’ars par diffusion  *)
| Mutatio   of Politeia.summa (* changement de régime *)
| Bellum    of Nid.t (* déclaration de guerre à ..*)
| Offensive of Nid.t (* passage à l’offensive *)
| Pax       of Nid.t (* paix conclue avec .. *)


type t = {
  actio   : actio;
  acteur  : Nid.t;
  date    : Date.t;
}


