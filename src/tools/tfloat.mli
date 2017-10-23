(*

 ****************************** src/tools/tfloat.mli ******************************


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

(** Opening this module grants an easier readibility for float operations *)

(** {5 Float operations} *)

  val ( +  ) : float -> float -> float
  val ( -  ) : float -> float -> float
  val ( *  ) : float -> float -> float
  val ( /  ) : float -> float -> float
  val squot : float -> float -> float -> float
(** [squot a x y] returns [x/y] if [y<>0], else [a] *)
  val quote : float -> float -> string -> float
(** [quote x y s] returns [x/y] if [y<>0], else raises [Failure s] *)
  val u      : float
(** float unity. [u] is equal to [1.] *)
  val abs    : float -> float
(** absolute value : [abs x] returns [-x] if [x] is negative *)
  val next   : float -> float
(** [next x] returns the least integer value strictly greather than [x], whereas [ceil x] returns the least integer value greater than or equal to [x].
  For example, [next 1.] returns [2.] whereas [ceil 1.] returns [1.] *)


  val arrondi : ?p:int -> float -> float
(** [arrondi 2.49] returns [2.] ; [arrondi 2.5] returns [3.] ; [arrondi (-1) 0.15] returns [0.2] *)
  val modulo : float ->float -> float
(** returns a positive float. For example : [(-23.2) modulo 20.] returns [16.8] *)
  val swy  : float list -> float list -> float -> float
  (** [swy [a0; ... an] [b0; ... bn] x] renvoie l’ordonnée du point d’abscisse [x] situé sur la ligne brisée définie
  par les points de coordonnées ([a0],[b0]); ...; ([an],[bn]). Les abscisses [a0] ... [an] sont supposées croissantes
  ; les valeurs [b0] et [bn] sont renvoyées pour [x]<[a0] et [x]>[an]. Exception [Invalid_argument] levée pour les listes vides ou de longueur différentes*)

val barycenter : ?p:float -> float -> float -> float
val ibarycenter : int -> int -> float -> float -> float

val logb : int -> float -> float
(** [logb b x] renvoie le logarithme de base [b] de [x] *)
val log2 :        float -> float

  

(** {5 Int operations} *)

  val ( ++ ) : int -> int -> int
  val ( -- ) : int -> int -> int
  val mult   : int -> int -> int
  val div    : int -> int -> int



(***************************** MODULE COUPLE *********************************)

module Couple : sig
  val make  : 'a -> 'b -> ('a * 'b)
  val sum   : (float * float) -> (float * float) -> (float * float)
  (** [sum  (a,b) (c,d)] returns [(a+c), (b+d)]*)
  val mean  : (float * float) -> (float * float) -> (float * float)
  (** [mean (a,b) (c,d)] returns [(a+c)/2, (b+d)/2]*)
end

