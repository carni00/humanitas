(*

 ****************************** co.mli ******************************


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

(** Easy generation of good-looking colors *)



(** {5 Advanced users modules } *)

module Nuance : sig
  (** Generation of identifiers of nuances *)

  type t = private float

  val degres : int
  (** number of default nuances (20) *)

  val circ : float
  (** circonference du cercle chromatique (20.) *)

  (** {5 20 ordered default nuances } *)

  val none      : t
  val bleu      : t
  val cyan      : t
  val ocean     : t
  val turquoise : t
  val celadon   : t
  val amande    : t
  val pomme     : t
  val vert      : t
  val lime      : t
  val jaune     : t
  val ble       : t
  val ambre     : t
  val orange    : t
  val corail    : t
  val rouge     : t
  val cerise    : t
  val magenta   : t
  val violine   : t
  val violet    : t
  val indigo    : t

  (** {5 custom nuances } *)

  val add       : t -> float -> t
  (** [add jaune x] builds a custom nuance from an existing one. [add jaune 0.] equals [jaune] ; [add jaune 1.] equals [ambre] ; [add jaune (-0.5)] returns a nuance equally between [vert] and [jaune]. *)

  val custom    : float -> t
  (** [custom x] buils a custom nuance [custom 0.] equals [bleu]. [custom 6.5] returns a nuance equally between
      [amande] and [emeraude]. [custom (-13.)] equals [custom 7.]. *)

  val cut       : t -> t -> t -> t
  (** [cut nMin nMax n] ensures [n] is between [nMin] and [nMax] *)

  val to_strn    : t -> string
  (** returns a string describing the nuance. *)

  val name_strn  : t -> string
  (** returns a string describing the nuance. For example all nuances between 0. (inclusive) and 1. (exclusive) will
      be described as "bleu" *)

  val float_strn : t -> string
  (** returns a string describing the nuance as a float. For example [float_strn indigo] returns ["19.00"] *)

  val arithmean  : t -> t -> t
  (** arithmean a b retuns the nuance just in between a and b, knowing a is before b in the chromatic order *)

  val weighmean  : t -> t -> float -> float -> t
  (** arithmean a b x y retuns a if y=0, b if x=0, else something between a and b *)
end

module Nil : sig
  (** Generation of colors defined by a triplet Nuance × Intensité × Luminosité *)
  (** A [nil] color is not usable by the video system and must be turned into a [Color.t] that is a [rgb] color *)
  (** See module Color Conversion functions *)

  type t
  val make : Nuance.t -> int -> int -> t
  (** [make n i l)] returns a [nil] color. [i] is the intensity of the color ; An intensity of 0 returns the color
      gray, whereas an intensity of 1000 returns a maximum intensity. [l] is the lightness of the color. A lightness
      of 0 returns black, whereas a lightness of 1000 returns white. Values under 0 are dealt as 0, whereas values
      above 1000 are dealt as 1000.*)

  val nuance : t -> Nuance.t
  (** [nuance nil] returns the nuance of the given nil color*)

  val intens : t -> int
  (** [intens nil] returns the intensity of the given nil color*)

  val lumina : t -> int
  (** [lumina nil] returns the lightness of the given nil color*)

  val nuaadd : t -> float -> t
  (** [nuaadd nil float] returns a color similar as [nil], excepting its nuance is [Nuance.add (nuance nil) float]*)

  val intadd : t -> int -> t
  (** [intadd nil int] returns a color similar as [nil], excepting its intensity is [(intens nil) + int]*)

  val lumadd : t -> int -> t
  (** [lumadd nil lum] returns a color similar as [nil], excepting its lightness is [(lumina nil) + lum]*)
end

module Rvb : sig
  (** Generation of colors defined by amounts of red, green and blue *)
  (** A [rvb] color must be turned into a [Color.t] *)
  (** See module Color Conversion functions *)

  type t

  val make : int -> int -> int -> t
  (** [make (r,g,b)] returns a rgb color with the given amounts of red, green and blue. Values under 0 are dealt as
      0., whereas values above 1000 are dealt as 1.*)

  val of_nil : Nil.t -> t
  (** [of_nil nil] computes a rgb color from a nil color.*)

  val add : t -> t -> t
  (** [add c1 c2] computes the rgb sum of the rgb c1 and c2 *)

  val lum_mult : t -> float -> t
  (** [lum_mult c r] computes a new rgb avec une luminosité ajustée d'un facteur r *)

  val black : t
  val white : t
  val red : t
  val yellow : t
  val green : t
  val cyan : t
  val blue : t
  val magenta : t
  val gray : int -> t
end

(******************************** MODULE COLOR *******************************)


(** {5 Basic flashy colors } *)

type t

val black    : t
val white    : t
val gray     : int -> t
(** [gray l] returns a gray of the given lightness.
    [gray 0] returns black, whereas [gray 1000] returns white *)

val brown    : ?n:Nuance.t -> ?i:int -> int -> t
(** [brown l] returns a brown of the given lightness.
    Default is [brown ~n:N.orange ~i:425 l] *)

val bleu     : t
val cyan     : t
val ocean    : t
val turquoise: t
val celadon  : t
val amande   : t
val pomme    : t
val vert     : t
val lime     : t
val jaune    : t
val ble      : t
val ambre    : t
val orange   : t
val corail   : t
val rouge    : t
val cerise   : t
val magenta  : t
val violine  : t
val violet   : t
val indigo   : t

val red      : t
(** alias for {!rouge} *)

val yellow   : t
(** alias for {!jaune} *)

val green    : t
(** alias for {!vert} *)

val blue     : t
(** alias for {!bleu} *)


(** {5 Computing custom colors } *)

val nil : Nuance.t -> int -> int -> t
(** [nil n i l] returns a color of the given nuance, intensity, and lightness
    An intensity of 0 returns the color gray, whereas an intensity of 1000 returns a maximum intensity. A lightness
    of 0 returns black, whereas a lightness of 1000 returns white. Values under 0 are dealt as 0, whereas values
    above 1000 are dealt as 1000.*)

val rvb : int -> int -> int -> t
(** [rvb r g b] returns a rgb color with the given amounts of red, green and blue.
    Values under 0 are dealt as 0., whereas values above 1000 are dealt as 1000.*)

val iMax : int
(** intensity maximum value (1000) *)

val lMax : int
(** lightness maximum value (1000) *)

val cMax : int
(** color (for a rgb color) maximum value (1000) *)

val lumina : t -> int
(** lightness of the given color (0..1000) *)

val lumadd : t -> int -> t

val arithmean : t -> t -> t
(** moyenne arithmétique de deux couleurs *)

(** {5 Conversion functions } *)

val of_nil : Nil.t -> t
(** builds a Color.t from a Nil.t *)

val of_rvb : Rvb.t -> t
(** builds a Color.t from a Rvb.t *)
(*val of_int : int -> t*)
(*(** builds a Color.t from a Rvb.t. Returns a specific color for integers from 0 to cMax*cMax*cMax-1 *)*)


(*val to_string : t -> string*)
(*val composition_strn : t -> string*)
val to_irr : float -> t -> (int*int*int*int)

(*val to_int      : t -> int*)
val to_sdlvideo : t -> int32
val to_sdlgfx   : ?a:float -> t -> int32
val to_sdlttf   : t -> (int*int*int)


val of_sdlvideo : int32 -> t
