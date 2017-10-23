(** Custom pervasives definitions *)

(** {5 Typed id as integers }*)

type 'a id = private int

(** {5 Basic modules } *)

(***************************** MODULE COMPARE *********************************)
module Compare : sig
  type sign =
  | Pos
  | Neg
  | Nul
  
  type situation =
  | Above
  | Below
  | Equal of int
  | Between of int*int
  
  val sign : int -> sign
  val situ : int -> int -> situation 
  (** [situ x a] returns the situation of [x] compared to [a] *)
  val bsitu : int -> int -> int -> situation
  (** [situ x a b] returns the situation of [x] compared to [a] and [b] (requires [a] < [b]) *)
end
 
(***************************** MODULE COUPLE *********************************)

module Couple : sig
  val make  : 'a -> 'b -> ('a * 'b)
  val foi   : (int * int) -> (float * float)
  val sum   : (int * int) -> (int * int) -> (int * int)
  (** [sum  (a,b) (c,d)] returns [(a+c), (b+d)]*)
  val mean  : (int * int) -> (int * int) -> (int * int)
  (** [mean (a,b) (c,d)] returns [(a+c)/2, (b+d)/2]*)
end

(***************************** MODULE OPT *********************************)
module Opt : sig
  val value : 'a option -> 'a
  (** [value (Some a)] returns [a] ; [value None] raises [Failure] *)
  val optdo : ('a -> unit) -> 'a option -> unit
  (** [optdo f (Some a)] executes [f a] ; [optdo f None] does nothing *)
  val smap  : 'b -> ('a -> 'b) -> 'a option -> 'b
  (** [smap b f (Some a)] returns [f a] ; [smap b f None] returns [b] *)

end

(***************************** MODULE RANDOM **********************************)

module Random : sig
include module type of Random

  val sign   : unit -> int
  (** returns 1 or (-1) *)
  val sFloat : float -> float
  (** [sFloat bound] returns a float between [-bound] (inclusive) and [bound] (exclusive).*)

end

(***************************** CUSTOM PERVASIVES DEFINITIONS ***********************************)
(** {5 Various } *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b


val is   : bool -> 'a -> 'a -> 'a
(** [is bool a b] returns [a] if [bool] is true, else [b] *)

val ( => ) : bool -> 'a -> 'a -> 'a
(** [(bool => a) b] returns [a] if [bool] is true, else [b] *)

val xor  : bool -> bool -> bool
(** [xor a b] returns true if [a] or (exclusive) [b] is true *)

val xnor  : bool -> bool -> bool
(** [xnor a b] returns true when both a and b are true, or (inclusive) if none are true *)


val fst3 : ('a*'b*'c) -> 'a
val snd3 : ('a*'b*'c) -> 'b
val trd3 : ('a*'b*'c) -> 'c

val fst4 : ('a*'b*'c*'d) -> 'a
val snd4 : ('a*'b*'c*'d) -> 'b
val trd4 : ('a*'b*'c*'d) -> 'c
val frh4 : ('a*'b*'c*'d) -> 'd

val fst5 : ('a*'b*'c*'d*'e) -> 'a
val snd5 : ('a*'b*'c*'d*'e) -> 'b
val trd5 : ('a*'b*'c*'d*'e) -> 'c
val frh5 : ('a*'b*'c*'d*'e) -> 'd
val ffh5 : ('a*'b*'c*'d*'e) -> 'e



val identity : 'a -> 'a
(** [identity] returns its argument unchanged *)
val flip    : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
(** [compose g f] returns the function (g o f)*)
val ( |- )  : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c


val pi : float


(***********************************************************************************)
(** {5 Aliases } *)

val (^^) : 'a list -> 'a list -> 'a list
(** alias for [List.append] *)

val both_or_none  : bool -> bool -> bool
(** alias for [xnor] *)


(***********************************************************************************)
(** {5 Conversion functions} *)

val bof : float -> bool
(** [bof x] returns true if [x]>0. *)
val boi : int   -> bool
(** [boi i] returns true if [i]>0  *)

val iob : bool   -> int
(** [iob true] returns 1 ; [iob false] returns 0 *)
val iof : float  -> int
val ios : string -> int

val foi : int    -> float
val fos : string -> float

val soi : int   -> string
val sof : float -> string


(***********************************************************************************)
(** {5 Float/Int operations} *)

val min3 : 'a -> 'a -> 'a -> 'a
val max3 : 'a -> 'a -> 'a -> 'a
val cut  : 'a -> 'a -> 'a -> 'a

(** [cut xMin xMax x] ensures [x] is between [xMin] and [xMax] *)

(***********************************************************************************)

(** {5 Extension } *)

(** {5 Int operations} *)

module Ext : sig

val  fill_nth_bit : int -> int -> int
(** [fill_nth_bit int n] returns int with bit n set to 1 *)
val clear_nth_bit : int -> int -> int
(** [clear_nth_bit int n] returns int with bit n set to 0 *)
val  read_nth_bit : int -> int -> int
(** [read_nth_bit int n] returns 0 or 1 *)

val modulo : int -> int -> int
(** returns a positive integer. For example : [modulo (-3) 5] returns [2] *)

val euclid : int -> int -> (int*int)
(** [euclid n d] returns [ n/d, n mod d ]*)

(*val intSum : int -> int*)
(** somme des entiers de 1 à n *)

val arithmean : int -> int -> int 
(** returns (a+b)/2 *)
val weighmean : int -> int -> int -> int -> int
(** returns (a*c + b*d) / (c+d) *)

val rangeMean : int -> int -> int -> int -> int
(** [rangeMean a b range d] returns [(a*(range-d) + b*d)/range] *) 

val swy  : int list -> int list -> int -> int
(** [swy [a0; ... an] [b0; ... bn] x] renvoie l’ordonnée du point d’abscisse [x] situé sur la ligne brisée définie
par les points de coordonnées ([a0],[b0]); ...; ([an],[bn]). Les abscisses [a0] ... [an] sont supposées croissantes
; les valeurs [b0] et [bn] sont renvoyées pour [x]<[a0] et [x]>[an]. Exception [Invalid_argument] levée pour les listes vides ou de longueur différentes*)

val squot : int -> int -> int -> int
(** [squot a x y] returns [x/y] if [y<>0], else [a] *)

val quote : int -> int -> string -> int
(** [quote x y s] returns [x/y] if [y<>0], else raises [Failure s] *)

val log2 : int -> int
(** [log2 i] renvoie le logarithme de base 2 de [i] (réciproque de [lsl]) ; fonctionne pour [i]<10^9 *)

(***********************************************************************************)
(** {5 Iterations } *)

val iter : int -> (int -> unit) -> unit
(** [iter n f] runs [n] times the [f] function. That is, is equivalent to [begin f 0; f 1; ...; f (n-1); end] *)

val fold_left : ('a -> int -> 'a) -> 'a -> int -> 'a
(** [fold_left f a n] returns [f (... (f (f a 0) 1) ...) (n-1)] ; [f] is called [n] times ; [fold_left f a 0] returns [a] ; [fold_left f a 1] returns [f a 0]. *)

val applique : ('a -> 'a) -> 'a -> int -> 'a
(** [applique f a n] returns [f (f ... (f a))] ; [f] is called [n] times ; [applique f a 0] returns [a].*)

end
