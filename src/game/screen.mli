(** Description de la géométrie de l'écran *)

type t
(** screen status *)

val create : unit -> t

val is_full     : t -> bool
(** answers "is it currently fullscreen ?" *)
val is_upToDate : t -> bool
(** answers "is the screen up to date ?" *)
val alter : t -> Task.t -> t
(** alter screen status following the given task *)
(*val note_update : t -> t*)
(** note in the screen status that the screen is now up to date *)

val frwip : t -> int
(** fullscreen resolution width (in pixels) *)
val frhip : t -> int
(** fullscreen resolution height *)
val iwip : t -> int
(** inside wip : width of the surface drawed. Equals frwip when in fullscreen. Inside of the window when not in fullscreen.*)
val ihip : t -> int
(** inside hip : width of the surface drawed. Equals frhip when in fullscreen. Inside of the window when not in fullscreen.*)
val ratio : t -> float
(** ratio iwip/ihip de la résolution d’affichage *)
val wie   : t -> int
(** Width in elements *)
val hie   : t -> int
(** Height in elements *)
val ewip  : t -> float
(** element width in pixels *)
val ehip  : t -> float
(** element height in pixels *)


val ycorratio : t -> float
(** ratio de correction à appliquer à la hauteur des choses à afficher (non déjà affectées par ewip/ehip) (utile lorsque la résolution d’affichage diffère de la résolution native de l’écran *)

type box = float * float * float * float
val wip : int
val hip : int
val full_area : box
(*** FIXME ***)

(* val game_intro : unit -> UI.t *)
