(** Quadtrees *)

type 'a t
type rect = private { ul : V2.t ; lr : V2.t }

(** {6 General purpose functions } *)

val fold_rect : 
  ?ul:V2.t -> ?lr:V2.t -> 
  (rect -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val fold : (rect -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map  : (rect -> 'a -> 'b) -> 'a t -> 'b t

val iter : (rect -> 'a -> unit) -> 'a t -> unit

(** {6 Set-related functions } *)

type 'a set = (V2.t * 'a) list t
(** Container for a set of (positioned) values.
    Each leaf contains a list of values which are located in the same area. *)

val empty : ul:V2.t -> lr:V2.t -> 'a set
(** empty set *)

val insert : int -> V2.t -> 'a -> 'a set -> 'a set
(** [insert leaf_size pos value tree] returns a tree containing [value] at position [pos]
    with the guarantee that the leaf containing [value] has no more than [leaf_size] values *)


val set_fold      :                                       (V2.t -> 'a -> 'b -> 'b) -> 'a set -> 'b -> 'b
val set_fold_rect : ?ul:V2.t option -> ?lr:V2.t option -> (V2.t -> 'a -> 'b -> 'b) -> 'a set -> 'b -> 'b

val set_iter      :                                       (V2.t -> 'a -> unit) -> 'a set -> unit
val set_iter_rect : ?ul:V2.t option -> ?lr:V2.t option -> (V2.t -> 'a -> unit) -> 'a set -> unit
