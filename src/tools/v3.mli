type t = {
  x : float ;
  y : float ;
  z : float
}

val make : float -> float ->float -> t
val null : t
val ex : t
val ey : t
val ez : t

val map  : (float -> float) -> t -> t
val map2 : (float -> float -> float) -> t -> t -> t

module Op : sig
  val ( +: ) : t -> t -> t
  val ( *: ) : float -> t -> t
  val ( /: ) : t -> float -> t
end


val sum     : t array -> t
val average : t array -> t

