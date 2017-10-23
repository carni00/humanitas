type t = { x : float ; y : float }

val null : t
val ex : t
val ey : t
val diag : float -> t
val make : float -> float -> t
val to_string : t -> string

module Op : sig
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : float -> t -> t
  val ( /: ) : t -> float -> t
end

val center : t -> t -> t
val orth : t -> t
