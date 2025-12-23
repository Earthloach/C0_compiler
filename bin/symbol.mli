type t

val symbol : string -> t

val name : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

val pp : Format.formatter -> t -> unit

module Set : Set.S with type elt = t
module Map : Map.S with type key = t