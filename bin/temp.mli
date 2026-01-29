type t = int

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val show : t -> string

val counter : int ref

val reset : unit -> unit

val create : unit -> int

val name : int -> string
