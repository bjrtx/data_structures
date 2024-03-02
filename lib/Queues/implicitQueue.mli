type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val snoc : 'a -> 'a t -> 'a t
val peek : 'a t -> 'a option
val pop : 'a t -> 'a t option
