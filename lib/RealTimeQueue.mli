type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val snoc : 'a -> 'a t -> 'a t
val head : 'a t -> 'a option
val tail : 'a t -> 'a t option
