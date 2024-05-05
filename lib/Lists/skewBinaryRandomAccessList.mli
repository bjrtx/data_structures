type 'a t

val empty : 'a t
val cons : 'a -> 'a t -> 'a t
val head : 'a t -> 'a option
val tail : 'a t -> 'a t option
val lookup : 'a t * int -> 'a option
val update : 'a t * int * 'a -> 'a t option
