(** Double-ended queues with implicit recursive slowdown after Okasaki.
    

[cons], [snoc], [tail] and [init] are amortized constant-time.
*)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val cons : 'a -> 'a t -> 'a t
val head : 'a t -> 'a option
val tail : 'a t -> 'a t option
val snoc : 'a -> 'a t -> 'a t
val last : 'a t -> 'a option
val init : 'a t -> 'a t option
