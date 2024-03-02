(** List implementation with constant-time concatenation. 
    
This is based on the 1985 paper by Hughes (who at the time did not yet use the name 'difference list').*)

type !'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val empty : 'a t
val append : 'a t -> 'a t -> 'a t
val cons : 'a -> 'a t -> 'a t
val push : 'a -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val snoc : 'a -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val size : 'a t -> int
val mem : 'a -> 'a t -> bool
val to_seq : 'a t -> 'a Seq.t
val to_arbitrary_seq : 'a t -> 'a Seq.t
val is_empty : 'a t -> bool

val backspace : 'a t
(** [append backspace (cons hd tl)] equals [tl].*)

val copy : 'a t
(** [append copy l] equals [l @ l].*)
