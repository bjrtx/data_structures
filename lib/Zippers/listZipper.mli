(** A data structure that traverses a list, keeping track of the current element, which it accesses or modifies in O(1) time. *)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val map : ('a -> 'b) -> 'a t -> 'b t
val of_list : 'a list -> 'a t
val of_list_end : 'a list -> 'a t
val to_list : 'a t -> 'a list
val current : 'a t -> 'a option
val left : 'a t -> 'a t
val right : 'a t -> 'a t

val reverse : 'a t -> 'a t
(** Reverses the underlying list. In the process, a cursor that was 'between' elements a and b, pointing to b, now is between b and a and points to a. 
    A cursor that was at the head is now past the end, and so on. *)

val insert : 'a -> 'a t -> 'a t
val change : 'a -> 'a t -> 'a t
