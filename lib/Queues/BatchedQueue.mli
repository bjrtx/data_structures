(** Batched queues.
    

Section 3.1.1 in Chris Okasaki's PhD thesis. Amortized constant-time push / peek / pop operations in ephemeral, not persistent use.*)

include Queue.S

val mem : 'a -> 'a t -> bool
val size : 'a t -> int
val rev : 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val to_seq : 'a t -> 'a Seq.t
val to_arbitrary_seq : 'a t -> 'a Seq.t
