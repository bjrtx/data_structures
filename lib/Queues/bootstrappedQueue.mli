(** Bootstrapped queues based on structural decomposition after Okasaki.
    

Section  7.1.2 in Chris Okasaki's PhD thesis. Constant-time peek. Push and pop amortized log*(n), which is practically amortized constant-time. *)

include Queue.S

val map : ('a -> 'b) -> 'a t -> 'b t
