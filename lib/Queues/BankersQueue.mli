(** Banker's amortized lazy queues after Okasaki.
    

Section 3.4.2 in Chris Okasaki's PhD thesis. Amortized constant-time push / peek / pop operations.*)

include Queue.S

val size : 'a t -> int
(** Constant-time. *)

val map : ('a -> 'b) -> 'a t -> 'b t
