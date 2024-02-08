(** Banker's amortized lazy queues after Okasaki.
    

Section 3.4.2 in Chris Okasaki's PhD thesis. Amortized constant-time operations.*)

include Queue.S

val size : 'a t -> int
