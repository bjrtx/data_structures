(** Physicist's amortized lazy queues after Okasaki.
    

Section 3.5.1 in Chris Okasaki's PhD thesis. Amortized constant-time push / peek / pop operations.*)

include Queue.S
val map: ('a -> 'b) -> 'a t -> 'b t