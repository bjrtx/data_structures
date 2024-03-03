(** Double-ended queues with implicit recursive slowdown after Okasaki.
    

[cons], [snoc], [tail] and [init] are amortized constant-time.
*)

include Deque.S
