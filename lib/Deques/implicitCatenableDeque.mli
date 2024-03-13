(** Catenable deques using implicit recursive slowdonw after Okasaki.
    
All queue operations, including [tail] and [init], are amortized O(1).
*)

module type QS = sig
  include Deque.S

  val size : 'a t -> int
end

module Make : functor (_ : QS) -> Deque.S
