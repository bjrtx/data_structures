module type QS = sig
  include Deque.S

  val size : 'a t -> int
end

module Make : functor (_ : QS) -> sig
  include Deque.S

  val append : 'a t -> 'a t -> 'a t
end
