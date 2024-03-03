(** Banker's deques are parameterized by a constant [c] which must be larger than 1. It controls the maximum permitted imbalance. *)

module Make (_ : sig
  val c : int
end) : Deque.S
