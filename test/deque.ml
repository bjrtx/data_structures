module type Deque = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val init : 'a t -> 'a t option
end

module _ : Deque = DataStructures.Deques.BankersDeque.Make (struct
  let c = 2
end)

module _ : Deque = DataStructures.Deques.ImplicitDeque

module _ : Deque = DataStructures.Deques.RealTimeDeque.Make (struct
  let c = 2
end)
