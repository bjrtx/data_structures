(** Output signature of the sortable-collection functors. *)
module type S = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val sort : t -> elt list
end
