(** Input signature of the sortable-collection functors. *)
module type S = sig
  type t

  val compare : t -> t -> int
end
