(** Input signature of the sortable-collection functors. *)
module type S = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] is respectively positive, negative or zero as [a] is larger, smaller or equal to [b]. *)
end
