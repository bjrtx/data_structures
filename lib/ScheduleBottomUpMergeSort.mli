(* Scheduled bottom-up merge-sort structure after Okasaki.

   Logarithmic insertion, linear-time sorting. *)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Sortable = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val sort : t -> elt list
end

module Make (Ord : OrderedType) : Sortable with type elt = Ord.t
