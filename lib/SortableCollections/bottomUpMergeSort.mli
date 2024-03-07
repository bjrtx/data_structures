(* Bottom-up merge-sort structure after Okasaki.

   Amortized logarithmic insertion, amortized linear-time sorting. *)

module Make (Ord : OrderedType.S) : Sortable.S with type elt = Ord.t
