(* Scheduled bottom-up merge-sort structure after Okasaki.

   Logarithmic insertion, linear-time sorting. *)

module Make (Ord : OrderedType.S) : Sortable.S with type elt = Ord.t
