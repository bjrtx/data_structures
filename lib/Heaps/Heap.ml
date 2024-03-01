module BinaryTree = Tree.Binary
module MultiwayTree = Tree.Multiway

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type Base = sig
  type elt
  type t

  val size : t -> int
  val empty : t
  val to_arbitrary_seq : t -> elt Seq.t
  val push : elt -> t -> t
  val merge : t -> t -> t
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
  val step : t -> (elt * t) option
  val peek : t -> elt option
end

module type PriorityQueue = sig
  include Base

  val to_ordered_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val sort : elt list -> elt list
  val pop : t -> t option
  val is_empty : t -> bool
end

module AddOps
    (Q : Base)
(*: PriorityQueue with type elt = Q.elt and type t = Q.t *) =
struct
  include Q

  let to_ordered_seq = Seq.unfold step
  let pop t = Option.map snd @@ step t
  let mem x heap = heap |> to_arbitrary_seq |> List.of_seq |> List.mem x
  let sort l = l |> of_list |> to_ordered_seq |> List.of_seq
  let is_empty t = Option.is_none @@ peek t
end

