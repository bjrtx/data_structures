module type S = sig
  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if [q] has no elements. *)

  val push : 'a -> 'a t -> 'a t
  (** [push x q] is obtained by the insertion of [x] into [q]. *)

  val peek : 'a t -> 'a option
  (** [peep x q] is either [None] if [q] is empty or [Some x] where [x] is the first-out element of [q]. *)

  val pop : 'a t -> 'a t option
end

module CatenableList (Q : S) = struct
  type 'a t = Empty | Cons of 'a * 'a t Lazy.t Q.t

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false

  let link cat s =
    match cat with Empty -> Lazy.force s | Cons (x, q) -> Cons (x, Q.push s q)

  let rec link_all (q : 'a t Lazy.t Q.t) : 'a t =
    let t : 'a t option = Option.map Lazy.force @@ Q.peek q in
    match Q.pop q with
    | Some qq when not @@ Q.is_empty qq ->
        link (Option.get t) (lazy (link_all qq))
    | _ -> Option.value t ~default:Empty

  let append xs ys =
    match (xs, ys) with
    | Empty, _ -> ys
    | _, Empty -> xs
    | _ -> link xs (lazy ys)

  let ( @ ) xs ys = append xs ys
  let peek = function Empty -> None | Cons (x, _) -> Some x

  let pop = function
    | Empty -> None
    | Cons (_, q) -> if Q.is_empty q then Some Empty else Some (link_all q)
end
