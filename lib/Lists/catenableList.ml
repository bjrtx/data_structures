module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
end

module Make (Q : S) = struct
  type 'a t = Empty | Cons of 'a * 'a t Lazy.t Q.t

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let leaf x = Cons (x, Q.empty)

  let link cat s =
    match cat with Empty -> Lazy.force s | Cons (x, q) -> Cons (x, Q.push s q)

  let rec link_all q : 'a t =
    let t =
      q |> Q.peek |> Option.map Lazy.force |> Option.value ~default:Empty
    in
    match Q.pop q with Some qq -> link t (lazy (link_all qq)) | _ -> t

  let append xs ys = link xs (lazy ys)
  let ( @ ) xs ys = append xs ys
  let peek = function Empty -> None | Cons (x, _) -> Some x
  let pop = function Empty -> None | Cons (_, q) -> Some (link_all q)
  let cons x xs = leaf x @ xs
  let snoc x xs = xs @ leaf x
end
