(** Queue with amortized constant time insertion and removal. *)

type 'a t = 'a SizedList.t * 'a SizedList.t

let empty = SizedList.(empty, empty)
let is_empty q = q = empty
let mem elt (l, r) = SizedList.(mem elt l || mem elt r)
let size (l, r) = SizedList.(size l + size r)
let rev (l, r) = (r, l)
let push x (l, r) = (SizedList.cons x l, r)
let map f (l, r) = (SizedList.map f l, SizedList.map f r)

let peek (l, r) =
  let rec last = function [] -> None | _ :: l -> last l in
  match SizedList.(to_list l, to_list r) with
  | _, x :: _ -> Some x
  | l, [] -> last l

let rec pop (l, r) =
  match SizedList.tail r with
  | Some r -> Some (l, r)
  | None -> SizedList.(if is_empty l then None else pop (empty, rev l))

let to_seq (l, r) = Seq.append (SizedList.to_seq l) SizedList.(to_seq @@ rev r)

let to_arbitrary_seq (l, r) =
  Seq.append (SizedList.to_seq l) (SizedList.to_seq r)
