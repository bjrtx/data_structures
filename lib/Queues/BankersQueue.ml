type 'a t = {
  left : 'a Stream.t;
  leftLength : int;
  right : 'a Stream.t;
  rightLength : int;
}
(* Representation invariant: leftLength is the length of left (resp. rightLength, right) and leftLength >= rightLength *)

let empty =
  { left = Stream.empty; leftLength = 0; right = Stream.empty; rightLength = 0 }

let is_empty { leftLength; _ } = leftLength = 0

let queue ({ left; leftLength; right; rightLength } as q) =
  if rightLength <= leftLength then q
  else
    let open Stream in
    {
      left = append left (reverse right);
      leftLength = leftLength + rightLength;
      right = empty;
      rightLength = 0;
    }

let push x q =
  queue
    {
      q with
      right = lazy (Cons (x, q.right));
      rightLength = succ q.rightLength;
    }

let peek = function
  | { left = (lazy Nil); _ } -> None
  | { left = (lazy (Cons (hd, _))); _ } -> Some hd

let pop = function
  | { left = (lazy Nil); _ } -> None
  | { left = (lazy (Cons (_, tl))); leftLength; _ } as q ->
      Some (queue { q with left = tl; leftLength = pred leftLength })

let map f ({ left; right; _ } as q) =
  { q with left = Stream.map f left; right = Stream.map f right }

let size { leftLength; rightLength; _ } = leftLength + rightLength
