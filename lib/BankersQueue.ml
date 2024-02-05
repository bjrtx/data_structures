type 'a t = {
  left : 'a Stream.t;
  leftLength : int;
  right : 'a Stream.t;
  rightLength : int;
}

let empty =
  { left = Stream.empty; leftLength = 0; right = Stream.empty; rightLength = 0 }

let isEmpty { leftLength; _ } = leftLength = 0

let queue q =
  if q.rightLength <= q.leftLength then q
  else
    {
      left = Stream.(append q.left (reverse q.right));
      leftLength = q.leftLength + q.rightLength;
      right = Stream.empty;
      rightLength = 0;
    }

let snoc x q =
  queue
    {
      q with
      right = lazy (Cons (x, q.right));
      rightLength = succ q.rightLength;
    }

let head q =
  match q.left with (lazy Nil) -> None | (lazy (Cons (hd, _))) -> Some hd

let tail q =
  match q.left with
  | (lazy Nil) -> None
  | (lazy (Cons (_, tl))) ->
      Some (queue { q with left = tl; leftLength = pred q.leftLength })
