type 'a t = {
  leftCopy : 'a list;
  left : 'a list Lazy.t;
  leftLength : int;
  right : 'a list;
  rightLength : int;
}

let empty =
  { leftCopy = []; left = lazy []; leftLength = 0; right = []; rightLength = 0 }

let isEmpty { leftLength; _ } = leftLength = 0

let checkCopy q =
  match q.leftCopy with [] -> { q with leftCopy = Lazy.force q.left } | _ -> q

let checkRight q =
  if q.rightLength <= q.leftLength then q
  else
    let leftCopy = Lazy.force q.left in
    {
      leftCopy;
      left = lazy List.(append leftCopy (rev q.right));
      leftLength = q.leftLength + q.rightLength;
      right = [];
      rightLength = 0;
    }

let queue q = checkCopy @@ checkRight q

let snoc x q =
  queue { q with right = x :: q.right; rightLength = succ q.rightLength }

let head = function
  | { leftCopy = []; _ } -> None
  | { leftCopy = hd :: _; _ } -> Some hd

let tail q =
  if q.leftCopy = [] then None
  else
    Some
      (queue
         {
           q with
           left = lazy (List.tl (Lazy.force q.left));
           leftLength = pred q.leftLength;
         })
