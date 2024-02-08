type 'a t = {
  leftCopy : 'a list;
  left : 'a list Lazy.t;
  leftLength : int;
  right : 'a list;
  rightLength : int;
}

let empty =
  { leftCopy = []; left = lazy []; leftLength = 0; right = []; rightLength = 0 }

let is_empty { leftLength; _ } = leftLength = 0

let map f q = {q with leftCopy = List.map f q.leftCopy; left = Lazy.map (List.map f) q.left; right = List.map f q.right}

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

let push x q =
  queue { q with right = x :: q.right; rightLength = succ q.rightLength }

let peek = function
  | { leftCopy = []; _ } -> None
  | { leftCopy = hd :: _; _ } -> Some hd

let pop q =
  if q.leftCopy = [] then None
  else
    Some
      (queue
         {
           q with
           left = lazy (List.tl (Lazy.force q.left));
           leftLength = pred q.leftLength;
         })
