type 'a t = int * 'a Stream.t

let to_stream (_, s) = s
let size (n, _) = n
let empty = (0, Stream.empty)
let is_empty = function 0, _ -> true | _ -> false
let cons x (n, t) = (succ n, Stream.cons x t)
let append (m, s) (n, t) = Stream.(m + n, s @ t)
let fold_left f acc (_, s) = Stream.fold_left f acc s
let ( @ ) = append

let rec take n (m, (lazy s)) =
  match (n, s) with
  | 0, _ | _, Stream.Nil -> empty
  | _, Cons (hd, tl) -> cons hd (take (pred n) (pred m, tl))

let drop n (m, s) = if n >= m then empty else (m - n, Stream.drop n s)
let reverse (n, s) = (n, Stream.reverse s)
let to_list (_, s) = Stream.to_list s
let to_seq (_, s) = Stream.to_seq s
let map f (n, s) = (n, Stream.map f s)
let peek (_, s) = Stream.peek s
let tail (m, s) = Stream.tail s |> Option.map (fun t -> (pred m, t))
