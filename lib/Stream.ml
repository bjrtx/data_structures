type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

let empty = lazy Nil
let is_empty = function (lazy Nil) -> true | _ -> false
let cons x t = lazy (Cons (x, t))

let rec append (lazy s) t =
  match s with Nil -> t | Cons (hd, tl) -> cons hd (append tl t)

let rec fold_left f acc = function
  | (lazy Nil) -> acc
  | (lazy (Cons (hd, tl))) -> fold_left f (f acc hd) tl

let ( @ ) = append

let rec take n (lazy s) =
  lazy
    (match (n, s) with
    | 0, _ | _, Nil -> Nil
    | _, Cons (hd, tl) -> Cons (hd, take (pred n) tl))

let drop n s =
  let rec aux = function
    | 0, (lazy tl) -> tl
    | _, (lazy Nil) -> Nil
    | n, (lazy (Cons (_, tl))) -> aux (pred n, tl)
  in
  lazy (aux (n, s))

let reverse s = fold_left (Fun.flip cons) empty s

let rec to_list (lazy s) =
  match s with Nil -> [] | Cons (hd, tl) -> hd :: to_list tl

let rec to_seq (lazy s) =
  match s with
  | Nil -> Seq.empty
  | Cons (hd, tl) -> fun () -> Seq.Cons (hd, to_seq tl)

let rec map f (lazy s) =
  match s with Nil -> lazy Nil | Cons (hd, tl) -> cons (f hd) (map f tl)

let peek (lazy s) = match s with Nil -> None | Cons (hd, _) -> Some hd
let tail (lazy s) = match s with Nil -> None | Cons (_, tl) -> Some tl
