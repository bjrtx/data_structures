type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

let empty = lazy Nil
let is_empty = function (lazy Nil) -> true | _ -> false
let cons x t = lazy (Cons (x, t))

let rec append s t =
  lazy
    (match s with
    | (lazy Nil) -> Lazy.force t
    | (lazy (Cons (hd, tl))) -> Cons (hd, append tl t))

let rec fold_left f acc = function
  | (lazy Nil) -> acc
  | (lazy (Cons (hd, tl))) -> fold_left f (f acc hd) tl

let ( @ ) = append

let rec take n s =
  lazy
    (match (n, s) with
    | 0, _ | _, (lazy Nil) -> Nil
    | _, (lazy (Cons (hd, tl))) -> Cons (hd, take (pred n) tl))

let drop n s =
  let rec aux = function
    | 0, (lazy tl) -> tl
    | _, (lazy Nil) -> Nil
    | n, (lazy (Cons (_, tl))) -> aux (pred n, tl)
  in
  lazy (aux (n, s))

let reverse s = fold_left (Fun.flip cons) empty s

let rec to_list = function
  | (lazy Nil) -> []
  | (lazy (Cons (hd, tl))) -> hd :: to_list tl

let rec to_seq = function
  | (lazy Nil) -> Seq.empty
  | (lazy (Cons (hd, tl))) -> fun () -> Seq.Cons (hd, to_seq tl)

let rec map f = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (hd, tl))) -> cons (f hd) (map f tl)

let peek = function (lazy Nil) -> None | (lazy (Cons (hd, _))) -> Some hd
