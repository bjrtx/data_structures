module Make (Const : sig
  val c : int
end) =
struct
  type 'a t = {
    f : 'a Stream.t;
    lenf : int;
    sf : 'a Stream.t;
    r : 'a Stream.t;
    lenr : int;
    sr : 'a Stream.t;
  }

  let c = Const.c

  let empty =
    {
      f = Stream.empty;
      lenf = 0;
      sf = Stream.empty;
      r = Stream.empty;
      lenr = 0;
      sr = Stream.empty;
    }

  let is_empty { lenf; lenr; _ } = lenf + lenr = 0
  let exec1 = function (lazy Stream.(Cons (_, s))) | s -> s

  let rec rotate_rev (lazy s) f a =
    let open Stream in
    match s with
    | Nil -> Stream.reverse f @ a
    | Cons (x, r) -> cons x (rotate_rev r (drop c f) (reverse (take c f) @ a))

  let rec rotate_drop i r f =
    let open Stream in
    if i < c then rotate_rev r (drop i f) empty
    else
      match r with
      | (lazy (Cons (x, r'))) -> cons x (rotate_drop (i - c) r' (drop c f))
      | _ -> failwith ""

  let queue ({ f; lenf; r; lenr; _ } as q) =
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      let sf = Stream.take i f in
      let r = rotate_drop i r f in
      { f = sf; lenf = i; sf; r; lenr = lenf + lenr - i; sr = r }
    else if lenr > (c * lenf) + 1 then
      let i = (lenf + lenr) / 2 in
      let j = lenf + lenr - i in
      let sf = rotate_drop j f r in
      let sr = Stream.take j r in
      { f = sf; lenf = i; sf; r = sr; lenr = j; sr }
    else q

  let cons x ({ f; lenf; sf; sr; _ } as q) =
    {
      q with
      f = Stream.cons x f;
      lenf = succ lenf;
      sf = exec1 sf;
      sr = exec1 sr;
    }

  let head { f; r; _ } =
    match Stream.peek f with None -> Stream.peek r | s -> s
end
