module Make (Const : sig
  val c : int
end) =
struct
  let c = Const.c

  type 'a t = { f : 'a Stream.t; lenf : int; r : 'a Stream.t; lenr : int }

  let empty = { f = Stream.empty; lenf = 0; r = Stream.empty; lenr = 0 }
  let is_empty { lenf; lenr; _ } = lenf + lenr = 0

  let queue ({ f; lenf; r; lenr } as q) =
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      {
        f = Stream.take i f;
        lenf = i;
        r = Stream.(r @ reverse (drop i f));
        lenr = lenf + lenr - i;
      }
    else if lenr > (c * lenf) + 1 then
      let i = (lenf + lenr) / 2 in
      let lenr = lenf + lenr - i in
      {
        f = Stream.(f @ reverse (drop lenr r));
        lenf = i;
        r = Stream.take lenr r;
        lenr;
      }
    else q

  let cons x ({ f; lenf; _ } as q) =
    { q with f = Stream.cons x f; lenf = succ lenf } |> queue

  let head = function
    | { f = (lazy Nil); r; _ } -> Stream.peek r
    | { f; _ } -> Stream.peek f

  let tail ({ f; r; lenf; _ } as q) =
    match Stream.tail f with
    | None -> if Stream.is_empty r then None else Some empty
    | Some tl -> Some { q with f = tl; lenf = pred lenf }
end
