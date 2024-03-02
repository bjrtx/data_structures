module Make (Const : sig
  val c : int
end) =
struct
  type 'a t = {
    f : 'a SizedStream.t;
    sf : 'a Stream.t;
    r : 'a SizedStream.t;
    sr : 'a Stream.t;
  }

  let c = Const.c

  let empty =
    {
      f = SizedStream.empty;
      sf = Stream.empty;
      r = SizedStream.empty;
      sr = Stream.empty;
    }

  let is_empty { f; r; _ } = SizedStream.is_empty f && SizedStream.is_empty r

  let exec1 = function
    | s -> ( match Stream.tail s with Some s -> s | None -> s)

  let rec rotate_rev s f a =
    let open SizedStream in
    match (peek s, tail s) with
    | None, _ -> reverse f @ a
    | Some x, r ->
        cons x
          (rotate_rev
             (Option.value ~default:empty r)
             (drop c f)
             (reverse (take c f) @ a))

  let rec rotate_drop i r f =
    let open SizedStream in
    if i < c then rotate_rev r (drop i f) empty
    else
      match (peek r, tail r) with
      | Some x, Some r' -> cons x (rotate_drop (i - c) r' (drop c f))
      | _ -> failwith ""

  let queue ({ f; r; _ } as q) =
    let lenf = SizedStream.size f and lenr = SizedStream.size r in
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      let f' = SizedStream.take i f in
      let r = rotate_drop i r f in
      {
        f = f';
        sf = f' |> SizedStream.to_stream;
        r;
        sr = r |> SizedStream.to_stream;
      }
    else if lenr > (c * lenf) + 1 then
      let i = (lenf + lenr) / 2 in
      let j = lenf + lenr - i in
      let f' = rotate_drop j f r in
      let r' = SizedStream.take j r in
      {
        f = f';
        sf = f' |> SizedStream.to_stream;
        r = r';
        sr = r' |> SizedStream.to_stream;
      }
    else q

  let cons x ({ f; sf; sr; _ } as q) =
    { q with f = SizedStream.cons x f; sf = exec1 sf; sr = exec1 sr }

  let head { f; r; _ } =
    match SizedStream.peek f with None -> SizedStream.peek r | s -> s
end
