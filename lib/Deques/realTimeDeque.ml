module Make (Const : sig
  val c : int
end) =
struct
  module SStream = Streams.SizedStream
  module Stream = Streams.Stream

  type 'a t = {
    f : 'a SStream.t;
    sf : 'a Stream.t;
    r : 'a SStream.t;
    sr : 'a Stream.t;
  }

  let c = Const.c

  let empty =
    {
      f = SStream.empty;
      sf = Stream.empty;
      r = SStream.empty;
      sr = Stream.empty;
    }

  let is_empty { f; r; _ } = SStream.is_empty f && SStream.is_empty r

  let exec1 = function
    | s -> ( match Stream.tail s with Some s -> s | None -> s)

  let exec2 s = exec1 (exec1 s)

  let rec rotate_rev s f a =
    let open SStream in
    match (peek s, tail s) with
    | None, _ -> reverse f @ a
    | Some x, r ->
        cons x
          (rotate_rev
             (Option.value ~default:empty r)
             (drop c f)
             (reverse (take c f) @ a))

  let rec rotate_drop i r f =
    let open SStream in
    if i < c then rotate_rev r (drop i f) empty
    else
      match (peek r, tail r) with
      | Some x, Some r' -> cons x (rotate_drop (i - c) r' (drop c f))
      | _ -> failwith ""

  let queue ({ f; r; _ } as q) =
    let lenf = SStream.size f and lenr = SStream.size r in
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      let r = rotate_drop i r f in
      let f = SStream.take i f in
      { f; sf = f |> SStream.to_stream; r; sr = r |> SStream.to_stream }
    else if lenr > (c * lenf) + 1 then
      let j = (lenf + lenr + 1) / 2 in
      let f = rotate_drop j f r in
      let r = SStream.take j r in
      { f; sf = f |> SStream.to_stream; r; sr = r |> SStream.to_stream }
    else q

  let cons x ({ f; sf; sr; _ } as q) =
    queue { q with f = SStream.cons x f; sf = exec1 sf; sr = exec1 sr }

  let head { f; r; _ } =
    match SStream.peek f with None -> SStream.peek r | s -> s

  let tail { f; r; sf; sr } =
    match SStream.tail f with
    | None -> SStream.tail r |> Option.map (Fun.const empty)
    | Some tl -> Some (queue { f = tl; sf = exec2 sf; r; sr = exec2 sr })

  let snoc x ({ r; sf; sr; _ } as q) =
    queue { q with r = SStream.cons x r; sf = exec1 sf; sr = exec1 sr }

  let last { f; r; _ } =
    match SStream.peek r with None -> SStream.peek f | s -> s

  let init { f; r; sf; sr } =
    match SStream.tail r with
    | None -> SStream.tail f |> Option.map (Fun.const empty)
    | Some tl -> Some (queue { f; sf = exec2 sf; r = tl; sr = exec2 sr })
end
