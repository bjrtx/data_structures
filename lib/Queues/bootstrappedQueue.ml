type 'a t = 'a non_empty option

and 'a non_empty = {
  f : 'a list;
  m : 'a list Lazy.t t;
  lenfm : int;
  r : 'a list;
  lenr : int;
}

let empty = None
let is_empty = Option.is_none
let head = function Some { f = x :: _; _ } -> Some x | _ -> None

let pop = function
  | Some ({ f = _ :: tf; lenfm; _ } as q) ->
      Some (Some { q with f = tf; lenfm = pred lenfm })
  | _ -> None

let rec map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
 fun fn ->
  Option.map (fun ({ f; m; r; _ } as q) ->
      {
        q with
        f = List.map fn f;
        m = map (Lazy.map (List.map fn)) m;
        r = List.map fn r;
      })

let check_f = function
  | { f = []; m = None; _ } -> None
  | { f = []; m; lenfm; r; lenr } ->
      Some
        {
          f = Lazy.force (Option.get @@ head m);
          m = Option.get @@ pop m;
          lenfm;
          r;
          lenr;
        }
  | q -> Some q

let rec queue : 'b. 'b non_empty -> 'b t = function
  | { f; m; lenfm; r; lenr } as q ->
      check_f
        (if lenr <= lenfm then q
         else
           {
             f;
             m = snoc (lazy (List.rev r)) m;
             lenfm = lenfm + lenr;
             r = [];
             lenr = 0;
           })

and snoc : 'c. 'c -> 'c t -> 'c t =
 fun x -> function
  | None -> Some { f = [ x ]; m = None; lenfm = 1; r = []; lenr = 0 }
  | Some ({ r; lenr; _ } as q) -> queue { q with r = x :: r; lenr = succ lenr }
