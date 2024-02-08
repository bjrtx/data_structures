type 'a t = Empty | Q of 'a non_empty

and 'a non_empty = {
  f : 'a list;
  m : 'a list Lazy.t t;
  lenfm : int;
  r : 'a list;
  lenr : int;
}

let empty = Empty
let is_empty = function Empty -> true | _ -> false
let peek = function Q { f = x :: _; _ } -> Some x | _ -> None

let pop = function
  | Q ({ f = _ :: tf; lenfm; _ } as q) ->
      Some (Q { q with f = tf; lenfm = pred lenfm })
  | _ -> None

let rec map: 'a 'b. ('a -> 'b) -> 'a t -> 'b t = fun fn -> function
    | Empty -> Empty
    | Q ({f; m; r; _} as q) -> Q {q with f = List.map fn f; m = map (Lazy.map (List.map fn)) m; r = List.map fn r}

let check_f = function
  | { f = []; m = Empty; _ } -> Empty
  | { f = []; m; lenfm; r; lenr } ->
      Q
        {
          f = Lazy.force (Option.get @@ peek m);
          m = Option.get @@ pop m;
          lenfm;
          r;
          lenr;
        }
  | q -> Q q

let rec queue : 'b. 'b non_empty -> 'b t = function
  | { f; m; lenfm; r; lenr } as q ->
      if lenr <= lenfm then check_f q
      else
        check_f
          {
            f;
            m = push (lazy (List.rev r)) m;
            lenfm = lenfm + lenr;
            r = [];
            lenr = 0;
          }

and push : 'c. 'c -> 'c t -> 'c t =
 fun x -> function
  | Empty -> Q { f = [ x ]; m = Empty; lenfm = 1; r = []; lenr = 0 }
  | Q ({ r; lenr; _ } as q) -> queue { q with r = x :: r; lenr = succ lenr }
