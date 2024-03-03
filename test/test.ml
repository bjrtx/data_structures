include DataStructures

let print_int_list l =
  List.iter
    (fun x ->
      print_int x;
      print_string " ")
    l;
  print_newline ()

module Tester (Q : Heaps.Heap.PriorityQueue with type elt = int) = struct
  let sorting_time = ref 0. in
  let stdlib_sorting_time = ref 0. in

  for _ = 0 to 10 do
    Random.self_init ();
    let l : int list = List.init 100000 (fun _ -> Random.int 1073741823) in
    let b = Sys.time () in
    let qsort = Q.sort l in
    sorting_time := !sorting_time +. Sys.time () -. b;
    let b = Sys.time () in
    let sorted = List.sort Int.compare l in
    stdlib_sorting_time := !stdlib_sorting_time +. Sys.time () -. b;
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
  done;
  Printf.printf "Sorting time: %fs\n" !sorting_time;
  Printf.printf "Stdlib Sorting time: %fs\n" !stdlib_sorting_time;
  Printf.printf "Ratio: %f\n" (!sorting_time /. !stdlib_sorting_time)

  let () = print_endline "Testing lefist trees"
end

module _ = Tester (Heaps.LeftistTree.Make (Int))

let () = print_endline "Testing skew heaps"

module _ = Tester (Heaps.SkewHeap.Make (Int))

let () = print_endline "Testing pairing heaps"

module _ = Tester (Heaps.PairingHeap.Make (Int))

let () = print_endline "Testing binomial heaps"

module _ = Tester (Heaps.BinomialHeap.Make (Int))

module BUMSTester (Q : SortableCollections.BottomUpMergeSort.Sortable with type elt = int) = struct
  let sorting_time = ref 0. in
  let stdlib_sorting_time = ref 0. in

  for _ = 0 to 10 do
    Random.self_init ();
    let l : int list = List.init 100000 (fun _ -> Random.int 1073741823) in
    let b = Sys.time () in
    let qsort = List.fold_left (fun acc x -> Q.add x acc) Q.empty l |> Q.sort in
    sorting_time := !sorting_time +. Sys.time () -. b;
    let b = Sys.time () in
    let sorted = List.sort Int.compare l in
    stdlib_sorting_time := !stdlib_sorting_time +. Sys.time () -. b;
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
  done;
  Printf.printf "Sorting time: %fs\n" !sorting_time;
  Printf.printf "Stdlib Sorting time: %fs\n" !stdlib_sorting_time;
  Printf.printf "Ratio: %f\n" (!sorting_time /. !stdlib_sorting_time)
end

let () = print_endline "Testing bottom-up merge-sort collections."

module _ = BUMSTester (SortableCollections.BottomUpMergeSort.Make (Int))

let () = print_endline "Testing scheduled bottom-up merge-sort collections."

module _ = BUMSTester (SortableCollections.ScheduleBottomUpMergeSort.Make (Int))
