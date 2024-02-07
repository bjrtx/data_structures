include DataStructures

module Tester (Q : Heap.PriorityQueue with type elt = int) = struct
  let print_int_list l =
    List.iter
      (fun x ->
        print_int x;
        print_string " ")
      l;
    print_newline ()

  let () = Random.self_init ()
  let l : int list = List.init 50000 (fun _ -> Random.int 100000)
  let b = Sys.time ()
  let qsort : int list = Q.sort l
  let first = Sys.time () -. b
  let () = Printf.printf "Sorting time: %fs\n" first
  let b = Sys.time ()
  let sorted = List.sort Int.compare l
  let second = Sys.time () -. b
  let () = Printf.printf "Stdlib Sorting time: %fs\n" second
  let () = Printf.printf "Ratio: %f\n" (first /. second)

  let () =
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
end

module _ = Tester (Heap.LeftistTree (Int))
module _ = Tester (Heap.SkewHeap (Int))
module _ = Tester (Heap.PairingHeap (Int))
module _ = Tester (Heap.BinomialHeap (Int))

module BUMSTester (Q : BottomUpMergeSort.Sortable with type elt = int) = struct
  let print_int_list l =
    List.iter
      (fun x ->
        print_int x;
        print_string " ")
      l;
    print_newline ()

  let () = Random.self_init ()
  let l : int list = List.init 50000 (fun _ -> Random.int 100000)
  let b = Sys.time ()

  let qsort : int list =
    List.fold_left (fun acc x -> Q.add x acc) Q.empty l |> Q.sort

  let first = Sys.time () -. b
  let () = Printf.printf "Sorting time: %fs\n" first
  let b = Sys.time ()
  let sorted = List.sort Int.compare l
  let second = Sys.time () -. b
  let () = Printf.printf "Stdlib Sorting time: %fs\n" second
  let () = Printf.printf "Ratio: %f\n" (first /. second)

  let () =
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
end

module _ = BUMSTester (BottomUpMergeSort.Make (Int))
module _ = BUMSTester (ScheduleBottomUpMergeSort.Make (Int))
