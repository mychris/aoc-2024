module Day25 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  let parse input =
    let count_dots arr =
      Array.map
        (fun a -> Array.fold_left (fun cnt c -> cnt + Bool.to_int (c = '.')) 0 a)
        arr
    in
    let transpose arr =
      let rows = Array.length arr in
      let cols = Array.length arr.(0) in
      let transposed =
        Array.init cols (fun i -> Array.init rows (fun j -> arr.(j).(i)))
      in
      transposed
    in
    let parse_key key =
      let key = transpose key in
      count_dots key |> Array.map (fun i -> 6 - i)
    in
    let parse_lock key =
      let lock =
        transpose key |> Array.map (fun l -> Array.to_list l |> List.rev |> Array.of_list)
      in
      count_dots lock |> Array.map (fun i -> 6 - i)
    in
    let rec parse' lines acc_keys acc_locks =
      if Option.is_none @@ Seq.uncons lines
      then acc_keys, acc_locks
      else (
        let this = Seq.take_while (fun s -> s <> "") lines in
        if Seq.length this = 0
        then parse' (Seq.drop_while (fun s -> s = "") lines) acc_keys acc_locks
        else (
          let arr =
            Array.of_seq (Seq.map (fun s -> Array.of_seq @@ String.to_seq s) this)
          in
          let rest = Seq.drop_while (fun s -> s <> "") lines in
          if fst @@ Option.get @@ Seq.uncons this = "#####"
          then parse' rest acc_keys (parse_lock arr :: acc_locks)
          else parse' rest (parse_key arr :: acc_keys) acc_locks))
    in
    let lines = String.split_on_char '\n' input |> List.to_seq in
    let keys, locks = parse' lines [] [] in
    List.rev keys, List.rev locks
  ;;

  let solve keys locks =
    let rec solve_key acc key locks =
      match locks with
      | [] -> acc
      | lock :: tl ->
        let z = Seq.zip (Array.to_seq key) (Array.to_seq lock) in
        if Seq.for_all (fun (k, l) -> k + l <= 5) z
        then solve_key (succ acc) key tl
        else solve_key acc key tl
    in
    List.fold_left (fun acc key -> solve_key acc key locks) 0 keys
  ;;

  let run input =
    let keys, locks = parse input in
    let x = solve keys locks in
    [ string_of_int x ]
  ;;

  let example () =
    let input =
      ""
      ^ "#####\n"
      ^ ".####\n"
      ^ ".####\n"
      ^ ".####\n"
      ^ ".#.#.\n"
      ^ ".#...\n"
      ^ ".....\n"
      ^ "\n"
      ^ "#####\n"
      ^ "##.##\n"
      ^ ".#.##\n"
      ^ "...##\n"
      ^ "...#.\n"
      ^ "...#.\n"
      ^ ".....\n"
      ^ "\n"
      ^ ".....\n"
      ^ "#....\n"
      ^ "#....\n"
      ^ "#...#\n"
      ^ "#.#.#\n"
      ^ "#.###\n"
      ^ "#####\n"
      ^ "\n"
      ^ ".....\n"
      ^ ".....\n"
      ^ "#.#..\n"
      ^ "###..\n"
      ^ "###.#\n"
      ^ "###.#\n"
      ^ "#####\n"
      ^ "\n"
      ^ ".....\n"
      ^ ".....\n"
      ^ ".....\n"
      ^ "#....\n"
      ^ "#.#..\n"
      ^ "#.#.#\n"
      ^ "#####\n"
    in
    run input
  ;;
end

include Day25
