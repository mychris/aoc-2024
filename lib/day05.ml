module Day05 : sig
  val run : string -> int list
  val example : unit -> int list
end = struct
  module IntMap = Map.Make (Int)

  let parse_input input =
    String.split_on_char '\n' input
    |> List.to_seq
    |> Seq.filter (fun s -> s <> "")
    |> Seq.partition_map (fun s ->
      if String.contains s '|'
      then Left (String.split_on_char '|' s |> List.map int_of_string)
      else Right (String.split_on_char ',' s |> List.map int_of_string))
    |> fun l -> List.of_seq (fst l), List.of_seq (snd l)
  ;;

  let pre_process_orderings os =
    let process map ordering =
      match IntMap.find_opt (List.nth ordering 0) map with
      | Some list -> IntMap.add (List.nth ordering 0) (List.nth ordering 1 :: list) map
      | None -> IntMap.add (List.nth ordering 0) [ List.nth ordering 1 ] map
    in
    List.fold_left process IntMap.empty os |> IntMap.map (fun l -> List.sort compare l)
  ;;

  let check_ordering orderings first second =
    IntMap.find_opt first orderings
    |> Option.map (fun x -> List.find_opt (fun x -> x = second) x)
    |> Option.join
    |> Option.is_some
  ;;

  let rec check_all orderings update =
    match update with
    | head :: tail ->
      if List.filter (fun x -> not (check_ordering orderings head x)) tail
         |> List.is_empty
      then check_all orderings tail
      else false
    | [] -> true
  ;;

  let split_correct_incorrect orderings updates =
    List.fold_left
      (fun acc update ->
        if check_all orderings update
        then update :: fst acc, snd acc
        else fst acc, update :: snd acc)
      ([], [])
      updates
  ;;

  let middle_number update = List.nth update (((1 + List.length update) / 2) - 1)

  let solve orderings updates =
    let sum_middle ll =
      List.fold_left (fun acc update -> acc + middle_number update) 0 ll
    in
    let compare_with_ordering orderings left right =
      if left = right then 0 else if check_ordering orderings left right then -1 else 1
    in
    let orderings = pre_process_orderings orderings in
    let correct, incorrect = split_correct_incorrect orderings updates in
    let corrected =
      List.map
        (fun update -> List.sort (compare_with_ordering orderings) update)
        incorrect
    in
    [ sum_middle correct; sum_middle corrected ]
  ;;

  let run input =
    let orderings, updates = parse_input input in
    solve orderings updates
  ;;

  let example () =
    let orderings =
      [ [ 47; 53 ]
      ; [ 97; 13 ]
      ; [ 97; 61 ]
      ; [ 97; 47 ]
      ; [ 75; 29 ]
      ; [ 61; 13 ]
      ; [ 75; 53 ]
      ; [ 29; 13 ]
      ; [ 97; 29 ]
      ; [ 53; 29 ]
      ; [ 61; 53 ]
      ; [ 97; 53 ]
      ; [ 61; 29 ]
      ; [ 47; 13 ]
      ; [ 75; 47 ]
      ; [ 97; 75 ]
      ; [ 47; 61 ]
      ; [ 75; 61 ]
      ; [ 47; 29 ]
      ; [ 75; 13 ]
      ; [ 53; 13 ]
      ]
    in
    let updates =
      [ [ 75; 47; 61; 53; 29 ]
      ; [ 97; 61; 53; 29; 13 ]
      ; [ 75; 29; 13 ]
      ; [ 75; 97; 47; 61; 53 ]
      ; [ 61; 13; 29 ]
      ; [ 97; 13; 75; 29; 47 ]
      ]
    in
    solve orderings updates
  ;;
end

include Day05
