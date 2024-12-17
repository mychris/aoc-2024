module Day01 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  (** [parse_line line] parses a single input line with two numbers into [first * second]. *)
  let parse_line line =
    let int_list =
      String.split_on_char ' ' line
      |> List.filter (fun s -> s <> "")
      |> List.map int_of_string
    in
    List.nth int_list 0, List.nth int_list 1
  ;;

  (** [parse_input input] parses the whole input into a list of [first * second]. *)
  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun x -> String.length (String.trim x) > 0)
    |> List.map parse_line
  ;;

  (** [splint_and_sort l] takes an [int * int list], creates two list via [List.split] and sorts both. *)
  let split_and_sort l =
    let left, right = List.split l in
    List.sort Int.compare left, List.sort Int.compare right
  ;;

  module First = struct
    let solve location_id_list =
      let l_sorted, r_sorted = split_and_sort location_id_list in
      List.combine l_sorted r_sorted
      |> List.map (fun (a, b) -> Int.abs (a - b))
      |> List.fold_left ( + ) 0
    ;;
  end

  module Second = struct
    let accum_list l =
      let merge accum elm =
        let num, amount = List.hd accum in
        if elm == num then (elm, amount + 1) :: List.tl accum else (elm, 1) :: accum
      in
      match l with
      | [] -> []
      | head :: tail -> List.fold_left merge [ head, 1 ] tail |> List.rev
    ;;

    let similarity left right =
      let rec calc_similarity result left right =
        match left, right with
        | [], [] -> result
        | _, [] -> result
        | [], _ -> result
        | (left_num, left_amount) :: l_tail, (right_num, right_amount) :: r_tail ->
          if left_num == right_num
          then
            calc_similarity
              (result + (left_num * left_amount * right_amount))
              l_tail
              r_tail
          else if left_num > right_num
          then calc_similarity result left r_tail
          else calc_similarity result l_tail right
      in
      calc_similarity 0 left right
    ;;

    let solve location_id_list =
      let left_sorted, right_sorted = split_and_sort location_id_list in
      similarity (accum_list left_sorted) (accum_list right_sorted)
    ;;
  end

  let run input =
    let location_id_list = parse_input input in
    List.map string_of_int [ First.solve location_id_list; Second.solve location_id_list ]
  ;;

  let example () =
    let location_id_list = [ 3, 4; 4, 3; 2, 5; 1, 3; 3, 9; 3, 3 ] in
    List.map string_of_int [ First.solve location_id_list; Second.solve location_id_list ]
  ;;
end

include Day01
