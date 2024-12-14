module Day07 : sig
  val run : string -> int list
  val example : unit -> int list
end = struct
  let parse_number_list input =
    let rec parse_number_list' input =
      match input with
      | head :: tail when String.trim head <> "" ->
        int_of_string (String.trim head) :: parse_number_list' tail
      | _ :: tail -> parse_number_list' tail
      | [] -> []
    in
    let number_list = String.split_on_char ' ' input in
    parse_number_list' number_list
  ;;

  let parse input =
    String.split_on_char '\n' input
    |> List.to_seq
    |> Seq.filter (fun s -> s <> "")
    |> Seq.map (fun s ->
      let parts = String.split_on_char ':' s in
      let test_value = int_of_string (String.trim (List.nth parts 0)) in
      let numbers = parse_number_list (List.nth parts 1) in
      test_value, numbers)
    |> List.of_seq
  ;;

  let con a b =
    let rec int_pow base exp = if exp = 0 then 1 else base * int_pow base (exp - 1) in
    let rec num_digits x = if x = 0 then 0 else 1 + num_digits (x / 10) in
    (a * int_pow 10 (num_digits b)) + b
  ;;

  let operations_add_mul driver test_value first second tail =
    driver test_value ((first + second) :: tail)
    || driver test_value ((first * second) :: tail)
  ;;

  let operations_add_mul_con driver test_value first second tail =
    driver test_value ((first + second) :: tail)
    || driver test_value ((first * second) :: tail)
    || driver test_value (con first second :: tail)
  ;;

  let solve_equation equation operations =
    let rec solve_equation' test_value numbers =
      match numbers with
      | first :: second :: tail ->
        first <= test_value && operations solve_equation' test_value first second tail
      | head :: [] -> head = test_value
      | [] -> false
    in
    solve_equation' (fst equation) (snd equation)
  ;;

  let solve equations =
    let g1, b1 =
      List.partition (fun x -> solve_equation x operations_add_mul) equations
    in
    let g2 = List.filter (fun x -> solve_equation x operations_add_mul_con) b1 in
    let good1 = List.map fst g1 in
    let good2 = List.map fst g2 in
    [ List.fold_left ( + ) 0 good1; List.fold_left ( + ) 0 (good1 @ good2) ]
  ;;

  let run input =
    let equations = parse input in
    solve equations
  ;;

  let example () =
    let equations =
      [ 190, [ 10; 19 ]
      ; 3267, [ 81; 40; 27 ]
      ; 83, [ 17; 5 ]
      ; 156, [ 15; 6 ]
      ; 7290, [ 6; 8; 6; 15 ]
      ; 161011, [ 16; 10; 13 ]
      ; 192, [ 17; 8; 14 ]
      ; 21037, [ 9; 7; 18; 13 ]
      ; 292, [ 11; 6; 16; 20 ]
      ]
    in
    solve equations
  ;;
end

include Day07
