(** [parse_line line] parses a single input line with two numbers into [first * second]. *)
let parse_line line =
  String.split_on_char ' ' line
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string
;;

(** [parse_input input] parses the whole input into a list of [first * second]. *)
let parse_input input =
  String.split_on_char '\n' input
  |> List.filter (fun x -> String.length (String.trim x) > 0)
  |> List.map parse_line
;;

let is_safe amount = amount >= 1 && amount <= 3
let is_decreasing left right = is_safe (left - right)
let is_increasing left right = is_safe (right - left)
let rem l idx = List.filteri (fun i _ -> idx != i) l

let check report =
  let rec inner f report =
    match report with
    | a :: b :: _ -> if f a b then inner f (List.tl report) else false
    | _ -> true
  in
  inner is_decreasing report || inner is_increasing report
;;

let check_skipping report =
  let rec check_skipping_inner report idx =
    if idx == 0
    then check (List.tl report)
    else check (rem report idx) || check_skipping_inner report (idx - 1)
  in
  check report || check_skipping_inner report (List.length report - 1)
;;

let solve reports checker = List.length (List.filter checker reports)

let run input =
  let reports = parse_input input in
  [ solve reports check; solve reports check_skipping ]
;;

let example () =
  let reports =
    [ [ 7; 6; 4; 2; 1 ]
    ; [ 1; 2; 7; 8; 9 ]
    ; [ 9; 7; 6; 2; 1 ]
    ; [ 1; 3; 2; 4; 5 ]
    ; [ 8; 6; 4; 4; 1 ]
    ; [ 1; 3; 6; 7; 9 ]
    ]
  in
  [ solve reports check; solve reports check_skipping ]
;;
