let inputs = [ [%blob "../inputs/day01.txt"]; [%blob "../inputs/day02.txt"] ]

let run_day nth day =
  let func, name = day in
  let result = func (Option.value (List.nth_opt inputs nth) ~default:"") in
  let result_string = "(" ^ String.concat ", " (List.map string_of_int result) ^ ")" in
  Printf.printf "Day %d: %s %s" (nth + 1) name result_string;
  print_endline ""
;;

let main () =
  let days =
    [ Aoc2024.Day01.run, "Historian Hysteria"; Aoc2024.Day02.run, "Red-Nosed Reports" ]
  in
  print_endline "Advent of Code 2024";
  List.iteri run_day days
;;

let () = Aoc2024.Common.time main
