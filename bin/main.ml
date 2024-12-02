let days =
  [| Aoc2024.Day01.run, "Historian Hysteria", [%blob "../inputs/day01.txt"]
   ; Aoc2024.Day02.run, "Red-Nosed Reports", [%blob "../inputs/day02.txt"]
   ; Aoc2024.Day03.run, "Mull It Over", [%blob "../inputs/day03.txt"]
  |]
;;

let run_day nth day =
  let func, name, input = day in
  let result = func input in
  let result_string = "(" ^ String.concat ", " (List.map string_of_int result) ^ ")" in
  Format.printf "Day %d: %s %s\n" (nth + 1) name result_string
;;

let main () =
  print_endline "Advent of Code 2024";
  Aoc2024.Common.time (fun () -> Array.iteri run_day days)
;;

let () = main ()
