module C = Aoc2024.Common

let days =
  [| (module Aoc2024.Day01 : C.Day), "Historian Hysteria", [%blob "../inputs/day01.txt"]
   ; (module Aoc2024.Day02), "Red-Nosed Reports", [%blob "../inputs/day02.txt"]
   ; (module Aoc2024.Day03), "Mull It Over", [%blob "../inputs/day03.txt"]
   ; (module Aoc2024.Day04), "Ceres Search", [%blob "../inputs/day04.txt"]
   ; (module Aoc2024.Day05), "Print Queue", [%blob "../inputs/day05.txt"]
   ; (module Aoc2024.Day06), "Guard Gallivant", [%blob "../inputs/day06.txt"]
   ; (module Aoc2024.Day07), "Bridge Repair", [%blob "../inputs/day07.txt"]
   ; (module Aoc2024.Day08), "Resonant Collinearity", [%blob "../inputs/day08.txt"]
  |]
;;

let arg_day = ref 0
let arg_example = ref false
let usage = "Usage: aoc-2024 [-day <day>] [-example]"

let arg_specs =
  [ "-day", Arg.Set_int arg_day, "<day> Run <day>. Use 0 to run all days (default)"
  ; "-example", Arg.Set arg_example, "Run the example instead of the input"
  ]
;;

let main day run_example =
  let runner nth day =
    let (module DAY : C.Day), name, input = day in
    let result = if run_example then DAY.example () else DAY.run input in
    let result_string = "(" ^ String.concat ", " (List.map string_of_int result) ^ ")" in
    Format.printf "Day %d: %s %s\n%!" (nth + 1) name result_string
  in
  try
    Printexc.record_backtrace true;
    Format.printf "Advent of Code 2024\n%!";
    if day >= 0
    then C.time (fun () -> runner day days.(day))
    else C.time (fun () -> Array.iteri runner days);
    Printexc.record_backtrace false
  with
  | exn ->
    Format.eprintf "Fatal error: %s\n%!" (Printexc.to_string exn);
    Printexc.print_backtrace stderr
;;

let () =
  Arg.parse arg_specs (fun _ -> ()) usage;
  main (!arg_day - 1) !arg_example
;;
