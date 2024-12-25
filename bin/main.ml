module type Day = sig
  val run : string -> string list
  val example : unit -> string list
end

let day_specs =
  [| (module Aoc2024.Day01 : Day), "Historian Hysteria", [%blob "../inputs/day01.txt"]
   ; (module Aoc2024.Day02), "Red-Nosed Reports", [%blob "../inputs/day02.txt"]
   ; (module Aoc2024.Day03), "Mull It Over", [%blob "../inputs/day03.txt"]
   ; (module Aoc2024.Day04), "Ceres Search", [%blob "../inputs/day04.txt"]
   ; (module Aoc2024.Day05), "Print Queue", [%blob "../inputs/day05.txt"]
   ; (module Aoc2024.Day06), "Guard Gallivant", [%blob "../inputs/day06.txt"]
   ; (module Aoc2024.Day07), "Bridge Repair", [%blob "../inputs/day07.txt"]
   ; (module Aoc2024.Day08), "Resonant Collinearity", [%blob "../inputs/day08.txt"]
   ; (module Aoc2024.Day09), "Disk Fragmenter", [%blob "../inputs/day09.txt"]
   ; (module Aoc2024.Day10), "Hoof It", [%blob "../inputs/day10.txt"]
   ; (module Aoc2024.Day11), "Plutonian Pebbles", [%blob "../inputs/day11.txt"]
   ; (module Aoc2024.Day12), "Garden Groups", [%blob "../inputs/day12.txt"]
   ; (module Aoc2024.Day13), "Claw Contraption", [%blob "../inputs/day13.txt"]
   ; (module Aoc2024.Day14), "Restroom Redoubt", [%blob "../inputs/day14.txt"]
   ; (module Aoc2024.Day15), "Warehouse Woes", [%blob "../inputs/day15.txt"]
   ; (module Aoc2024.Day16), "Reindeer Maze", [%blob "../inputs/day16.txt"]
   ; (module Aoc2024.Day17), "Chronospatial Computer", [%blob "../inputs/day17.txt"]
   ; (module Aoc2024.Day18), "RAM Run", [%blob "../inputs/day18.txt"]
   ; (module Aoc2024.Day19), "Linen Layout", [%blob "../inputs/day19.txt"]
   ; (module Aoc2024.Day20), "Race Condition", [%blob "../inputs/day20.txt"]
   ; (module Aoc2024.Day21), "Keypad Conundrum", [%blob "../inputs/day21.txt"]
   ; (module Aoc2024.Day22), "Monkey Market", [%blob "../inputs/day22.txt"]
   ; (module Aoc2024.Day23), "LAN Party", [%blob "../inputs/day23.txt"]
   ; (module Aoc2024.Day24), "Crossed Wires", [%blob "../inputs/day24.txt"]
   ; (module Aoc2024.Day25), "Code Chronicle", [%blob "../inputs/day25.txt"]
  |]
;;

let time f =
  let t = Sys.time () in
  let res = f () in
  Format.printf "Execution time: %fs\n" (Sys.time () -. t);
  res
;;

let arg_days = ref []
let arg_example = ref false
let usage name = Format.asprintf "Usage: %s [-day <day>]... [-example]" name

let arg_specs =
  [ ( "-day"
    , Arg.Int (fun d -> arg_days := !arg_days @ [ d ])
    , "<day> Run <day>. Can be given multiple times" )
  ; "-example", Arg.Set arg_example, "Run the example instead of the input"
  ]
;;

let main days_to_run run_example =
  let runner (day_num, day_spec) =
    let (module DAY : Day), name, input = day_spec in
    let result =
      if run_example then DAY.example () else if input = "" then [] else DAY.run input
    in
    let result_string =
      if result = [] then "[]" else "[ \"" ^ String.concat "\", \"" result ^ "\" ]"
    in
    Format.printf "Day %d: %s %s\n%!" day_num name result_string
  in
  Format.printf "Advent of Code 2024\n%!";
  time (fun () -> List.iter runner days_to_run)
;;

let () =
  Printexc.record_backtrace true;
  try
    Arg.parse arg_specs (fun _ -> ()) (usage Sys.argv.(0));
    let day_list =
      if List.is_empty !arg_days
      then Array.to_list day_specs |> List.mapi (fun idx day -> idx + 1, day)
      else List.map (fun d -> d, day_specs.(d - 1)) !arg_days
    in
    main day_list !arg_example
  with
  | exn ->
    Format.eprintf "Fatal error: %s\n%!" (Printexc.to_string exn);
    Printexc.print_backtrace stderr
;;
