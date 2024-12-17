module Day04 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  module Puzzle = struct
    type t = char array array

    let of_string s =
      let lines = String.split_on_char '\n' s |> List.filter (fun s -> s <> "") in
      let arr =
        Array.make_matrix (List.length lines) (String.length (List.hd lines)) ' '
      in
      List.iteri (fun x line -> String.iteri (fun y c -> arr.(x).(y) <- c) line) lines;
      arr
    ;;

    let get coord (puzzle : t) =
      let x, y = coord in
      if x < 0 || y < 0 || x >= Array.length puzzle || y >= Array.length puzzle.(x)
      then ' '
      else puzzle.(x).(y)
    ;;

    let choord coords (puzzle : t) =
      List.map (fun coord -> get coord puzzle) coords |> List.to_seq |> String.of_seq
    ;;

    let fold f init (puzzle : t) =
      Array.to_seq puzzle
      |> Seq.fold_lefti
           (fun acc x line ->
             Array.to_seq line |> Seq.fold_lefti (fun acc y c -> f (x, y) c acc) acc)
           init
    ;;
  end

  let check_xmas puzzle coords = Bool.to_int ("XMAS" = Puzzle.choord puzzle coords)

  let count_xmas puzzle coord =
    if Puzzle.get coord puzzle = 'X'
    then (
      let x, y = coord in
      check_xmas [ x, y; x, y + 1; x, y + 2; x, y + 3 ] puzzle
      + check_xmas [ x, y; x, y - 1; x, y - 2; x, y - 3 ] puzzle
      + check_xmas [ x, y; x + 1, y; x + 2, y; x + 3, y ] puzzle
      + check_xmas [ x, y; x - 1, y; x - 2, y; x - 3, y ] puzzle
      + check_xmas [ x, y; x + 1, y + 1; x + 2, y + 2; x + 3, y + 3 ] puzzle
      + check_xmas [ x, y; x - 1, y - 1; x - 2, y - 2; x - 3, y - 3 ] puzzle
      + check_xmas [ x, y; x + 1, y - 1; x + 2, y - 2; x + 3, y - 3 ] puzzle
      + check_xmas [ x, y; x - 1, y + 1; x - 2, y + 2; x - 3, y + 3 ] puzzle)
    else 0
  ;;

  let count_x_mas puzzle coord =
    if Puzzle.get coord puzzle = 'A'
    then (
      let x, y = coord in
      let x_mas =
        Puzzle.choord
          [ x, y; x - 1, y - 1; x + 1, y + 1; x - 1, y + 1; x + 1, y - 1 ]
          puzzle
      in
      Bool.to_int
        (x_mas = "AMSMS" || x_mas = "ASMMS" || x_mas = "AMSSM" || x_mas = "ASMSM"))
    else 0
  ;;

  let solve puzzle =
    List.map
      string_of_int
      [ Puzzle.fold (fun coord _ count -> count + count_xmas puzzle coord) 0 puzzle
      ; Puzzle.fold (fun coord _ count -> count + count_x_mas puzzle coord) 0 puzzle
      ]
  ;;

  let run input =
    let puzzle = Puzzle.of_string input in
    solve puzzle
  ;;

  let example () =
    let input =
      "MMMSXXMASM\n"
      ^ "MSAMXMSMSA\n"
      ^ "AMXSXMAAMM\n"
      ^ "MSAMASMSMX\n"
      ^ "XMASAMXAMM\n"
      ^ "XXAMMXXAMA\n"
      ^ "SMSMSASXSS\n"
      ^ "SAXAMASAAA\n"
      ^ "MAMMMXMMMM\n"
      ^ "MXMXAXMASX\n"
    in
    let puzzle = Puzzle.of_string input in
    solve puzzle
  ;;
end

include Day04
