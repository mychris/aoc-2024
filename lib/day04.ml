module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
  ;;
end

module Puzzle = Map.Make (Coord)

let parse input =
  String.split_on_char '\n' input
  |> List.to_seq
  |> Seq.filter (fun s -> s <> "")
  |> Seq.fold_lefti
       (fun puzzle line s ->
         String.to_seq s
         |> Seq.fold_lefti (fun puzzle row c -> Puzzle.add (line, row) c puzzle) puzzle)
       Puzzle.empty
;;

let choord puzzle coords =
  List.to_seq coords
  |> Seq.map (fun coord -> Puzzle.find_opt coord puzzle)
  |> Seq.map (fun c -> Option.value c ~default:' ')
  |> String.of_seq
;;

let check_xmas puzzle coords = Bool.to_int ("XMAS" = choord puzzle coords)

let count_xmas puzzle coord =
  let x, y = coord in
  check_xmas puzzle [ x, y; x, y + 1; x, y + 2; x, y + 3 ]
  + check_xmas puzzle [ x, y; x, y - 1; x, y - 2; x, y - 3 ]
  + check_xmas puzzle [ x, y; x + 1, y; x + 2, y; x + 3, y ]
  + check_xmas puzzle [ x, y; x - 1, y; x - 2, y; x - 3, y ]
  + check_xmas puzzle [ x, y; x + 1, y + 1; x + 2, y + 2; x + 3, y + 3 ]
  + check_xmas puzzle [ x, y; x - 1, y - 1; x - 2, y - 2; x - 3, y - 3 ]
  + check_xmas puzzle [ x, y; x + 1, y - 1; x + 2, y - 2; x + 3, y - 3 ]
  + check_xmas puzzle [ x, y; x - 1, y + 1; x - 2, y + 2; x - 3, y + 3 ]
;;

let count_x_mas puzzle coord =
  if Puzzle.find_opt coord puzzle <> Some 'A'
  then 0
  else (
    let x, y = coord in
    let x_mas =
      choord puzzle [ x, y; x - 1, y - 1; x + 1, y + 1; x - 1, y + 1; x + 1, y - 1 ]
    in
    Bool.to_int (x_mas = "AMSMS" || x_mas = "ASMMS" || x_mas = "AMSSM" || x_mas = "ASMSM"))
;;

let solve puzzle =
  let _ = puzzle in
  [ Puzzle.fold (fun coord _ count -> count + count_xmas puzzle coord) puzzle 0
  ; Puzzle.fold (fun coord _ count -> count + count_x_mas puzzle coord) puzzle 0
  ]
;;

let run input =
  let puzzle = parse input in
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
  let puzzle = parse input in
  solve puzzle
;;
