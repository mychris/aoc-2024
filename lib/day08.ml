module Day08 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  module Coord = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) =
      match compare x1 x2 with
      | 0 -> compare y1 y2
      | c -> c
    ;;
  end

  module AntennaMap = Map.Make (Char)
  module CoordSet = Set.Make (Coord)

  let parse input =
    let lines =
      String.split_on_char '\n' input |> List.filter (fun s -> String.trim s <> "")
    in
    let width, height = String.length (List.hd lines), List.length lines in
    let map =
      List.to_seq lines
      |> Seq.map (fun s -> String.to_seq s)
      |> Seq.mapi (fun x s -> Seq.mapi (fun y c -> c, x, y) s)
      |> Seq.concat
      |> Seq.filter (fun (c, _, _) -> c <> '.')
      |> Seq.fold_left
           (fun map (c, x, y) -> AntennaMap.add_to_list c (x, y) map)
           AntennaMap.empty
    in
    width, height, map
  ;;

  let map_antenna_pairs f positions =
    List.map
      (fun l -> List.map (fun r -> if l = r then [] else f l r) positions)
      positions
    |> List.flatten
    |> List.flatten
  ;;

  let rec antinodes_for_pair width height times left right =
    let lx, ly = left in
    let rx, ry = right in
    let antinodes =
      [ lx - ((rx - lx) * times), ly - ((ry - ly) * times)
      ; rx - ((lx - rx) * times), ry - ((ly - ry) * times)
      ]
      |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
    in
    if times = 1
    then antinodes
    else antinodes @ antinodes_for_pair width height (times - 1) left right
  ;;

  let antinodes_in_line width height left right =
    antinodes_for_pair width height (width * height) left right @ [ left; right ]
  ;;

  let find_antinodes_for_antenna_pairs width height antenna positions =
    let _ = antenna in
    map_antenna_pairs (antinodes_for_pair width height 1) positions |> CoordSet.of_list
  ;;

  let find_antinodes_resonant_harmonics width height antenna positions =
    let _ = antenna in
    map_antenna_pairs (antinodes_in_line width height) positions |> CoordSet.of_list
  ;;

  let solve (width, height, map) =
    let antinode_searchers =
      [ find_antinodes_for_antenna_pairs; find_antinodes_resonant_harmonics ]
    in
    let antinode_sets =
      List.map
        (fun searcher ->
          AntennaMap.fold
            (fun antenna positions l ->
              CoordSet.union (searcher width height antenna positions) l)
            map
            CoordSet.empty)
        antinode_searchers
    in
    List.map string_of_int (List.map CoordSet.cardinal antinode_sets)
  ;;

  let run input = solve (parse input)

  let example () =
    let input =
      ""
      ^ "............\n"
      ^ "........0...\n"
      ^ ".....0......\n"
      ^ ".......0....\n"
      ^ "....0.......\n"
      ^ "......A.....\n"
      ^ "............\n"
      ^ "............\n"
      ^ "........A...\n"
      ^ ".........A..\n"
      ^ "............\n"
      ^ "............\n"
    in
    solve (parse input)
  ;;
end

include Day08
