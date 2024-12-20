module Day20 : sig
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

  module CoordMap = Map.Make (Coord)
  module CoordSet = Set.Make (Coord)

  let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2

  let parse input =
    let r =
      String.split_on_char '\n' input
      |> List.to_seq
      |> Seq.filter (fun s -> s <> "")
      |> Seq.fold_lefti
           (fun track x l ->
             String.to_seq l
             |> Seq.fold_lefti (fun track y c -> CoordMap.add (x, y) c track) track)
           CoordMap.empty
    in
    let track =
      CoordMap.filter (fun _ c -> c <> '#') r
      |> CoordMap.bindings
      |> List.map fst
      |> CoordSet.of_list
    in
    let s_pos = CoordMap.filter (fun _ c -> c = 'S') r |> CoordMap.choose |> fst in
    let e_pos = CoordMap.filter (fun _ c -> c = 'E') r |> CoordMap.choose |> fst in
    track, s_pos, e_pos
  ;;

  let walk (track, s_pos, e_pos) =
    let rec aux track pos e_pos path n =
      if pos = e_pos
      then path
      else (
        let dirs = [ add pos (-1, 0); add pos (1, 0); add pos (0, -1); add pos (0, 1) ] in
        let next_pos =
          List.find (fun p -> CoordSet.mem p track && (n = 1 || p <> path.(n - 2))) dirs
        in
        path.(n) <- next_pos;
        aux track next_pos e_pos path (n + 1))
    in
    let arr = Array.make (CoordSet.cardinal track) s_pos in
    aux track s_pos e_pos arr 1
  ;;

  let find_shortcuts path (min_timesave, max_cheat_dist) =
    let count = ref 0 in
    let the_end = Array.length path - 1 in
    for i = 0 to the_end do
      let x1, y1 = path.(i) in
      for j = i + 1 to the_end do
        let x2, y2 = path.(j) in
        let dist = abs (x2 - x1) + abs (y2 - y1) in
        if dist <= max_cheat_dist && j - (i + dist) >= min_timesave then incr count
      done
    done;
    !count
  ;;

  let solve input r1 r2 =
    let path = walk (parse input) in
    [ find_shortcuts path r1; find_shortcuts path r2 ]
  ;;

  let run input = List.map string_of_int (solve input (100, 2) (100, 20))

  let example () =
    let input =
      ""
      ^ "###############\n"
      ^ "#...#...#.....#\n"
      ^ "#.#.#.#.#.###.#\n"
      ^ "#S#...#.#.#...#\n"
      ^ "#######.#.#.###\n"
      ^ "#######.#.#...#\n"
      ^ "#######.#.###.#\n"
      ^ "###..E#...#...#\n"
      ^ "###.#######.###\n"
      ^ "#...###...#...#\n"
      ^ "#.#####.#.###.#\n"
      ^ "#.#...#.#.#...#\n"
      ^ "#.#.#.#.#.#.###\n"
      ^ "#...#...#...###\n"
      ^ "###############\n"
    in
    List.map string_of_int (solve input (20, 2) (50, 20))
  ;;
end

include Day20
