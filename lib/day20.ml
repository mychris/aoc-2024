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
      |> CoordMap.filter (fun _ c -> c <> '#')
    in
    ( r |> CoordMap.bindings |> List.map fst |> CoordSet.of_list
    , r |> CoordMap.filter (fun _ c -> c = 'S') |> CoordMap.choose |> fst
    , r |> CoordMap.filter (fun _ c -> c = 'E') |> CoordMap.choose |> fst )
  ;;

  let walk (track_set, s_pos, e_pos) =
    let rec aux track_set pos e_pos path n =
      if pos <> e_pos
      then (
        let dirs = [ add pos (-1, 0); add pos (1, 0); add pos (0, -1); add pos (0, 1) ] in
        let n_pos =
          List.find
            (fun p -> CoordSet.mem p track_set && (n = 1 || p <> path.(n - 2)))
            dirs
        in
        path.(n) <- n_pos;
        aux track_set n_pos e_pos path (n + 1))
      else path
    in
    let arr = Array.make (CoordSet.cardinal track_set) s_pos in
    aux track_set s_pos e_pos arr 1
  ;;

  let solve input (min_time_1, max_cheat_dist_1) (min_time_2, max_cheat_dist_2) =
    let path = walk (parse input) in
    let count_1 = ref 0 in
    let count_2 = ref 0 in
    let the_end = Array.length path - 1 in
    for i = 0 to the_end do
      let x1, y1 = path.(i) in
      for j = i + min_time_1 + 1 to the_end do
        let x2, y2 = path.(j) in
        let dist = abs (x2 - x1) + abs (y2 - y1) in
        if dist <= max_cheat_dist_1 then incr count_1;
        if dist <= max_cheat_dist_2 && j - i - dist >= min_time_2 then incr count_2
      done
    done;
    [ !count_1; !count_2 ]
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
