module Day10 : sig
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

  module HeightMap = struct
    type t = int array array

    let of_string input =
      String.split_on_char '\n' input
      |> List.to_seq
      |> Seq.filter (fun s -> s <> "")
      |> Seq.map (fun s ->
        String.to_seq s |> Seq.map (fun c -> Char.code c - Char.code '0') |> Array.of_seq)
      |> Array.of_seq
    ;;

    let get x y (map : t) =
      if x < 0 || y < 0 || x >= Array.length map || y >= Array.length map.(x)
      then None
      else Some map.(x).(y)
    ;;
  end

  let find_trails map =
    let rec find_trails_at x y map cur acc this_trail =
      if cur = 9 && Some 9 = HeightMap.get x y map
      then List.rev ((x, y) :: this_trail) :: acc
      else (
        match HeightMap.get x y map with
        | None -> acc
        | Some h ->
          if h <> cur
          then acc
          else
            find_trails_at (x + 1) y map (cur + 1) [] ((x, y) :: this_trail)
            @ find_trails_at x (y + 1) map (cur + 1) [] ((x, y) :: this_trail)
            @ find_trails_at (x - 1) y map (cur + 1) [] ((x, y) :: this_trail)
            @ find_trails_at x (y - 1) map (cur + 1) [] ((x, y) :: this_trail))
    in
    let rec find_trails' x y map acc =
      match HeightMap.get x y map with
      | Some h when h = 0 ->
        find_trails' x (y + 1) map (find_trails_at x y map 0 [] [] :: acc)
      | Some _ -> find_trails' x (y + 1) map acc
      | None ->
        (match HeightMap.get (x + 1) 0 map with
         | Some _ -> find_trails' (x + 1) 0 map acc
         | None -> acc)
    in
    find_trails' 0 0 map [] |> List.flatten
  ;;

  let score_trailheads trails =
    let rec last = function
      | x :: [] -> x
      | _ :: tl -> last tl
      | [] -> raise (Failure "")
    in
    let trail_heads =
      List.fold_left
        (fun map t -> CoordMap.add_to_list (List.hd t) (last t) map)
        CoordMap.empty
        trails
    in
    let n =
      CoordMap.fold
        (fun _ tops acc -> acc + CoordSet.cardinal (CoordSet.of_list tops))
        trail_heads
        0
    in
    n
  ;;

  let run input =
    let map = HeightMap.of_string input in
    let trails = find_trails map in
    let scores = score_trailheads trails in
    List.map string_of_int [ scores; List.length trails ]
  ;;

  let example () =
    let input =
      ""
      ^ "89010123\n"
      ^ "78121874\n"
      ^ "87430965\n"
      ^ "96549874\n"
      ^ "45678903\n"
      ^ "32019012\n"
      ^ "01329801\n"
      ^ "10456732\n"
    in
    run input
  ;;
end

include Day10
