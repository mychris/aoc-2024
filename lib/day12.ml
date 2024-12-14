module Day12 : sig
  val run : string -> int list
  val example : unit -> int list
end = struct
  module Direction = struct
    type t =
      | Right
      | Down
      | Left
      | Up

    let rotate = function
      | Right -> Down
      | Down -> Left
      | Left -> Up
      | Up -> Right
    ;;
  end

  module CoordSet = Set.Make (struct
      type t = int * int

      let compare (x1, y1) (x2, y2) =
        match compare x1 x2 with
        | 0 -> compare y1 y2
        | c -> c
      ;;
    end)

  module Garden = struct
    type t = char array array

    let of_string input =
      String.split_on_char '\n' input
      |> List.filter (fun s -> s <> "")
      |> List.map (fun s -> String.to_seq s |> Array.of_seq)
      |> Array.of_list
    ;;

    let get (x, y) garden =
      if x < 0 || y < 0 || x >= Array.length garden || y >= Array.length garden.(x)
      then None
      else Some garden.(x).(y)
    ;;

    let copy garden = Array.map Array.copy garden
  end

  let find_regions garden =
    let rec explore_region x y r garden =
      match Garden.get (x, y) garden with
      | Some ir when r = ir ->
        garden.(x).(y) <- '.';
        [ x, y ]
        @ explore_region (x + 1) y r garden
        @ explore_region x (y + 1) r garden
        @ explore_region (x - 1) y r garden
        @ explore_region x (y - 1) r garden
      | _ -> []
    in
    let rec aux x y regions garden =
      match Garden.get (x, y) garden with
      | Some r when r <> '.' ->
        let region = explore_region x y r garden in
        aux x (y + 1) (region :: regions) garden
      | Some _ -> aux x (y + 1) regions garden
      | None ->
        (match Garden.get (x + 1, 0) garden with
         | Some _ -> aux (x + 1) 0 regions garden
         | None -> regions)
    in
    aux 0 0 [] (Garden.copy garden)
  ;;

  let movef (x, y) = function
    | Direction.Right -> x, y + 1
    | Direction.Down -> x + 1, y
    | Direction.Left -> x, y - 1
    | Direction.Up -> x - 1, y
  ;;

  let moveb (x, y) = function
    | Direction.Right -> x, y - 1
    | Direction.Down -> x - 1, y
    | Direction.Left -> x, y + 1
    | Direction.Up -> x + 1, y
  ;;

  let explore_region (garden : Garden.t) (region : (int * int) list) =
    let visited = ref CoordSet.empty in
    let rec aux pos r dir garden =
      if Some r <> Garden.get pos garden
      then
        if Some r = Garden.get (movef pos (Direction.rotate dir)) garden
           || Some r <> Garden.get (movef (moveb pos dir) (Direction.rotate dir)) garden
        then 0, 1, 1
        else 0, 1, 0
      else if CoordSet.mem pos !visited
      then 0, 0, 0
      else (
        let dirs = [ Direction.Right; Direction.Left; Direction.Up; Direction.Down ] in
        visited := CoordSet.add pos !visited;
        List.map (fun d -> aux (movef pos d) r d garden) dirs
        |> List.fold_left
             (fun (acc_a, acc_p, acc_s) (a, p, s) -> acc_a + a, acc_p + p, acc_s + s)
             (1, 0, 0))
    in
    let pos = List.hd region in
    aux pos (Option.get (Garden.get pos garden)) Right garden
  ;;

  let run input =
    let garden = Garden.of_string input in
    let regions = find_regions garden in
    let ans =
      List.map (explore_region garden) regions
      |> List.fold_left (fun (a1, a2) (a, p, s) -> a1 + (a * p), a2 + (a * s)) (0, 0)
    in
    [ fst ans; snd ans ]
  ;;

  let example () =
    let input =
      ""
      ^ "RRRRIICCFF\n"
      ^ "RRRRIICCCF\n"
      ^ "VVRRRCCFFF\n"
      ^ "VVRCCCJFFF\n"
      ^ "VVVVCJJCFE\n"
      ^ "VVIVCCJJEE\n"
      ^ "VVIIICJJEE\n"
      ^ "MIIIIIJJEE\n"
      ^ "MIIISIJEEE\n"
      ^ "MMMISSJEEE\n"
    in
    run input
  ;;
end

include Day12
