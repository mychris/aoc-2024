module Day15 : sig
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

  type direction =
    | Left
    | Right
    | Up
    | Down

  type box =
    | Box
    | BoxLeft
    | BoxRight

  let move_dir (x, y) = function
    | Left -> x, y - 1
    | Right -> x, y + 1
    | Up -> x - 1, y
    | Down -> x + 1, y
  ;;

  let parse input =
    let lines = String.split_on_char '\n' input in
    let boxes, walls, robot =
      List.filter (fun s -> s <> "" && String.get s 0 = '#') lines
      |> List.to_seq
      |> Seq.fold_lefti
           (fun acc x line ->
             String.to_seq line
             |> Seq.fold_lefti
                  (fun (boxes, walls, robot) y -> function
                    | '#' -> boxes, CoordSet.add (x, y) walls, robot
                    | '@' -> boxes, walls, (x, y)
                    | 'O' -> CoordMap.add (x, y) Box boxes, walls, robot
                    | _ -> boxes, walls, robot)
                  acc)
           (CoordMap.empty, CoordSet.empty, (0, 0))
    in
    let movement =
      List.filter (fun s -> s <> "" && String.get s 0 <> '#') lines
      |> List.concat_map (fun s -> String.to_seq s |> List.of_seq)
      |> List.map (function
        | '^' -> Up
        | '<' -> Left
        | '>' -> Right
        | 'v' -> Down
        | _ -> raise (Failure "input movement"))
    in
    boxes, walls, robot, movement
  ;;

  let resize boxes walls robot =
    let boxes =
      CoordMap.to_list boxes
      |> List.concat_map (fun ((x, y), elm) ->
        match elm with
        | Box -> [ (x, y * 2), BoxLeft; (x, (y * 2) + 1), BoxRight ]
        | _ -> raise (Failure "resize"))
      |> CoordMap.of_list
    in
    let walls =
      CoordSet.to_list walls
      |> List.concat_map (fun (x, y) -> [ x, y * 2; x, (y * 2) + 1 ])
      |> CoordSet.of_list
    in
    boxes, walls, (fst robot, snd robot * 2)
  ;;

  let rec try_move boxes walls robot dir =
    let next_robot = move_dir robot dir in
    if CoordSet.mem next_robot walls
    then false, []
    else if not (CoordMap.mem next_robot boxes)
    then true, []
    else (
      match dir with
      | Left | Right ->
        let should_move, box_moves = try_move boxes walls next_robot dir in
        should_move, (next_robot, move_dir next_robot dir) :: box_moves
      | Up | Down ->
        (match CoordMap.find next_robot boxes with
         | Box ->
           let should_move, box_moves = try_move boxes walls next_robot dir in
           should_move, (next_robot, move_dir next_robot dir) :: box_moves
         | (BoxLeft | BoxRight) as box ->
           let left_box =
             if box = BoxRight then move_dir next_robot Left else next_robot
           in
           let right_box =
             if box = BoxRight then next_robot else move_dir next_robot Right
           in
           let b1, m1 = try_move boxes walls left_box dir in
           let b2, m2 = try_move boxes walls right_box dir in
           ( b1 && b2
           , [ left_box, move_dir left_box dir; right_box, move_dir right_box dir ]
             @ m1
             @ m2 )))
  ;;

  let rec walk boxes walls robot = function
    | [] -> boxes, robot
    | dir :: tail ->
      (match try_move boxes walls robot dir with
       | false, _ -> walk boxes walls robot tail
       | true, box_moves ->
         let robot = move_dir robot dir in
         let new_boxes =
           List.map fst box_moves |> List.fold_left (Fun.flip CoordMap.remove) boxes
         in
         let new_boxes =
           List.fold_left
             (fun acc (fpos, tpos) -> CoordMap.add tpos (CoordMap.find fpos boxes) acc)
             new_boxes
             box_moves
         in
         walk new_boxes walls robot tail)
  ;;

  let checksum boxes walls =
    let _ = walls in
    CoordMap.fold
      (fun (x, y) elm acc -> if elm = BoxRight then acc else acc + (x * 100) + y)
      boxes
      0
  ;;

  let run input =
    let boxes, walls, robot, movement = parse input in
    let result, _ = walk boxes walls robot movement in
    let r1 = checksum result walls in
    let boxes, walls, robot = resize boxes walls robot in
    let result, _ = walk boxes walls robot movement in
    let r2 = checksum result walls in
    List.map string_of_int [ r1; r2 ]
  ;;

  let example () =
    let input1 =
      ""
      ^ "########\n"
      ^ "#..O.O.#\n"
      ^ "##@.O..#\n"
      ^ "#...O..#\n"
      ^ "#.#.O..#\n"
      ^ "#...O..#\n"
      ^ "#......#\n"
      ^ "########\n"
      ^ "\n"
      ^ "<^^>>>vv<v>>v<<\n"
    in
    let input2 =
      ""
      ^ "#######\n"
      ^ "#...#.#\n"
      ^ "#.....#\n"
      ^ "#..OO@#\n"
      ^ "#..O..#\n"
      ^ "#.....#\n"
      ^ "#######\n"
      ^ "\n"
      ^ "<vv<<^^<<^^"
    in
    let input3 =
      ""
      ^ "##########\n"
      ^ "#..O..O.O#\n"
      ^ "#......O.#\n"
      ^ "#.OO..O.O#\n"
      ^ "#..O@..O.#\n"
      ^ "#O#..O...#\n"
      ^ "#O..O..O.#\n"
      ^ "#.OO.O.OO#\n"
      ^ "#....O...#\n"
      ^ "##########\n"
      ^ "\n"
      ^ "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n"
      ^ "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n"
      ^ "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n"
      ^ "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n"
      ^ "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n"
      ^ "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n"
      ^ ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n"
      ^ "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n"
      ^ "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n"
      ^ "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n"
    in
    let _ = input1, input2, input3 in
    run input3
  ;;
end

include Day15
