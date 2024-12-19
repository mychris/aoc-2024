module Day16 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  type e =
    | Wall
    | Free
    | End

  type d =
    | Right
    | Left
    | Up
    | Down

  type state =
    { x : int
    ; y : int
    ; dir : d
    }

  let move s =
    match s.dir with
    | Right -> { s with y = s.y + 1 }
    | Left -> { s with y = s.y - 1 }
    | Down -> { s with x = s.x + 1 }
    | Up -> { s with x = s.x - 1 }
  ;;

  let rotate_cw s =
    match s.dir with
    | Right -> { s with dir = Down }
    | Left -> { s with dir = Up }
    | Down -> { s with dir = Left }
    | Up -> { s with dir = Right }
  ;;

  let rotate_ccw s =
    match s.dir with
    | Right -> { s with dir = Up }
    | Left -> { s with dir = Down }
    | Down -> { s with dir = Right }
    | Up -> { s with dir = Left }
  ;;

  module Maze = struct
    let find state maze = maze.(state.x).(state.y)
  end

  module StateMap = Map.Make (struct
      type t = state

      let compare s1 s2 =
        match compare s1.x s2.x with
        | 0 ->
          (match compare s1.y s2.y with
           | 0 -> compare s1.dir s2.dir
           | c -> c)
        | c -> c
      ;;
    end)

  module IntMap = Map.Make (Int)

  module CoordSet = Set.Make (struct
      type t = int * int

      let compare (x1, y1) (x2, y2) =
        match compare x1 x2 with
        | 0 -> compare y1 y2
        | c -> c
      ;;
    end)

  let parse input =
    let lines = String.split_on_char '\n' input in
    let maze =
      Array.make_matrix (List.length lines) (String.length (List.hd lines)) Free
    in
    List.to_seq lines
    |> Seq.fold_lefti
         (fun (maze, s_pos, e_pos) x line ->
           String.to_seq line
           |> Seq.fold_lefti
                (fun (maze, s_pos, e_pos) y c ->
                  match c with
                  | '#' ->
                    maze.(x).(y) <- Wall;
                    maze, s_pos, e_pos
                  | '.' ->
                    maze.(x).(y) <- Free;
                    maze, s_pos, e_pos
                  | 'E' ->
                    maze.(x).(y) <- End;
                    maze, s_pos, (x, y)
                  | 'S' ->
                    maze.(x).(y) <- Free;
                    maze, (x, y), e_pos
                  | _ -> failwith "input")
                (maze, s_pos, e_pos))
         (maze, (0, 0), (0, 0))
  ;;

  let solve maze s_pos =
    let cache = ref StateMap.empty in
    let paths = ref IntMap.empty in
    let work = Queue.create () in
    let rec solve' () =
      match Queue.take_opt work with
      | None -> ()
      | Some (state, points, path) ->
        (match Maze.find state maze with
         | Wall -> solve' ()
         | End ->
           if StateMap.find_opt state !cache |> Option.value ~default:Int.max_int < points
           then solve' ()
           else (
             cache := StateMap.add state points !cache;
             paths := IntMap.add_to_list points (state :: path) !paths;
             solve' ())
         | Free ->
           if StateMap.find_opt state !cache |> Option.value ~default:Int.max_int < points
           then solve' ()
           else (
             cache := StateMap.add state points !cache;
             let move_state = move state in
             Queue.add (move_state, points + 1, state :: path) work;
             Queue.add (rotate_ccw state, points + 1000, path) work;
             Queue.add (rotate_cw state, points + 1000, path) work;
             solve' ()))
    in
    Queue.push ({ x = fst s_pos; y = snd s_pos; dir = Right }, 0, []) work;
    solve' ();
    let lowest_points =
      StateMap.to_seq !cache
      |> Seq.filter_map (fun (state, p) ->
        if Maze.find state maze = End then Some p else None)
      |> Seq.fold_left min Int.max_int
    in
    let paths = IntMap.find lowest_points !paths in
    let positions =
      List.flatten paths |> List.map (fun s -> s.x, s.y) |> CoordSet.of_list
    in
    [ lowest_points; CoordSet.cardinal positions ]
  ;;

  let run input =
    let maze, s_pos, _ = parse input in
    List.map string_of_int (solve maze s_pos)
  ;;

  let example () =
    let input =
      ""
      ^ "#################\n"
      ^ "#...#...#...#..E#\n"
      ^ "#.#.#.#.#.#.#.#.#\n"
      ^ "#.#.#.#...#...#.#\n"
      ^ "#.#.#.#.###.#.#.#\n"
      ^ "#...#.#.#.....#.#\n"
      ^ "#.#.#.#.#.#####.#\n"
      ^ "#.#...#.#.#.....#\n"
      ^ "#.#.#####.#.###.#\n"
      ^ "#.#.#.......#...#\n"
      ^ "#.#.###.#####.###\n"
      ^ "#.#.#...#.....#.#\n"
      ^ "#.#.#.#####.###.#\n"
      ^ "#.#.#.........#.#\n"
      ^ "#.#.#.#########.#\n"
      ^ "#S#.............#\n"
      ^ "#################\n"
    in
    run input
  ;;
end

include Day16
