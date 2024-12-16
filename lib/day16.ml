module Day16 : sig
  val run : string -> int list
  val example : unit -> int list
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
    { pos : int * int
    ; dir : d
    ; points : int
    ; path : (int * int) list
    }

  let move s =
    match s.dir with
    | Right -> { s with pos = fst s.pos, snd s.pos + 1; points = s.points + 1 }
    | Left -> { s with pos = fst s.pos, snd s.pos - 1; points = s.points + 1 }
    | Down -> { s with pos = fst s.pos + 1, snd s.pos; points = s.points + 1 }
    | Up -> { s with pos = fst s.pos - 1, snd s.pos; points = s.points + 1 }
  ;;

  let rotate_cw s =
    match s.dir with
    | Right -> { s with dir = Down; points = s.points + 1000 }
    | Left -> { s with dir = Up; points = s.points + 1000 }
    | Down -> { s with dir = Left; points = s.points + 1000 }
    | Up -> { s with dir = Right; points = s.points + 1000 }
  ;;

  let rotate_ccw s =
    match s.dir with
    | Right -> { s with dir = Up; points = s.points + 1000 }
    | Left -> { s with dir = Down; points = s.points + 1000 }
    | Down -> { s with dir = Right; points = s.points + 1000 }
    | Up -> { s with dir = Left; points = s.points + 1000 }
  ;;

  module Maze = Map.Make (struct
      type t = int * int

      let compare (x1, y1) (x2, y2) =
        match compare x1 x2 with
        | 0 -> compare y1 y2
        | c -> c
      ;;
    end)

  module StateSet = Set.Make (struct
      type t = state

      let compare s1 s2 =
        match compare s1.pos s2.pos with
        | 0 -> compare s1.dir s2.dir
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
    List.to_seq lines
    |> Seq.fold_lefti
         (fun (maze, s_pos, e_pos) x line ->
           String.to_seq line
           |> Seq.fold_lefti
                (fun (maze, s_pos, e_pos) y c ->
                  match c with
                  | '#' -> Maze.add (x, y) Wall maze, s_pos, e_pos
                  | '.' -> Maze.add (x, y) Free maze, s_pos, e_pos
                  | 'E' -> Maze.add (x, y) End maze, s_pos, (x, y)
                  | 'S' -> Maze.add (x, y) Free maze, (x, y), e_pos
                  | _ -> raise (Failure "input"))
                (maze, s_pos, e_pos))
         (Maze.empty, (0, 0), (0, 0))
  ;;

  let solve maze s_pos =
    let cache = ref StateSet.empty in
    let paths = ref IntMap.empty in
    let work = Queue.create () in
    let rec solve' maze =
      if not (Queue.is_empty work)
      then (
        let state = Queue.pop work in
        if StateSet.mem state !cache && (StateSet.find state !cache).points < state.points
        then solve' maze
        else (
          cache := StateSet.remove state !cache;
          match Maze.find state.pos maze with
          | End ->
            paths := IntMap.add_to_list state.points (List.rev state.path) !paths;
            cache := StateSet.add state !cache;
            solve' maze
          | Wall -> solve' maze
          | Free ->
            cache := StateSet.add state !cache;
            let move_state = move state in
            let move_state =
              { move_state with path = move_state.pos :: move_state.path }
            in
            Queue.push move_state work;
            Queue.push (rotate_ccw state) work;
            Queue.push (rotate_cw state) work;
            solve' maze))
    in
    Queue.push { pos = s_pos; dir = Right; points = 0; path = [ s_pos ] } work;
    solve' maze;
    let lowest_points =
      StateSet.fold
        (fun s acc -> min acc s.points)
        (StateSet.filter (fun s -> Maze.find s.pos maze = End) !cache)
        Int.max_int
    in
    let paths = IntMap.find lowest_points !paths in
    let positions = List.flatten paths |> CoordSet.of_list in
    [ lowest_points; CoordSet.cardinal positions ]
  ;;

  let run input =
    let maze, s_pos, _ = parse input in
    solve maze s_pos
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
