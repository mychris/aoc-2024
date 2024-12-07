type field =
  | Obstructed
  | Empty
  | Visited
  | Guard

module Coord = struct
  type t = int * int
end

module Guard = struct
  type t = int * int * int

  let movement = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
  let init x y = x, y, 0

  let rotate guard =
    let x, y, dir = guard in
    x, y, (dir + 1) mod 4
  ;;

  let move guard =
    let x, y, dir = guard in
    let mov_x, mov_y = movement.(dir) in
    x + mov_x, y + mov_y, dir
  ;;

  let position guard =
    let x, y, _ = guard in
    x, y
  ;;
end

module Lab = struct
  type t = int * field array

  let width (lab : t) = fst lab
  let height (lab : t) = Array.length (snd lab) / fst lab

  let look_at (pos : Coord.t) (lab : t) =
    let x, y = pos in
    let width, arr = lab in
    let idx = (x * width) + y in
    if x < 0 || y < 0 || y >= width || idx >= Array.length arr
    then None
    else Some arr.(idx)
  ;;

  let to_seq (lab : t) =
    let width, arr = lab in
    Array.copy arr
    |> Array.to_seqi
    |> Seq.map (fun (idx, e) -> (idx / width, idx mod width), e)
  ;;

  let add (pos : Coord.t) (elm : field) (lab : t) =
    let width, arr = lab in
    let x, y = pos in
    let c = Array.copy arr in
    c.((x * width) + y) <- elm;
    width, c
  ;;

  let pp fmt (lab : t) =
    let width, arr = lab in
    Array.iteri
      (fun idx e ->
        if idx mod width = 0 && idx <> 0 then Format.fprintf fmt "\n";
        match e with
        | Obstructed -> Format.fprintf fmt "#"
        | Empty -> Format.fprintf fmt "."
        | Visited -> Format.fprintf fmt "X"
        | Guard -> Format.fprintf fmt "^")
      arr
  ;;
end

(** Mutable datastructure representing a set of guard *)
module GuardSet = struct
  type t = int array array

  let for_lab lab = Array.make_matrix (Lab.width lab) (Lab.height lab) 0

  let mem (guard : Guard.t) (guard_set : t) =
    let x, y, dir = guard in
    (1 lsl dir) land guard_set.(x).(y) <> 0
  ;;

  let add (guard : Guard.t) (guard_set : t) =
    let x, y, dir = guard in
    guard_set.(x).(y) <- guard_set.(x).(y) lor (1 lsl dir);
    guard_set
  ;;
end

(** Mutable datastructure representing a set of coordinates *)
module CoordSet = struct
  type t = bool array array

  let for_lab lab = Array.make_matrix (Lab.width lab) (Lab.height lab) false
  let mem (coord : Coord.t) (coord_set : t) = coord_set.(fst coord).(snd coord)

  let add (coord : Coord.t) (coord_set : t) =
    coord_set.(fst coord).(snd coord) <- true;
    coord_set
  ;;

  let cardinal (coord_set : t) =
    Array.fold_left
      (fun acc arr -> Array.fold_left (fun acc x -> acc + Bool.to_int x) acc arr)
      0
      coord_set
  ;;
end

let parse_input input =
  let in_list =
    String.split_on_char '\n' input
    |> List.to_seq
    |> Seq.filter (fun s -> s <> "")
    |> Seq.map (fun s -> String.to_seq s)
  in
  let width = Seq.length (Option.get (Seq.find (fun _ -> true) in_list)) in
  let arr =
    Seq.concat in_list
    |> Seq.map (fun c ->
      match c with
      | '#' -> Obstructed
      | '^' -> Guard
      | _ -> Empty)
    |> Array.of_seq
  in
  let guard =
    Seq.fold_lefti
      (fun acc x s ->
        Seq.fold_lefti (fun acc y elm -> if elm = '^' then Guard.init x y else acc) acc s)
      (Guard.init 0 0)
      in_list
  in
  guard, (width, arr)
;;

let test_obstacle lab guard =
  let rec test_obstacle' lab guard visited =
    GuardSet.mem guard visited
    ||
    match Lab.look_at (Guard.position (Guard.move guard)) lab with
    | Some Obstructed -> test_obstacle' lab (Guard.rotate guard) visited
    | Some _ -> test_obstacle' lab (Guard.move guard) (GuardSet.add guard visited)
    | None -> false
  in
  test_obstacle' lab guard (GuardSet.for_lab lab)
;;

let find_obstacles lab guard visited =
  Lab.to_seq lab
  |> Seq.filter (fun (_, v) -> v <> Obstructed)
  |> Seq.map (fun (k, _) -> k)
  |> Seq.filter (fun pos -> pos <> Guard.position guard && CoordSet.mem pos visited)
  |> Seq.fold_left
       (fun acc pos ->
         acc + if test_obstacle (Lab.add pos Obstructed lab) guard then 1 else 0)
       0
;;

let solve lab guard =
  let rec solve' lab guard visited =
    let next_guard = Guard.move guard in
    match Lab.look_at (Guard.position next_guard) lab with
    | Some Obstructed -> solve' lab (Guard.rotate guard) visited
    | Some _ -> solve' lab next_guard (CoordSet.add (Guard.position guard) visited)
    | None -> CoordSet.add (Guard.position guard) visited
  in
  let visited = solve' lab guard (CoordSet.for_lab lab) in
  [ CoordSet.cardinal visited; find_obstacles lab guard visited ]
;;

let run input =
  let guard, lab = parse_input input in
  solve lab guard
;;

let example () =
  let map =
    "....#.....\n"
    ^ ".........#\n"
    ^ "..........\n"
    ^ "..#.......\n"
    ^ ".......#..\n"
    ^ "..........\n"
    ^ ".#..^.....\n"
    ^ "........#.\n"
    ^ "#.........\n"
    ^ "......#...\n"
  in
  let guard, lab = parse_input map in
  solve lab guard
;;
