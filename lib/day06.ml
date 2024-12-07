(***********************************************
 * Too many micro opimizations
 * Lots of mutable datastructures etc to
 * increase performance
 ***********************************************)
type field =
  | Obstructed
  | Empty
  | Visited
  | Guard

module Coord = struct
  type t = int * int
end

module Lab = struct
  type t = int * field array

  let width (lab : t) = fst lab
  let height (lab : t) = Array.length (snd lab) / fst lab

  let look_at (pos : Coord.t) (lab : t) =
    let x, y = pos in
    let width, arr = lab in
    if x lor y >= 0 && y < width
    then (
      let idx = (x * width) + y in
      if idx < Array.length arr then Some (Array.unsafe_get arr idx) else None)
    else None
  ;;

  (** Calculate the distance to the next obstacle from [pos] with [incr] *)
  let distance_to_obstacle (pos : Coord.t) (incr : Coord.t) (lab : t) =
    let x, y = pos in
    let incr_x, incr_y = incr in
    let width, height = width lab, height lab in
    let _, arr = lab in
    let rec distance_to_obstacle' x y distance =
      if x >= 0 && y >= 0 && x < height && y < width
      then
        if Array.unsafe_get arr ((x * width) + y) <> Obstructed
        then distance_to_obstacle' (x + incr_x) (y + incr_y) (distance + 1)
        else Some distance
      else None
    in
    distance_to_obstacle' x y 0
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
    arr.((x * width) + y) <- elm
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

module Guard = struct
  type t =
    { x : int
    ; y : int
    ; dir : int
    }

  let movement = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
  let init x y = { x; y; dir = 0 }
  let rotate (guard : t) = { guard with dir = (guard.dir + 1) land 3 }

  let move (guard : t) =
    let mov_x, mov_y = movement.(guard.dir) in
    { guard with x = guard.x + mov_x; y = guard.y + mov_y }
  ;;

  (** snap the guard right before the next obstacle, or to the border *)
  let snap lab (guard : t) =
    let incr_x, incr_y = movement.(guard.dir) in
    Lab.distance_to_obstacle (guard.x, guard.y) (incr_x, incr_y) lab
    |> Option.map (fun distance ->
      { guard with
        x = guard.x + (incr_x * (distance - 1))
      ; y = guard.y + (incr_y * (distance - 1))
      })
  ;;

  let position (guard : t) = guard.x, guard.y
end

(** Mutable datastructure representing a set of guard *)
module GuardSet = struct
  type t = int array array

  let for_lab lab = Array.make_matrix (Lab.width lab) (Lab.height lab) 0

  let mem (guard : Guard.t) (guard_set : t) =
    (1 lsl guard.dir) land guard_set.(guard.x).(guard.y) <> 0
  ;;

  let add (guard : Guard.t) (guard_set : t) =
    guard_set.(guard.x).(guard.y) <- guard_set.(guard.x).(guard.y) lor (1 lsl guard.dir);
    guard_set
  ;;

  let reset (guard_set : t) =
    Array.iteri (fun _ row -> Array.iteri (fun j _ -> row.(j) <- 0) row) guard_set
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
  let lines = String.split_on_char '\n' input in
  let width = String.length (List.hd lines) in
  let in_list =
    List.filter_map (fun s -> if s = "" then None else Some (String.to_seq s)) lines
    |> List.to_seq
  in
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

let test_obstacle lab guard visited =
  let rec test_obstacle' lab guard visited =
    let opt_guard = Guard.snap lab guard in
    match opt_guard with
    | Some guard ->
      GuardSet.mem guard visited
      || test_obstacle' lab (Guard.rotate guard) (GuardSet.add guard visited)
    | None -> false
  in
  GuardSet.reset visited;
  test_obstacle' lab guard visited
;;

let find_obstacles lab guard visited =
  let guard_set = GuardSet.for_lab lab in
  Lab.to_seq lab
  |> Seq.fold_left
       (fun acc (k, v) ->
         if v <> Obstructed && k <> Guard.position guard && CoordSet.mem k visited
         then (
           Lab.add k Obstructed lab;
           let result = acc + if test_obstacle lab guard guard_set then 1 else 0 in
           Lab.add k Empty lab;
           result)
         else acc)
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
