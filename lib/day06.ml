type field =
  | Obstructed
  | Empty
  | Visited
  | Guard

module Coordinate = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) = ((x2 lsl 20) lor y2) - ((x1 lsl 20) lor y1)
end

module Guard = struct
  type t = int * int * int

  let movement = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
  let make x y = x, y, 0

  let compare (x1, y1, dir1) (x2, y2, dir2) =
    (* compare is really crucial and only needed for the sets, so this should be fine *)
    (x2 - x1) lor (y2 - y1) lor (dir2 - dir1)
  ;;

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

module CoordSet = Set.Make (Coordinate)
module GuardSet = Set.Make (Guard)

module Lab = struct
  type t = int * field array

  let look_at pos lab =
    let x, y = pos in
    let width, arr = lab in
    let idx = (x * width) + y in
    if x < 0 || y < 0 || y >= width || idx >= Array.length arr
    then None
    else Some arr.(idx)
  ;;

  let to_seq lab =
    let width, arr = lab in
    Array.copy arr
    |> Array.to_seqi
    |> Seq.map (fun (idx, e) -> (idx / width, idx mod width), e)
  ;;

  let add pos elm lab =
    let width, arr = lab in
    let x, y = pos in
    let c = Array.copy arr in
    c.((x * width) + y) <- elm;
    width, c
  ;;

  let pp fmt lab =
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
        Seq.fold_lefti (fun acc y elm -> if elm = '^' then Guard.make x y else acc) acc s)
      (Guard.make 0 0)
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
  test_obstacle' lab guard GuardSet.empty
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
  let visited = solve' lab guard CoordSet.empty in
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
