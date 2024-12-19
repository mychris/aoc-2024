module Day18 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  type space =
    | Safe
    | Corrupted

  module Coord = struct
    type t =
      { x : int
      ; y : int
      }

    let make x y = { x; y }
    let left c = { c with x = c.x - 1 }
    let right c = { c with x = c.x + 1 }
    let up c = { c with y = c.y - 1 }
    let down c = { c with y = c.y + 1 }

    let compare c1 c2 =
      match compare c1.x c2.x with
      | 0 -> compare c1.y c2.y
      | c -> c
    ;;

    let pp fmt c = Format.fprintf fmt "%d,%d" c.x c.y
  end

  module CoordMap = Map.Make (Coord)

  let fill_map map size thing =
    let rec fill (c : Coord.t) map size thing =
      if c.x > size
      then fill (Coord.make 0 (c.y + 1)) map size thing
      else if c.y > size
      then map
      else fill (Coord.make (c.x + 1) c.y) (CoordMap.add c thing map) size thing
    in
    fill (Coord.make 0 0) map size thing
  ;;

  let fill_corrupted seq len ram =
    Seq.take len seq
    |> Seq.fold_left (fun map space -> CoordMap.add space Corrupted map) ram
  ;;

  let parse_bytes input =
    String.split_on_char '\n' input
    |> List.filter (fun s -> s <> "")
    |> List.map (fun l ->
      let p = String.split_on_char ',' l in
      Coord.make
        (int_of_string (String.trim (List.nth p 0)))
        (int_of_string (String.trim (List.nth p 1))))
    |> List.to_seq
  ;;

  let find_path map size =
    let cache = Hashtbl.create (size * size) in
    let queue = Queue.create () in
    let rec aux map =
      match Queue.take_opt queue with
      | None -> Hashtbl.find_opt cache (Coord.make size size)
      | Some (coord, count) ->
        (match CoordMap.find_opt coord map with
         | Some Corrupted | None -> aux map
         | Some Safe ->
           let old_count =
             Option.value ~default:Int.max_int (Hashtbl.find_opt cache coord)
           in
           if old_count > count
           then (
             Hashtbl.add cache coord count;
             List.iter
               (fun c -> Queue.add (c, count + 1) queue)
               [ Coord.left coord; Coord.right coord; Coord.up coord; Coord.down coord ];
             aux map)
           else aux map)
    in
    Queue.add (Coord.make 0 0, 0) queue;
    aux map
  ;;

  let search_first_impossible part_1_len corrupted size =
    let empty_ram = fill_map CoordMap.empty size Safe in
    let rec aux corrupted size low high =
      if high < low
      then fst (Option.get (Seq.uncons (Seq.drop (low - 1) corrupted)))
      else (
        let mid = low + ((high - low) / 2) in
        match find_path (fill_corrupted corrupted mid empty_ram) size with
        | Some _ -> aux corrupted size (mid + 1) high
        | None -> aux corrupted size low (mid - 1))
    in
    aux corrupted size part_1_len (Seq.length corrupted)
  ;;

  let solve input part_1_len size =
    let corrupted = parse_bytes input in
    let ram = fill_map CoordMap.empty size Safe in
    let ram = fill_corrupted corrupted part_1_len ram in
    let part_1_path_len = Option.get (find_path ram size) in
    let byte = search_first_impossible part_1_len corrupted size in
    [ string_of_int part_1_path_len; Format.asprintf "%a" Coord.pp byte ]
  ;;

  let run input = solve input 1024 70

  let example () =
    let input =
      ""
      ^ "5,4\n"
      ^ "4,2\n"
      ^ "4,5\n"
      ^ "3,0\n"
      ^ "2,1\n"
      ^ "6,3\n"
      ^ "2,4\n"
      ^ "1,5\n"
      ^ "0,6\n"
      ^ "3,3\n"
      ^ "2,6\n"
      ^ "5,1\n"
      ^ "1,2\n"
      ^ "5,5\n"
      ^ "2,5\n"
      ^ "6,5\n"
      ^ "1,4\n"
      ^ "0,4\n"
      ^ "6,4\n"
      ^ "1,1\n"
      ^ "6,1\n"
      ^ "1,0\n"
      ^ "0,5\n"
      ^ "1,6\n"
      ^ "2,0\n"
    in
    solve input 12 6
  ;;
end

include Day18
