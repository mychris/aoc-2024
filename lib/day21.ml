(** After every input, the robot always ends in an A.
    As many repetitions as possible results in the best path.
    Prioritize < over other movements, if possible
    Prioritize v over >
    Rational: if it is farther away from the A, the more robots we need on the dirpads,
    the shorter the path gets, because down the pipe, more inputs can be given on the path.
    Stick with more repetitions, if above priority isn't possible.
    Use memoization. *)

module Day21 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  let numpad_movement here there =
    let here = if here = 'A' then 0xA else Char.code here - Char.code '0' in
    let there = if there = 'A' then 0xA else Char.code there - Char.code '0' in
    let r =
      match (here lsl 4) lor there with
      | 0x00 | 0x11 | 0x22 | 0x33 | 0x44 | 0x55 -> "A"
      | 0x66 | 0x77 | 0x88 | 0x99 | 0xAA -> "A"
      | 0x78 | 0x89 | 0x45 | 0x56 | 0x12 | 0x23 | 0x0A -> ">A"
      | 0x87 | 0x98 | 0x54 | 0x65 | 0x21 | 0x32 | 0xA0 -> "<A"
      | 0x74 | 0x85 | 0x96 | 0x41 | 0x52 | 0x63 | 0x20 | 0x3A -> "vA"
      | 0x47 | 0x58 | 0x69 | 0x14 | 0x25 | 0x36 | 0x02 | 0xA3 -> "^A"
      | 0x71 | 0x82 | 0x93 | 0x50 | 0x6A -> "vvA"
      | 0x17 | 0x28 | 0x39 | 0x05 | 0xA6 -> "^^A"
      | 0x79 | 0x46 | 0x13 -> ">>A"
      | 0x97 | 0x64 | 0x31 -> "<<A"
      | 0x80 | 0x9A -> "vvvA"
      | 0x08 | 0xA9 -> "^^^A"
      | 0x75 | 0x86 | 0x42 | 0x53 | 0x10 | 0x2A -> "v>A"
      | 0x57 | 0x68 | 0x24 | 0x35 | 0xA2 -> "<^A"
      | 0x01 -> "^<A"
      | 0x84 | 0x95 | 0x51 | 0x62 | 0x30 -> "<vA"
      | 0x48 | 0x59 | 0x15 | 0x26 | 0x03 -> "^>A"
      | 0x72 | 0x83 | 0x5A -> "vv>A"
      | 0x40 -> ">vvA"
      | 0x27 | 0x38 | 0xA5 -> "<^^A"
      | 0x04 -> "^^<"
      | 0x81 | 0x92 | 0x60 -> "<vvA"
      | 0x18 | 0x29 | 0x06 -> "^^>A"
      | 0x76 | 0x43 -> "v>>A"
      | 0x1A -> ">>vA"
      | 0x67 | 0x34 -> "<<^A"
      | 0xA1 -> "^<<A"
      | 0x49 | 0x16 -> "^>>A"
      | 0x94 | 0x61 -> "<<vA"
      | 0x70 -> ">vvvA"
      | 0x8A -> "vvv>A"
      | 0x07 -> "^^^<A"
      | 0xA8 -> "<^^^A"
      | 0x90 -> "<vvvA"
      | 0x09 -> "^^^>A"
      | 0x73 -> "vv>>A"
      | 0x4A -> ">>vvA"
      | 0xA4 -> "^^<<A"
      | 0x37 -> "<<^^A"
      | 0x19 -> ">>^^A"
      | 0x91 -> "vv<<A"
      | 0x7A -> ">>vvvA"
      | 0xA7 -> "^^^<<A"
      | x -> failwith (Format.asprintf "numpad_movement %X %X %02X" here there x)
    in
    r
  ;;

  let dirpad_movement (here, there) =
    let r =
      match here, there with
      | '^', '^' | '<', '<' | 'v', 'v' | '>', '>' | 'A', 'A' -> "A"
      | '^', 'A' | '<', 'v' | 'v', '>' -> ">A"
      | 'A', '^' | 'v', '<' | '>', 'v' -> "<A"
      | '^', 'v' | 'A', '>' -> "vA"
      | 'v', '^' | '>', 'A' -> "^A"
      | '<', '>' -> ">>A"
      | '>', '<' -> "<<A"
      | '^', '>' -> "v>A"
      | '>', '^' -> "<^A"
      | '^', '<' -> "v<A"
      | 'A', 'v' -> "<vA"
      | '<', '^' -> ">^A"
      | 'v', 'A' -> "^>A"
      | '<', 'A' -> ">>^A"
      | 'A', '<' -> "v<<A"
      | _ -> failwith (Format.asprintf "dirpad_movement %c %c" here there)
    in
    r
  ;;

  let type_numpad input =
    let rec type_numpad' movements =
      match Seq.uncons movements with
      | None -> ""
      | Some ((here, there), tail) -> numpad_movement here there ^ type_numpad' tail
    in
    type_numpad' (Seq.zip (Seq.cons 'A' (String.to_seq input)) (String.to_seq input))
  ;;

  let type_dirpad n_robots sequence =
    let cache = Hashtbl.create (4 * n_robots) in
    let rec type_dirpad' n_robots movements =
      if n_robots = 0
      then String.length movements
      else (
        match Hashtbl.find_opt cache (movements, n_robots) with
        | None ->
          let len =
            Seq.zip (Seq.cons 'A' (String.to_seq movements)) (String.to_seq movements)
            |> Seq.map dirpad_movement
            |> Seq.map (type_dirpad' (n_robots - 1))
            |> Seq.fold_left ( + ) 0
          in
          Hashtbl.add cache (movements, n_robots) len;
          len
        | Some n -> n)
    in
    type_dirpad' n_robots sequence
  ;;

  let solve inputs dir_robots =
    let numpad_path = List.map type_numpad inputs in
    let lengths = List.map (type_dirpad dir_robots) numpad_path in
    let numbers =
      List.map (fun s -> int_of_string (String.sub s 0 (String.length s - 1))) inputs
    in
    List.fold_left ( + ) 0 (List.map2 ( * ) lengths numbers)
  ;;

  let run input =
    let input = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
    List.map string_of_int [ solve input 2; solve input 25 ]
  ;;

  let example () =
    let _ = [ "029A"; "980A"; "179A"; "456A"; "379A" ] in
    let input = [ "2A" ] in
    List.map string_of_int [ solve input 2; solve input 25 ]
  ;;
end

include Day21
