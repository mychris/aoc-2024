module Day19 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  let parse input =
    let lines = String.split_on_char '\n' input |> List.to_seq in
    let patterns =
      Seq.take_while (fun s -> s <> "") lines
      |> Seq.fold_left (fun acc l -> acc @ String.split_on_char ',' l) []
      |> List.map String.trim
    in
    let designs =
      Seq.drop_while (fun s -> s <> "") lines
      |> Seq.drop_while (fun s -> s = "")
      |> Seq.filter_map (fun s -> if s = "" then None else Some (String.trim s))
      |> List.of_seq
    in
    patterns, designs
  ;;

  let sub ~prefix word =
    let lp = String.length prefix in
    String.sub word lp (String.length word - lp)
  ;;

  let solve (patterns, designs) =
    let cache = Hashtbl.create (List.length patterns * List.length patterns) in
    let rec solve' patterns design =
      match Hashtbl.find_opt cache design with
      | None ->
        let amount =
          List.filter (fun p -> String.starts_with ~prefix:p design) patterns
          |> List.map (fun p -> solve' patterns (sub ~prefix:p design))
          |> List.fold_left ( + ) 0
        in
        Hashtbl.add cache design amount;
        amount
      | Some amount -> amount
    in
    Hashtbl.add cache "" 1;
    List.map (solve' patterns) designs
  ;;

  let run input =
    let counts = solve (parse input) in
    let first = List.fold_left (fun acc c -> acc + Bool.to_int (c > 0)) 0 counts in
    let second = List.fold_left ( + ) 0 counts in
    List.map string_of_int [ first; second ]
  ;;

  let example () =
    let input =
      ""
      ^ "r, wr, b, g, bwu, rb, gb, br\n"
      ^ "\n"
      ^ "brwrr\n"
      ^ "bggr\n"
      ^ "gbbr\n"
      ^ "rrbgbr\n"
      ^ "ubwu\n"
      ^ "bwurrg\n"
      ^ "brgr\n"
      ^ "bbrgwb\n"
    in
    run input
  ;;
end

include Day19
