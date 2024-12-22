(** Not a very optimal solution, but I really wanted to using sequences for this *)
module Day22 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  module IntSet = Set.Make (Int)
  module IntMap = Map.Make (Int)

  let encode_diff a b c d =
    ((a + 10) lsl 24) lor ((b + 10) lsl 16) lor ((c + 10) lsl 8) lor (d + 10)
  ;;

  let next_sn sn =
    let sn = (sn lsl 6) lxor sn land 0xFFFFFF in
    let sn = (sn lsr 5) lxor sn in
    let sn = (sn lsl 11) lxor sn land 0xFFFFFF in
    sn
  ;;

  let sn_seq sn = Seq.iterate next_sn sn

  let sn_diff_seq sn =
    let seq = sn_seq sn in
    Seq.zip seq (Seq.drop 1 seq) |> Seq.map (fun (x, y) -> (y mod 10) - (x mod 10))
  ;;

  let change_seq sn =
    let diff_seq = sn_diff_seq sn in
    diff_seq
    |> Seq.zip (Seq.drop 1 diff_seq)
    |> Seq.zip (Seq.drop 2 diff_seq)
    |> Seq.zip (Seq.drop 3 diff_seq)
    |> Seq.map (fun (a, (b, (c, d))) -> encode_diff a b c d)
    |> Seq.zip (Seq.drop 4 (sn_seq sn) |> Seq.map (fun x -> x mod 10))
    |> Seq.map (fun (x, y) -> y, x)
  ;;

  module Second = struct
    let solve sn_list =
      (* For each of the inputs, calculate a map which maps from the four consecutive changes to the resulting number of bananas *)
      let change_maps =
        List.map (fun sn -> change_seq sn |> Seq.take 2000 |> List.of_seq) sn_list
        |> List.map (fun cs ->
          List.fold_right
            (fun (change, bananas) map -> IntMap.add change bananas map)
            cs
            IntMap.empty)
      in
      (* Go through all these maps and create one with the sums *)
      let sum_bananas add old = Some (Option.value ~default:0 old + add) in
      let acc_change_map =
        List.fold_left
          (fun map this_change_map ->
            IntMap.fold
              (fun change bananas map -> IntMap.update change (sum_bananas bananas) map)
              this_change_map
              map)
          IntMap.empty
          change_maps
      in
      (* Now find the key with the biggest value in the map *)
      let r =
        IntMap.fold
          (fun _ this_bananas bananas -> max this_bananas bananas)
          acc_change_map
          0
      in
      r
    ;;
  end

  module First = struct
    let solve sn_list =
      List.map (Seq.iterate next_sn) sn_list
      |> List.map (Seq.drop 2000)
      |> List.map (Seq.take 1)
      |> List.map (fun s -> fst (Option.get (Seq.uncons s)))
      |> List.fold_left ( + ) 0
    ;;
  end

  let run input =
    let input =
      String.split_on_char '\n' input
      |> List.filter (fun s -> s <> "")
      |> List.map int_of_string
    in
    List.map string_of_int [ First.solve input; Second.solve input ]
  ;;

  let example () =
    let _ = [ 1; 10; 100; 2024 ] in
    let input = [ 1; 2; 3; 2024 ] in
    List.map string_of_int [ First.solve input; Second.solve input ]
  ;;
end

include Day22
