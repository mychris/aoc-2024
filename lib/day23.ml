module Day23 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  module IntMap = Map.Make (Int)
  module IntSet = Set.Make (Int)

  module TrippleSet = Set.Make (struct
      type t = int * int * int

      let compare (x1, y1, z1) (x2, y2, z2) =
        compare ((x1 lsl 32) lor (y1 lsl 16) lor z1) ((x2 lsl 32) lor (y2 lsl 16) lor z2)
      ;;
    end)

  let encode_name s = (Char.code (String.get s 0) lsl 8) lor Char.code (String.get s 1)

  let decode_name n =
    String.make 1 (char_of_int (n lsr 8)) ^ String.make 1 (char_of_int (n land 0xFF))
  ;;

  let parse input =
    let r =
      String.split_on_char '\n' input
      |> List.filter (fun s -> s <> "")
      |> List.fold_left
           (fun map line ->
             let pair = String.split_on_char '-' line in
             let a, b = encode_name (List.hd pair), encode_name (List.nth pair 1) in
             IntMap.add_to_list a b (IntMap.add_to_list b a map))
           IntMap.empty
    in
    IntMap.fold
      (fun node conn acc -> IntMap.add node (IntSet.of_list conn) acc)
      r
      IntMap.empty
  ;;

  let is_chief_node x = char_of_int (x lsr 8) = 't'

  let rec choose k l =
    if k = 0
    then [ [] ]
    else (
      match compare (List.length l) k with
      | x when x < 0 -> []
      | 0 -> [ l ]
      | _ ->
        (match l with
         | h :: t -> List.map (fun l -> h :: l) (choose (pred k) t) @ choose k t
         | [] -> failwith "choose"))
  ;;

  module First = struct
    let sort_tripplet (x, y, z) =
      let l = List.sort Int.compare [ x; y; z ] in
      List.hd l, List.nth l 1, List.nth l 2
    ;;

    let check map node connected =
      if not (is_chief_node node)
      then TrippleSet.empty
      else if IntSet.cardinal connected < 2
      then TrippleSet.empty
      else
        List.fold_left
          (fun acc l ->
            let x, y = List.hd l, List.nth l 1 in
            if IntSet.mem y (IntMap.find x map)
            then TrippleSet.add (sort_tripplet (node, x, y)) acc
            else acc)
          TrippleSet.empty
          (choose 2 (IntSet.elements connected))
    ;;

    let solve map =
      let r =
        IntMap.fold
          (fun node connected result ->
            let this_result = check map node connected in
            TrippleSet.union result this_result)
          map
          TrippleSet.empty
      in
      TrippleSet.cardinal r
    ;;
  end

  module Second = struct
    let check_size map size =
      (* Consider only systems which have enough connections *)
      let map =
        IntMap.filter (fun _ conn_set -> IntSet.cardinal conn_set >= pred size) map
      in
      if IntMap.cardinal map < size
      then None
      else (
        (* Sort the systems by the number of connections, descending *)
        let systems =
          List.map
            (fun (node, conn_set) -> node, IntSet.cardinal conn_set)
            (IntMap.bindings map)
          |> List.sort (fun (_, n1) (_, n2) -> compare n2 n1)
          |> List.map fst
        in
        (* Go through each system *)
        List.find_map
          (fun node ->
            (* Look at each set of size - 1 connections and check the properties *)
            choose (size - 1) (IntSet.elements (IntMap.find node map))
            |> List.find_map (fun comb ->
              (* Every node in comb must be connected to every other node in comb *)
              if List.for_all
                   (fun c ->
                     List.for_all
                       (fun d -> d = c || IntSet.mem c (IntMap.find d map))
                       comb)
                   comb
              then Some (List.sort Int.compare (node :: comb))
              else None))
          systems)
    ;;

    let solve map =
      let sizes = Seq.init (IntMap.cardinal map) Fun.id in
      let to_check = Seq.drop 3 sizes |> List.of_seq |> List.rev in
      let result = List.find_map (check_size map) to_check in
      String.concat "," (List.map decode_name (Option.value ~default:[] result))
    ;;
  end

  let run input =
    let map = parse input in
    [ string_of_int (First.solve map); Second.solve map ]
  ;;

  let example () =
    let input =
      String.concat
        "\n"
        [ "kh-tc"
        ; "qp-kh"
        ; "de-cg"
        ; "ka-co"
        ; "yn-aq"
        ; "qp-ub"
        ; "cg-tb"
        ; "vc-aq"
        ; "tb-ka"
        ; "wh-tc"
        ; "yn-cg"
        ; "kh-ub"
        ; "ta-co"
        ; "de-co"
        ; "tc-td"
        ; "tb-wq"
        ; "wh-td"
        ; "ta-ka"
        ; "td-qp"
        ; "aq-cg"
        ; "wq-ub"
        ; "ub-vc"
        ; "de-ta"
        ; "wq-aq"
        ; "wq-vc"
        ; "wh-yn"
        ; "ka-de"
        ; "kh-ta"
        ; "co-tc"
        ; "wh-qp"
        ; "tb-vc"
        ; "td-yn"
        ]
    in
    run input
  ;;
end

include Day23
