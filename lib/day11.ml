module Day11 : sig
  val run : string -> int list
  val example : unit -> int list
end = struct
  module Pair = struct
    type t = int * int

    let compare (n1, t1) (n2, t2) =
      match compare n1 n2 with
      | 0 -> compare t1 t2
      | c -> c
    ;;
  end

  module PairMap = Map.Make (Pair)

  let parse input =
    String.split_on_char '\n' input
    |> List.hd
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s -> int_of_string s)
  ;;

  let blink l t =
    let cache = ref PairMap.empty in
    let pows = Array.make 25 0 in
    let rec blink' n t =
      if t = 0
      then 1
      else (
        match PairMap.find_opt (n, t) !cache with
        | Some x -> x
        | None ->
          let result =
            if n = 0
            then blink' 1 (t - 1)
            else (
              let digits = int_of_float (floor (log10 (float_of_int n))) + 1 in
              if digits land 1 = 1
              then blink' (n * 2024) (t - 1)
              else (
                let div = pows.(digits lsr 1) in
                blink' (n / div) (t - 1) + blink' (n mod div) (t - 1)))
          in
          cache := PairMap.add (n, t) result !cache;
          result)
    in
    Array.mapi_inplace (fun i _ -> int_of_float (10. ** float_of_int i)) pows;
    List.fold_left (fun acc s -> blink' s t + acc) 0 l
  ;;

  let run input =
    let l = parse input in
    let times = [ 25; 75 ] in
    List.map (fun t -> blink l t) times
  ;;

  let example () = run "125 17\n"
end

include Day11
