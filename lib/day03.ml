module Day03 : sig
  val run : string -> int list
  val example : unit -> int list
end = struct
  type intsr =
    | Do
    | Dont
    | Mul of (int * int)

  let find_regex = Re.Pcre.regexp "mul\\([1-9]\\d{0,2},[1-9]\\d{0,2}\\)|do(n't)?\\(\\)"
  let mul_regex = Re.Pcre.regexp "mul\\(([1-9]\\d{0,2}),([1-9]\\d{0,2})\\)"

  let parse_input input =
    let parse_instr instr_str =
      match instr_str with
      | "do()" -> Do
      | "don't()" -> Dont
      | _ ->
        let m = Re.exec mul_regex instr_str in
        Mul (int_of_string (Re.Group.get m 1), int_of_string (Re.Group.get m 2))
    in
    Re.all find_regex input
    |> List.map (fun m -> Re.Group.get m 0)
    |> List.map parse_instr
  ;;

  let execute stream =
    List.map
      (fun instr ->
        match instr with
        | Do | Dont -> raise (Failure "unhandled flow")
        | Mul (x, y) -> x * y)
      stream
  ;;

  let skip_flow stream =
    List.filter
      (fun instr ->
        match instr with
        | Do | Dont -> false
        | _ -> true)
      stream
  ;;

  let interp_flow stream =
    let rec inner stream state =
      match stream with
      | head :: tail ->
        (match head with
         | Do -> inner tail true
         | Dont -> inner tail false
         | Mul x -> if state then Mul x :: inner tail state else inner tail state)
      | [] -> []
    in
    inner stream true
  ;;

  let solve f input = parse_input input |> f |> execute |> List.fold_left ( + ) 0
  let run input = [ solve skip_flow input; solve interp_flow input ]

  let example () =
    let in1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" in
    let in2 =
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    in
    [ solve skip_flow in1; solve interp_flow in2 ]
  ;;
end

include Day03
