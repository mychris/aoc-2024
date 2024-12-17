module Day17 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  type state =
    { a : int
    ; b : int
    ; c : int
    ; pc : int
    ; out : int list
    }

  let parse input =
    let a, b, c, p =
      String.split_on_char '\n' input
      |> List.fold_left
           (fun (a, b, c, p) line ->
             if String.starts_with ~prefix:"Register A:" line
             then int_of_string (String.sub line 12 (String.length line - 12)), b, c, p
             else if String.starts_with ~prefix:"Register B:" line
             then a, int_of_string (String.sub line 12 (String.length line - 12)), c, p
             else if String.starts_with ~prefix:"Register C:" line
             then a, b, int_of_string (String.sub line 12 (String.length line - 12)), p
             else if String.starts_with ~prefix:"Program: " line
             then
               ( a
               , b
               , c
               , List.map
                   int_of_string
                   (String.split_on_char ',' (String.sub line 9 (String.length line - 9)))
               )
             else a, b, c, p)
           (0, 0, 0, [])
    in
    Array.of_list p, { a; b; c; pc = 0; out = [] }
  ;;

  let cycle state program =
    if state.pc < Array.length program
    then (
      let op, literal = program.(state.pc), program.(state.pc + 1) in
      let combo =
        match literal with
        | 0 | 1 | 2 | 3 -> literal
        | 4 -> state.a
        | 5 -> state.b
        | 6 -> state.c
        | _ -> failwith (Format.asprintf "combo %d" literal)
      in
      let state = { state with pc = state.pc + 2 } in
      match op with
      | 0 -> { state with a = state.a lsr combo }
      | 1 -> { state with b = state.b lxor literal }
      | 2 -> { state with b = combo land 0b111 }
      | 3 -> if state.a <> 0 then { state with pc = literal } else state
      | 4 -> { state with b = state.b lxor state.c }
      | 5 -> { state with out = (combo land 0b111) :: state.out }
      | 6 -> { state with b = state.a lsr combo }
      | 7 -> { state with c = state.a lsr combo }
      | _ -> failwith (Format.asprintf "op %d" op))
    else failwith "cycle"
  ;;

  let emulate state program =
    let rec emulate' state program =
      if state.pc < Array.length program
      then emulate' (cycle state program) program
      else state
    in
    let end_state = emulate' state program in
    { end_state with out = List.rev end_state.out }
  ;;

  let emulate_with_a program a = emulate { a; b = 0; c = 0; pc = 0; out = [] } program

  (* It seems we need to assume that there is only one operation on a which uses a literal *)
  (* Let's find it and use that operation to reduce the search space *)
  let find_a_op program =
    let rec aux program pc a =
      if pc >= Array.length program
      then (
        match a with
        | None -> failwith "can not analyze, no op on a found"
        | Some x -> x)
      else if program.(pc) = 0 && Option.is_some a
      then failwith "can not analyze, multiple operations on a"
      else if program.(pc) <> 0
      then aux program (pc + 2) a
      else if program.(pc + 1) >= 4
      then failwith "can not analyze, op on a depends on other register"
      else aux program (pc + 2) (Some program.(pc + 1))
    in
    aux program 0 None
  ;;

  let cmp_prog_out program out =
    let out_arr = Array.of_list out in
    Array.length program = Array.length out_arr
    && Array.for_all2 (fun x y -> x = y) program out_arr
  ;;

  (* After a quick analysis of the program, it appears that in each iteration, a is shifted by a constant amount *)
  (* The program loop depends on the current value of a in each iteration *)
  (* It appears that the program can be constructed backwards, by trying out all the possible values of a for *)
  (* each iteration *)
  let backtrack_solution program =
    let a_shift_amount = find_a_op program in
    let rec aux program rev_program a_solution this_a =
      if this_a >= 1 lsl a_shift_amount
      then None
      else (
        match rev_program with
        | [] ->
          if cmp_prog_out program (emulate_with_a program a_solution).out
          then Some (a_solution lor this_a)
          else None
        | hd :: tl ->
          let a_to_try = (a_solution lsl a_shift_amount) lor this_a in
          let es = emulate_with_a program a_to_try in
          if List.hd es.out = hd
          then (
            match aux program tl a_to_try 0 with
            | None -> aux program rev_program a_solution (this_a + 1)
            | x -> x)
          else aux program rev_program a_solution (this_a + 1))
    in
    aux program (List.rev (Array.to_list program)) 0 0
  ;;

  let run input =
    let program, state = parse input in
    let es = emulate state program in
    let magic_a = backtrack_solution program in
    [ List.map string_of_int es.out |> String.concat ","
    ; (if Option.is_none magic_a then "None" else string_of_int (Option.get magic_a))
    ]
  ;;

  let example () =
    let input =
      ""
      ^ "Register A: 2024\n"
      ^ "Register B: 0\n"
      ^ "Register C: 0\n"
      ^ "\n"
      ^ "Program: 0,3,5,4,3,0\n"
    in
    run input
  ;;
end

include Day17
