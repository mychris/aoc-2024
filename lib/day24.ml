module Day24 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  module StrMap = Map.Make (String)

  type signal =
    | High
    | Low
    | Floating

  type op =
    | AND
    | OR
    | XOR

  type gate =
    { in1 : string
    ; in2 : string
    ; mutable out : string
    ; op : op
    }

  let operate op in1 in2 =
    match in1, in2, op with
    | Floating, _, _ -> Floating
    | _, Floating, _ -> Floating
    | High, High, AND -> High
    | _, _, AND -> Low
    | Low, Low, OR -> Low
    | _, _, OR -> High
    | Low, High, XOR -> High
    | High, Low, XOR -> High
    | _, _, XOR -> Low
  ;;

  let wire_signal_to_int (wire, signal) =
    match signal with
    | Floating -> failwith @@ Format.asprintf "Floating wire %s" wire
    | High -> wire, 1
    | Low -> wire, 0
  ;;

  let parse (input : string) =
    let wire_regex = Re.Pcre.regexp "([a-z-A-Z0-9]+): (0|1)" in
    let gate_regex =
      Re.Pcre.regexp "([a-zA-Z0-9]+) (AND|OR|XOR) ([a-zA-Z0-9]+) -> ([a-zA-Z0-9]+)"
    in
    let gates =
      Re.all gate_regex input
      |> List.fold_left
           (fun acc m ->
             let op =
               match Re.Group.get m 2 with
               | "AND" -> AND
               | "OR" -> OR
               | "XOR" -> XOR
               | x -> failwith @@ Format.asprintf "unknown gate type %s" x
             in
             { in1 = min (Re.Group.get m 1) (Re.Group.get m 3)
             ; in2 = max (Re.Group.get m 1) (Re.Group.get m 3)
             ; out = Re.Group.get m 4
             ; op
             }
             :: acc)
           []
    in
    let wire_states =
      List.fold_left
        (fun m gate ->
          StrMap.add gate.in1 Floating m
          |> StrMap.add gate.in2 Floating
          |> StrMap.add gate.out Floating)
        StrMap.empty
        gates
    in
    let wire_states =
      List.fold_left
        (fun m mtch ->
          let wire = Re.Group.get mtch 1 in
          let state = Re.Group.get mtch 2 in
          StrMap.add wire (if state = "1" then High else Low) m)
        wire_states
        (Re.all wire_regex input)
    in
    wire_states, gates
  ;;

  let rec simulate wire_states gates =
    let change, wire_states =
      List.fold_left
        (fun (change, wire_states) gate ->
          match StrMap.find gate.out wire_states with
          | Floating ->
            let r =
              operate
                gate.op
                (StrMap.find gate.in1 wire_states)
                (StrMap.find gate.in2 wire_states)
            in
            change || r <> Floating, StrMap.add gate.out r wire_states
          | _ -> change, wire_states)
        (false, wire_states)
        gates
    in
    if change then simulate wire_states gates else wire_states
  ;;

  let find_wires wire_states beg =
    StrMap.to_seq wire_states
    |> Seq.filter (fun (wire, _) -> String.get wire 0 = beg)
    |> List.of_seq
    |> List.sort (fun (wire1, _) (wire2, _) -> String.compare wire1 wire2)
  ;;

  let wires_to_number wire_states beg =
    find_wires wire_states beg
    |> List.to_seq
    |> Seq.map wire_signal_to_int
    |> Seq.map snd
    |> Seq.fold_lefti (fun acc i state -> acc lor (state lsl i)) 0
  ;;

  module First = struct
    let solve wire_states gates =
      let end_state = simulate wire_states gates in
      wires_to_number end_state 'z'
    ;;
  end

  module Second = struct
    let find_gate_by_in1 gates in1 op_name =
      List.find (fun g -> (g.in1 = in1 || g.in2 = in1) && g.op = op_name) gates
    ;;

    let find_gate_by_in1_opt gates in1 op_name =
      List.find_opt (fun g -> (g.in1 = in1 || g.in2 = in1) && g.op = op_name) gates
    ;;

    let find_gate_by_in2 gates in1 in2 op_name =
      List.find
        (fun g ->
          ((g.in1 = in1 && g.in2 = in2) || (g.in2 = in1 && g.in1 = in2)) && g.op = op_name)
        gates
    ;;

    let find_gates_by_out gates out =
      let r = List.find_all (fun g -> g.out = out) gates in
      assert (List.length r = 1);
      List.hd r
    ;;

    let swap_gates_out g1 g2 =
      let out = g1.out in
      g1.out <- g2.out;
      g2.out <- out;
      [ g1.out; g2.out ]
    ;;

    let fix_r_gate gates carry_wire bit_out_wire =
      let r_gate = find_gate_by_in1 gates carry_wire XOR in
      if r_gate.out <> bit_out_wire
      then (
        let swap_with = find_gates_by_out gates bit_out_wire in
        swap_gates_out swap_with r_gate)
      else []
    ;;

    let fix_s_gate gates carry_wire x_in y_in z_out =
      let r_gate = find_gate_by_in1 gates carry_wire XOR in
      let s_gate = find_gate_by_in2 gates x_in y_in XOR in
      let wire_s_gate_r_gate =
        if r_gate.in1 = carry_wire then r_gate.in2 else r_gate.in1
      in
      assert (r_gate.out = z_out);
      if s_gate.out <> wire_s_gate_r_gate
      then (
        let swap_with = find_gates_by_out gates wire_s_gate_r_gate in
        swap_gates_out swap_with s_gate)
      else []
    ;;

    let fix_a_carry_gate gates carry_wire x_in y_in =
      let s_gate = find_gate_by_in2 gates x_in y_in XOR in
      let a1_gate = find_gate_by_in2 gates x_in y_in AND in
      let a2_gate = find_gate_by_in2 gates s_gate.out carry_wire AND in
      let o1_gate = find_gate_by_in1_opt gates a1_gate.out OR in
      let o2_gate = find_gate_by_in1_opt gates a2_gate.out OR in
      (* lets assume that for the gates calculating the input for the OR gate, at least one is wired correctly *)
      (* lets also assume, that the output of the or gate, which is the next carry wire, is correct *)
      if Option.is_none o1_gate
      then (
        (* the a1_gate is wired to the wrong output. *)
        let o_gate = Option.get o2_gate in
        let needed_wire = if o_gate.in1 = a2_gate.out then o_gate.in2 else o_gate.in1 in
        let swap_with = find_gates_by_out gates needed_wire in
        o_gate.out, swap_gates_out swap_with a1_gate)
      else if Option.is_none o2_gate
      then (
        (* the a2_gate is wired to the wrong output. *)
        let o_gate = Option.get o1_gate in
        let needed_wire = if o_gate.in1 = a1_gate.out then o_gate.in2 else o_gate.in1 in
        let swap_with = find_gates_by_out gates needed_wire in
        o_gate.out, swap_gates_out swap_with a2_gate)
      else (Option.get o2_gate).out, []
    ;;

    (* We are only to be supposed to swap output wires.
       Wires need to be identified by gate inputs.

       The first adder must be (with c00 beeing the wire for the first carry bit):
       x00 XOR y00 -> z00
       x00 AND y00 -> c00
       We assume, this adder is correct.

       From there on onwards, every adder must be:
       ( x_X XOR y_X ) XOR c_X-1 -> z_X
       ( x_X XOR y_X ) AND ( c_X-1 ) OR ( x_X AND y_X ) -> c_X

       For the last adder, the carry output goes to a z wire

       This is by no means a general solver. Lot's of assumption which seem to hold for the input!
    *)
    let solve _wire_states gates =
      let swaps = Queue.create () in
      let _last_carry_wire = ref (find_gate_by_in2 gates "x00" "y00" AND).out in
      for i = 1 to 44 do
        (* In each iteration, the basic assumption is that the last carry wire is correct *)
        (* Then, it should be possible to find all the correct gates if the circuite itself is as assumed *)
        let x_in = Format.asprintf "x%02d" i in
        let y_in = Format.asprintf "y%02d" i in
        let z_out = Format.asprintf "z%02d" i in
        (* The r_gate, the gate computing the result bit, is the XOR gate which has the last carry bit as input *)
        let swaps1 = fix_r_gate gates !_last_carry_wire z_out in
        (* Now we know, the r_gate has the correct output to the z_X wire *)
        (* One input to the r_gate in the carry, the other is the result of x_X XOR y_X *)
        (* So we can now fix this XOR gate, since there is only one XOR gate with x_X and y_X as inputs *)
        let swaps2 = fix_s_gate gates !_last_carry_wire x_in y_in z_out in
        (* Now the calculation for the outbut bit is correctly wired. *)
        (* In addition, x_X XOR y_X is wired to the correct output, this means we now can fix the first AND *)
        (* gate for the carry calculation *)
        let carry_wire, swaps3 = fix_a_carry_gate gates !_last_carry_wire x_in y_in in
        _last_carry_wire := carry_wire;
        List.iter (fun x -> Queue.add x swaps) swaps1;
        List.iter (fun x -> Queue.add x swaps) swaps2;
        List.iter (fun x -> Queue.add x swaps) swaps3
      done;
      Queue.to_seq swaps |> List.of_seq |> List.sort String.compare |> String.concat ","
    ;;
  end

  let run input =
    let wire_states, gates = parse input in
    let r1 = First.solve wire_states gates in
    let r2 = Second.solve wire_states gates in
    [ string_of_int r1; r2 ]
  ;;

  let example () =
    let input =
      ""
      ^ "x00: 1\n"
      ^ "x01: 0\n"
      ^ "x02: 1\n"
      ^ "x03: 1\n"
      ^ "x04: 0\n"
      ^ "y00: 1\n"
      ^ "y01: 1\n"
      ^ "y02: 1\n"
      ^ "y03: 1\n"
      ^ "y04: 1\n"
      ^ "\n"
      ^ "ntg XOR fgs -> mjb\n"
      ^ "y02 OR x01 -> tnw\n"
      ^ "kwq OR kpj -> z05\n"
      ^ "x00 OR x03 -> fst\n"
      ^ "tgd XOR rvg -> z01\n"
      ^ "vdt OR tnw -> bfw\n"
      ^ "bfw AND frj -> z10\n"
      ^ "ffh OR nrd -> bqk\n"
      ^ "y00 AND y03 -> djm\n"
      ^ "y03 OR y00 -> psh\n"
      ^ "bqk OR frj -> z08\n"
      ^ "tnw OR fst -> frj\n"
      ^ "gnj AND tgd -> z11\n"
      ^ "bfw XOR mjb -> z00\n"
      ^ "x03 OR x00 -> vdt\n"
      ^ "gnj AND wpb -> z02\n"
      ^ "x04 AND y00 -> kjc\n"
      ^ "djm OR pbm -> qhw\n"
      ^ "nrd AND vdt -> hwm\n"
      ^ "kjc AND fst -> rvg\n"
      ^ "y04 OR y02 -> fgs\n"
      ^ "y01 AND x02 -> pbm\n"
      ^ "ntg OR kjc -> kwq\n"
      ^ "psh XOR fgs -> tgd\n"
      ^ "qhw XOR tgd -> z09\n"
      ^ "pbm OR djm -> kpj\n"
      ^ "x03 XOR y03 -> ffh\n"
      ^ "x00 XOR y04 -> ntg\n"
      ^ "bfw OR bqk -> z06\n"
      ^ "nrd XOR fgs -> wpb\n"
      ^ "frj XOR qhw -> z04\n"
      ^ "bqk OR frj -> z07\n"
      ^ "y03 OR x01 -> nrd\n"
      ^ "hwm AND bqk -> z03\n"
      ^ "tgd XOR rvg -> z12\n"
      ^ "tnw OR pbm -> gnj\n"
    in
    run input
  ;;
end

include Day24
