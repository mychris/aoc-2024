module Day13 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  type coord =
    { x : int
    ; y : int
    }

  type thing =
    { a : coord
    ; b : coord
    ; p : coord
    }

  let parse input =
    let set_a (t : thing) x y = { t with a = { x; y } } in
    let set_b (t : thing) x y = { t with b = { x; y } } in
    let set_p (t : thing) x y = { t with p = { x; y } } in
    let x_y_re = Re.Pcre.regexp ".*: X(\\+|=)([1-9]\\d*), Y(\\+|=)([1-9]\\d*).*" in
    String.split_on_char '\n' input
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s ->
      if String.starts_with ~prefix:"Button A:" s
      then set_a, s
      else if String.starts_with ~prefix:"Button B:" s
      then set_b, s
      else if String.starts_with ~prefix:"Prize:" s
      then set_p, s
      else raise (Failure "input"))
    |> List.map (fun (thing, s) ->
      let m = Re.exec x_y_re s in
      thing, int_of_string (Re.Group.get m 2), int_of_string (Re.Group.get m 4))
    |> List.fold_left
         (fun (l, n) (update, x, y) ->
           let t =
             if n = 3
             then { a = { x = 0; y = 0 }; b = { x = 0; y = 0 }; p = { x = 0; y = 0 } }
             else List.hd l
           in
           let t = update t x y in
           if n = 3 then t :: l, 1 else t :: List.tl l, n + 1)
         ([], 3)
    |> fst
    |> List.rev
  ;;

  let fix_prize (t : thing) =
    { t with p = { x = t.p.x + 10000000000000; y = t.p.y + 10000000000000 } }
  ;;

  let calc t =
    let calc_b div =
      div ((t.a.x * t.p.y) - (t.a.y * t.p.x)) ((t.a.x * t.b.y) - (t.a.y * t.b.x))
    in
    let calc_a div b = div (t.p.x - (t.b.x * b)) t.a.x in
    let answ_b = calc_b ( / ) in
    let test_b = calc_b ( mod ) in
    let answ_a = calc_a ( / ) answ_b in
    let test_a = calc_a ( mod ) answ_b in
    match test_a, test_b with
    | 0, 0 -> Some ((answ_a * 3) + answ_b)
    | _, _ -> None
  ;;

  let run input =
    let machines = parse input in
    List.map
      (fun f -> List.map f machines |> List.filter_map Fun.id |> List.fold_left ( + ) 0)
      [ calc; Fun.compose calc fix_prize ]
    |> List.map string_of_int
  ;;

  let example () =
    let input =
      ""
      ^ "Button A: X+94, Y+34\n"
      ^ "Button B: X+22, Y+67\n"
      ^ "Prize: X=8400, Y=5400\n"
      ^ "\n"
      ^ "Button A: X+26, Y+66\n"
      ^ "Button B: X+67, Y+21\n"
      ^ "Prize: X=12748, Y=12176\n"
      ^ "\n"
      ^ "Button A: X+17, Y+86\n"
      ^ "Button B: X+84, Y+37\n"
      ^ "Prize: X=7870, Y=6450\n"
      ^ "\n"
      ^ "Button A: X+69, Y+23\n"
      ^ "Button B: X+27, Y+71\n"
      ^ "Prize: X=18641, Y=10279\n"
    in
    run input
  ;;
end

include Day13
