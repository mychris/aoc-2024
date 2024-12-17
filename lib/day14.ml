module Day14 : sig
  val run : string -> string list
  val example : unit -> string list
end = struct
  type spec =
    { x : int
    ; y : int
    ; vx : int
    ; vy : int
    }

  let line_re = Re.Pcre.regexp "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)"

  let parse input =
    String.split_on_char '\n' input
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s ->
      let m = Re.exec line_re s in
      { x = int_of_string (Re.Group.get m 1)
      ; y = int_of_string (Re.Group.get m 2)
      ; vx = int_of_string (Re.Group.get m 3)
      ; vy = int_of_string (Re.Group.get m 4)
      })
  ;;

  let count spec width height =
    let x_mid = width lsr 1 in
    let y_mid = height lsr 1 in
    let q1, q2, q3, q4 =
      List.fold_left
        (fun (q1, q2, q3, q4) spec ->
          if spec.x = x_mid || spec.y = y_mid
          then q1, q2, q3, q4
          else (
            match spec.x < x_mid, spec.y < y_mid with
            | true, true -> q1 + 1, q2, q3, q4
            | true, false -> q1, q2 + 1, q3, q4
            | false, true -> q1, q2, q3 + 1, q4
            | false, false -> q1, q2, q3, q4 + 1))
        (0, 0, 0, 0)
        spec
    in
    [ q1; q2; q3; q4 ]
  ;;

  let move specs width height times =
    let wrap n m = ((n mod m) + m) mod m in
    let move' times width height spec =
      { spec with
        x = wrap (spec.x + (spec.vx * times)) width
      ; y = wrap (spec.y + (spec.vy * times)) height
      }
    in
    List.map (move' times width height) specs
  ;;

  (* lets check if there is a line which has more then 25 next to each other *)
  (* chosen somewhat arbitrarily after looking at the ouput images *)
  let check_tree specs =
    let rec check_tree' specs this_line =
      if this_line >= 25
      then true
      else (
        match specs with
        | f :: (s :: _ as tl) ->
          if f.y <> s.y || s.x - f.x <> 1
          then check_tree' tl 0
          else check_tree' tl (this_line + 1)
        | [] -> false
        | _ :: [] -> false)
    in
    let specs =
      List.fast_sort
        (fun s1 s2 ->
          match compare s1.y s2.y with
          | 0 -> compare s1.x s2.x
          | c -> c)
        specs
    in
    check_tree' specs 0
  ;;

  let find_tree specs width height =
    let rec find_tree' specs count =
      if check_tree specs
      then count
      else find_tree' (move specs width height 1) (count + 1)
    in
    find_tree' specs 0
  ;;

  let run input =
    let width, height = 101, 103 in
    let specs = parse input in
    let specs_after_100 = move specs width height 100 in
    List.map
      string_of_int
      [ List.fold_left ( * ) 1 (count specs_after_100 width height)
      ; find_tree specs width height
      ]
  ;;

  let example () =
    let input =
      ""
      ^ "p=0,4 v=3,-3\n"
      ^ "p=6,3 v=-1,-3\n"
      ^ "p=10,3 v=-1,2\n"
      ^ "p=2,0 v=2,-1\n"
      ^ "p=0,0 v=1,3\n"
      ^ "p=3,0 v=-2,-2\n"
      ^ "p=7,6 v=-1,-3\n"
      ^ "p=3,0 v=-1,-2\n"
      ^ "p=9,3 v=2,3\n"
      ^ "p=7,3 v=-1,2\n"
      ^ "p=2,4 v=2,-3\n"
      ^ "p=9,5 v=-3,-3\n"
    in
    let specs = parse input in
    let specs_after_100 = move specs 11 7 100 in
    List.map string_of_int [ List.fold_left ( * ) 1 (count specs_after_100 11 7); 0 ]
  ;;
end

include Day14
