let time f =
  let t = Sys.time () in
  let res = f () in
  Printf.printf "Execution time: %fs" (Sys.time () -. t);
  print_endline "";
  res
;;
