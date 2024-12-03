let time f =
  let t = Sys.time () in
  let res = f () in
  Format.printf "Execution time: %fs\n" (Sys.time () -. t);
  res
;;
