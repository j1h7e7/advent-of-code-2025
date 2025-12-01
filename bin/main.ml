let () = print_endline ""

let day_num =
  if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 0

let test_suffix = if Array.length Sys.argv >= 3 then Sys.argv.(2) else ""
let data = Advent_of_code.Data.read_file day_num test_suffix
let answer = Advent_of_code.Day1.solve data
let () = print_endline answer
