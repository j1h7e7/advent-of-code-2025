let () = print_endline ""

let day_num =
  if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 1

let part = if Array.length Sys.argv >= 3 then int_of_string Sys.argv.(2) else 1
let test_suffix = if Array.length Sys.argv >= 4 then Sys.argv.(3) else ""
let data = Advent_of_code.Data.read_file day_num test_suffix

let solver =
  match (day_num, part) with
  | 1, 1 -> Advent_of_code.Day1.solve
  | 1, 2 -> Advent_of_code.Day1.solve2
  | _ -> fun _ -> "N/A"

let answer = solver data
let () = print_endline answer
