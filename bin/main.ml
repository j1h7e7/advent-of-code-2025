let () = print_endline ""

let day_num : int =
  if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 1

let part : int =
  if Array.length Sys.argv >= 3 then int_of_string Sys.argv.(2) else 1

let test_suffix : string =
  if Array.length Sys.argv >= 4 then Sys.argv.(3) else ""

let data : string = Advent_of_code.Data.read_file day_num test_suffix

let solver : string -> string =
  match (day_num, part) with
  | 1, 1 -> Advent_of_code.Day1.solve
  | 1, 2 -> Advent_of_code.Day1.solve2
  | 2, 1 -> Advent_of_code.Day2.solve
  | 2, 2 -> Advent_of_code.Day2.solve2
  | _ -> fun _ -> "N/A"

let answer = solver data
let () = print_endline answer
