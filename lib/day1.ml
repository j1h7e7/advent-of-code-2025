let parse_instruction instr =
  let dir = instr.[0] in
  let sval = String.sub instr 1 (String.length instr - 1) in
  let nval = int_of_string sval in
  if dir == 'L' then -nval else nval

let rec count_0_pos (acc, vals) =
  match (acc, vals) with
  | 0, [] -> 1
  | _, [] -> 0
  | 0, v :: vs -> 1 + count_0_pos ((0 + v) mod 100, vs)
  | x, v :: vs -> count_0_pos ((x + v) mod 100, vs)

let solve inputdata =
  let lines = String.split_on_char '\n' inputdata in
  let filtered_lines = List.filter (fun x -> String.length x > 0) lines in
  let parsed_lines = List.map parse_instruction filtered_lines in
  let ans = count_0_pos (50, parsed_lines) in
  string_of_int ans
