let parse_instruction instr =
  let dir = instr.[0] in
  let sval = String.sub instr 1 (String.length instr - 1) in
  let nval = int_of_string sval in
  if dir == 'L' then -nval else nval

let%test _ = parse_instruction "L50" = -50
let%test _ = parse_instruction "R50" = 50

let rec count_0_pos (acc, vals) =
  match (acc, vals) with
  | 0, [] -> 1
  | _, [] -> 0
  | 0, v :: vs -> 1 + count_0_pos ((0 + v) mod 100, vs)
  | x, v :: vs -> count_0_pos ((x + v) mod 100, vs)

let%test _ = count_0_pos (10, [ 10; 20; 30 ]) = 0
let%test _ = count_0_pos (70, [ 10; 20; 30 ]) = 1
let%test _ = count_0_pos (50, [ -50 ]) = 1

let parse_instructions inputdata =
  let lines = String.split_on_char '\n' inputdata in
  let filtered_lines = List.filter (fun x -> String.length x > 0) lines in
  List.map parse_instruction filtered_lines

let solve inputdata =
  let parsed_lines = parse_instructions inputdata in
  let ans = count_0_pos (50, parsed_lines) in
  string_of_int ans

let count_clicks pos_i dif =
  let pos_i' = if pos_i < 0 then pos_i + 100 else pos_i in
  let pos_f = pos_i' + dif in
  let mag = abs pos_f / 100 in
  let neg_err_corr = if pos_f <= 0 && pos_i' != 0 then 1 else 0 in
  (* correct for rounding toward 0 *)
  let ans = mag + neg_err_corr in
  ans

let%test _ = count_clicks 10 100 = 1
let%test _ = count_clicks 0 100 = 1
let%test _ = count_clicks 10 200 = 2
let%test _ = count_clicks 0 200 = 2
let%test _ = count_clicks 0 10 = 0
let%test _ = count_clicks 0 (-100) = 1
let%test _ = count_clicks 0 (-10) = 0
let%test _ = count_clicks 10 (-100) = 1
let%test _ = count_clicks 10 (-200) = 2

let rec count_0_clicks (acc, vals) =
  match (acc, vals) with
  | _, [] -> 0
  | x, v :: vs -> count_clicks x v + count_0_clicks ((x + v) mod 100, vs)

let solve2 inputdata =
  let parsed_lines = parse_instructions inputdata in
  let ans = count_0_clicks (50, parsed_lines) in
  string_of_int ans
