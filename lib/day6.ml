open Utils

let space_regex = Str.regexp " +"

let parse_line line =
  Str.split space_regex line |> List.filter (fun x -> String.length x > 0)

let get_single_op op_char =
  match op_char with
  | "+" -> fun x y -> x + y
  | "*" -> fun x y -> x * y
  | _ -> assert false

let%test _ = get_single_op "+" 2 2 = 4

let get_start_val op_char =
  match op_char with "+" -> 0 | "*" -> 1 | _ -> assert false

let parse_data data =
  let lines = splitlines data in
  transpose @@ List.map parse_line lines

let eval_col col =
  let rev_col = List.rev col in
  let op = List.hd rev_col in
  List.fold_left (get_single_op op) (get_start_val op)
    (List.tl rev_col |> List.map int_of_string)

let solve input_data =
  let cols = parse_data input_data in
  let ans = List.map eval_col cols in
  ans |> sum |> string_of_int
