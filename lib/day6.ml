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

let rec parse_chunk lines active_chunk =
  match lines with
  | [] -> [ active_chunk ]
  | [] :: _ -> [ active_chunk ]
  | x :: xs when List.for_all (fun c -> c = ' ') x -> begin
      let res = parse_chunk xs [] in
      res @ [ active_chunk ]
    end
  | x :: xs -> parse_chunk xs (active_chunk @ [ x ])

let chunks_to_vals chunks =
  List.map
    (fun line ->
      int_of_string @@ String.trim @@ String.of_seq @@ List.to_seq line)
    chunks

let parse_data2 data =
  let lines = splitlines data in
  let op_line = lines |> List.rev |> List.hd in
  let vals = lines |> List.rev |> List.tl |> List.rev in
  let char_mat =
    List.map (fun line -> List.of_seq @@ String.to_seq line) vals
  in
  let res = parse_chunk (transpose char_mat) [] in
  let vals' = List.map chunks_to_vals @@ List.rev res in
  let ops = parse_line op_line in
  (vals', ops)

let%test _ =
  parse_data2 "12 34\n5   6\n+  * \n" = ([ [ 15; 2 ]; [ 3; 46 ] ], [ "+"; "*" ])

let solve2 input_data =
  let vals, ops = parse_data2 input_data in
  let ans =
    List.combine ops vals
    |> List.map (fun (op, v) ->
        List.fold_left (get_single_op op) (get_start_val op) v)
  in
  string_of_int @@ sum ans
