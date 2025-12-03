open Utils

let parse_rating line =
  line |> String.trim |> String.to_seq
  |> Seq.map (String.make 1)
  |> Seq.map int_of_string |> List.of_seq

let%test _ = parse_rating "1234" = [ 1; 2; 3; 4 ]

let compare_ris (i1, r1) (i2, r2) =
  if i2 > i1 then (i2, r2)
  else if i2 == i1 && r2 < r1 then (i2, r2)
  else (i1, r1)

let%test _ = compare_ris (5, 1) (6, 2) = (6, 2)
let%test _ = compare_ris (5, 2) (6, 1) = (6, 1)
let%test _ = compare_ris (5, 2) (5, 1) = (5, 1)

let get_first_max_rating ratings =
  let ratings_trim = List.take (List.length ratings - 1) ratings in
  let ris = List.mapi (fun i r -> (r, i)) ratings_trim in
  let best = List.fold_left compare_ris (0, 0) ris in
  best

let%test _ = get_first_max_rating [ 9; 9; 0 ] = (9, 0)
let%test _ = get_first_max_rating [ 8; 9; 0 ] = (9, 1)

let rec max_int_in_list nums =
  match nums with
  | [] -> 0
  | v :: vs -> begin
      let m = max_int_in_list vs in
      if m > v then m else v
    end

let get_best_joltage ratings =
  let d1, i1 = get_first_max_rating ratings in
  let remainder = List.drop (i1 + 1) ratings in
  let d2 = max_int_in_list remainder in
  let ans = (d1 * 10) + d2 in
  ans

let%test _ = get_best_joltage [ 9; 8; 7 ] = 98
let%test _ = get_best_joltage [ 9; 7; 8 ] = 98
let%test _ = get_best_joltage [ 8; 7; 9 ] = 89

let parse_ranges inputdata =
  let lines = String.split_on_char '\n' inputdata in
  List.map parse_rating lines

let solve inputdata =
  let ratings = parse_ranges inputdata in
  let joltages = List.map get_best_joltage ratings in
  string_of_int @@ List.fold_left Int.add 0 joltages

let get_first_max_rating2 ratings trim =
  let ratings_trim = List.take (List.length ratings - trim) ratings in
  let ris = List.mapi (fun i r -> (r, i)) ratings_trim in
  let best = List.fold_left compare_ris (0, 0) ris in
  best

let rec get_joltage_step ratings n =
  match n with
  | 0 -> 0
  | x -> begin
      let r, i = get_first_max_rating2 ratings (x - 1) in
      let newratings = List.drop (i + 1) ratings in
      let m = get_joltage_step newratings (n - 1) in
      (r * pow10 (n - 1)) + m
    end

let%test _ = get_joltage_step [ 9; 5; 1 ] 2 = 95
let%test _ = get_joltage_step [ 9; 1; 5 ] 2 = 95
let%test _ = get_joltage_step [ 1; 9; 5 ] 2 = 95
let%test _ = get_joltage_step [ 9; 1; 5 ] 3 = 915

let solve2 inputdata =
  let ratings = parse_ranges inputdata in
  let joltages = List.map (Fun.flip get_joltage_step 12) ratings in
  string_of_int @@ sum joltages
