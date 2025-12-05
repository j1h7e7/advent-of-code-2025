open Utils

let parse_range line =
  let parts = String.split_on_char '-' @@ String.trim line in
  (int_of_string @@ List.nth parts 0, int_of_string @@ List.nth parts 1)

let%test _ = parse_range "10-100" = (10, 100)

let parse_ranges data =
  let lines = splitlines data in
  List.map parse_range lines

let parse_ids data =
  let lines = splitlines data in
  List.map (fun line -> int_of_string @@ String.trim line) lines

let parse_ranges_and_ids input_data =
  let chunks = Str.split (Str.regexp "\n\n") input_data in
  let ranges = parse_ranges @@ List.nth chunks 0 in
  let ids = parse_ids @@ List.nth chunks 1 in
  (ranges, ids)

let%test _ = parse_ranges_and_ids "1-2\n\n3" = ([ (1, 2) ], [ 3 ])

let is_in_ranges ranges x =
  List.exists (fun (lo, hi) -> x >= lo && x <= hi) ranges

let solve input_data =
  let ranges, ids = parse_ranges_and_ids input_data in
  let in_ranges = List.map (is_in_ranges ranges) ids in
  string_of_int @@ sum @@ List.map (fun x -> if x then 1 else 0) in_ranges
