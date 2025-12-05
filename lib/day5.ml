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

let rec combine_ranges in_ranges out_ranges =
  match (in_ranges, out_ranges) with
  | [], y -> y
  | x :: xs, [] -> combine_ranges xs [ x ]
  | x :: xs, y :: ys -> begin
      let x1, x2 = x in
      let y1, y2 = y in
      let new_head =
        if x1 > y2 then [ (x1, x2); (y1, y2) ] else [ (y1, Int.max x2 y2) ]
      in
      let new_out_ranges = List.append new_head ys in
      combine_ranges xs new_out_ranges
    end

let%test _ = combine_ranges [ (1, 2) ] [] = [ (1, 2) ]
let%test _ = combine_ranges [ (1, 2); (2, 3) ] [] = [ (1, 3) ]
let%test _ = combine_ranges [ (1, 3); (2, 4) ] [] = [ (1, 4) ]
let%test _ = combine_ranges [ (1, 2); (5, 6) ] [] = [ (5, 6); (1, 2) ]
let range_size (r1, r2) = r2 - r1 + 1

let solve2 input_data =
  let range_chunk = List.nth (Str.split (Str.regexp "\n\n") input_data) 0 in
  let ranges =
    List.sort (fun (x, _) (y, _) -> Stdlib.compare x y)
    @@ parse_ranges range_chunk
  in
  let ranges' = combine_ranges ranges [] in
  let ans = sum @@ List.map range_size ranges' in
  string_of_int ans
