let rec pow10 pow = match pow with 0 -> 1 | n -> 10 * pow10 (n - 1)

let parse_range range =
  let ranges = String.split_on_char '-' @@ String.trim range in
  match ranges with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> (0, 0)

let%test _ = parse_range "10-20" = (10, 20)
let%test _ = parse_range "100-0" = (100, 0)

let get_seq bound =
  let strbound = string_of_int bound in
  let len = String.length strbound in
  let seq_size = len / 2 in
  let strseq = String.sub strbound 0 seq_size in
  if len <= 1 then (0, 1) else (int_of_string strseq, len - (len / 2))

let%test _ = get_seq 34 = (3, 1)
let%test _ = get_seq 54 = (5, 1)
let%test _ = get_seq 100 = (1, 2)
let%test _ = get_seq 999 = (9, 2)
let%test _ = get_seq 5200 = (52, 2)

let make_rep_id (seq, size) =
  let mult = pow10 size + 1 in
  mult * seq

let%test _ = make_rep_id (5, 1) = 55
let%test _ = make_rep_id (5, 2) = 505
let is_valid_rep_id n = (String.length @@ string_of_int n) mod 2 = 0

let increment_seq (seq, size) =
  if seq + 1 >= pow10 size then (seq + 1, size + 1) else (seq + 1, size)

let%test _ = increment_seq (5, 1) = (6, 1)
let%test _ = increment_seq (5, 2) = (6, 2)
let%test _ = increment_seq (9, 1) = (10, 2)
let seq_geq (loseq, losize) (hiseq, hisize) = losize >= hisize && loseq >= hiseq

let rec seqs_between lo hi () =
  if seq_geq lo hi then Seq.Nil
  else Seq.Cons (lo, seqs_between (increment_seq lo) hi)

let%test _ = List.of_seq @@ seqs_between (5, 1) (6, 1) = [ (5, 1) ]
let%test _ = List.of_seq @@ seqs_between (5, 1) (7, 1) = [ (5, 1); (6, 1) ]
let%test _ = List.of_seq @@ seqs_between (5, 2) (7, 2) = [ (5, 2); (6, 2) ]
let%test _ = List.of_seq @@ seqs_between (9, 1) (11, 2) = [ (9, 1); (10, 2) ]
let%test _ = List.of_seq @@ seqs_between (9, 1) (1, 2) = [ (9, 1) ]

let get_ids_between (lorange, hirange) =
  let loseq = get_seq lorange in
  let hiseq = increment_seq @@ get_seq hirange in
  let ids0 = seqs_between loseq hiseq in
  let ids1 = Seq.map make_rep_id ids0 in
  let ids2 = Seq.filter is_valid_rep_id ids1 in
  let ids3 = Seq.filter (fun x -> x >= lorange && x <= hirange) ids2 in
  ids3

let%test _ = List.of_seq @@ get_ids_between (50, 60) = [ 55 ]
let%test _ = List.of_seq @@ get_ids_between (50, 69) = [ 55; 66 ]
let%test _ = List.of_seq @@ get_ids_between (56, 69) = [ 66 ]
let%test _ = List.of_seq @@ get_ids_between (500, 600) = []
let%test _ = List.of_seq @@ get_ids_between (500, 1050) = [ 1010 ]
let%test _ = List.of_seq @@ get_ids_between (1000, 1200) = [ 1010; 1111 ]

let ans_for_single_range range =
  let ids = get_ids_between range in
  Seq.fold_left Int.add 0 ids

let%test _ = ans_for_single_range (11, 22) = 33

let solve inputdata =
  let str_ranges0 = String.split_on_char ',' inputdata in
  let str_ranges1 = List.filter (fun x -> String.length x > 0) str_ranges0 in
  let ranges = List.map parse_range str_ranges1 in
  let ans_list = List.map ans_for_single_range ranges in
  string_of_int @@ List.fold_left Int.add 0 ans_list
