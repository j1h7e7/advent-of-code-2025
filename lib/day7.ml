open Utils

let rec row_acc comb_row m1 m2 =
  match comb_row with
  | [] -> ([ m1 ], 0)
  | (t, r) :: xs -> begin
      let is_split = r = '^' in
      let m0' = m1 || (t && is_split) in
      let m1' = m2 || (t && not is_split) in
      let m2' = t && is_split in
      let beams, ans = row_acc xs m1' m2' in
      ([ m0' ] @ beams, ans + Bool.to_int (t && is_split))
    end

let run_row comb_row =
  let row, cnt = row_acc comb_row false false in
  (List.tl row, cnt)

let%test _ =
  run_row [ (false, '.'); (true, '^'); (false, '.') ]
  = ([ true; false; true ], 1)

let%test _ =
  run_row [ (false, '^'); (true, '.'); (false, '.') ]
  = ([ false; true; false ], 0)

let initialize_row first_row =
  List.map (fun c -> c = 'S') @@ str_to_chrs first_row

let solve input_data =
  let lines = splitlines input_data in
  let beams = initialize_row @@ List.hd lines in
  let _, ans =
    List.fold_left
      (fun (beam1, ans1) row ->
        begin
          let beam2, ans2 = run_row @@ List.combine beam1 row in
          (beam2, ans1 + ans2)
        end)
      (beams, 0)
      (List.map str_to_chrs lines)
  in
  string_of_int ans

let rec row_acc2 comb_row m1 m2 =
  match comb_row with
  | [] -> [ m1 ]
  | (t, r) :: xs -> begin
      let is_split = r = '^' in
      let m0' = m1 + if is_split then t else 0 in
      let m1' = m2 + if is_split then 0 else t in
      let m2' = if is_split then t else 0 in
      let beams = row_acc2 xs m1' m2' in
      [ m0' ] @ beams
    end

let run_row2 comb_row =
  let row = row_acc2 comb_row 0 0 in
  List.tl row

let%test _ = run_row2 [ (1, '.'); (2, '^'); (0, '.') ] = [ 3; 0; 2 ]
let%test _ = run_row2 [ (0, '^'); (4, '.'); (2, '.') ] = [ 0; 4; 2 ]

let solve2 input_data =
  let lines = splitlines input_data in
  let beams = List.map Bool.to_int @@ initialize_row @@ List.hd lines in
  let ans =
    List.fold_left
      (fun beam1 row -> run_row2 @@ List.combine beam1 row)
      beams
      (List.map str_to_chrs lines)
  in
  string_of_int @@ sum ans
