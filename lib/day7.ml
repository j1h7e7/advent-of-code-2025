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

let process_combined_row comb_row =
  let row, cnt = row_acc comb_row false false in
  (List.tl row, cnt)

let%test _ =
  process_combined_row [ (false, '.'); (true, '^'); (false, '.') ]
  = ([ true; false; true ], 1)

let%test _ =
  process_combined_row [ (false, '^'); (true, '.'); (false, '.') ]
  = ([ false; true; false ], 0)

let run_row beams row = process_combined_row @@ List.combine beams row

let initialize_row first_row =
  List.map (fun c -> c = 'S') @@ str_to_chrs first_row

let solve input_data =
  let lines = splitlines input_data in
  let beams = initialize_row @@ List.hd lines in
  let _, ans =
    List.fold_left
      (fun (beam1, ans1) row ->
        begin
          let beam2, ans2 = run_row beam1 row in
          (beam2, ans1 + ans2)
        end)
      (beams, 0)
      (List.map str_to_chrs lines)
  in
  string_of_int ans
