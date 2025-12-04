open Utils

module PosSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let parse_char char = match char with '@' -> 1 | _ -> 0
let%test _ = parse_char '@' = 1
let%test _ = parse_char '.' = 0

let parse_grid_line line =
  line |> String.trim |> String.to_seq |> Seq.map parse_char |> List.of_seq

let parse_grid inputdata =
  let lines = splitlines inputdata in
  List.map parse_grid_line lines

let%test _ = parse_grid_line "@.@" = [ 1; 0; 1 ]
let array_2d x y = Array.map (fun _ -> Array.make y 0) (Array.make x 0)

let add_with_cap arr i j v =
  let m = Array.length arr in
  let n = Array.length arr.(0) in
  match (i, j) with
  | x, y when x < 0 || y < 0 -> ()
  | x, y when x >= m || y >= n -> ()
  | _ -> arr.(i).(j) <- arr.(i).(j) + v

let add_around arr i j v =
  let dirs = [ -1; 0; 1 ] in
  dirs
  |> List.iter (fun d1 ->
      begin
        dirs |> List.iter (fun d2 -> add_with_cap arr (i + d1) (j + d2) v)
      end)

let increment_around arr i j = add_around arr i j 1

let count_nearby grid =
  let m = List.length grid in
  let n = List.length @@ List.hd grid in
  let ans = array_2d m n in
  grid
  |> List.iteri (fun i grid_line ->
      begin
        grid_line
        |> List.iteri (fun j v ->
            match v with 1 -> increment_around ans i j | _ -> ())
      end);
  ans

let%test _ = count_nearby [ [ 1; 0; 0 ] ] = [| [| 1; 1; 0 |] |]
let%test _ = count_nearby [ [ 1; 0; 1 ] ] = [| [| 1; 2; 1 |] |]

let is_accessible (grid_val, nearby_val) =
  match (grid_val, nearby_val) with 0, _ -> 0 | 1, x when x < 5 -> 1 | _ -> 0

let combine_2d list1 list2 =
  List.combine list1 list2
  |> List.map (fun (row1, row2) -> List.combine row1 row2)

let solve input_data =
  let grid = parse_grid input_data in
  let nearby = List.map Array.to_list @@ Array.to_list @@ count_nearby grid in
  let ans =
    List.combine grid nearby
    |> List.map (fun (grid_line, nearby_line) ->
        List.combine grid_line nearby_line |> List.map is_accessible |> sum)
    |> sum
  in
  string_of_int ans

let find_accessible grid nearby =
  let nearby' = List.map Array.to_list @@ Array.to_list @@ nearby in
  let combined = combine_2d grid nearby' in
  let filterfunc i j v =
    match is_accessible v with 1 -> Some (i, j) | _ -> None
  in
  List.flatten
  @@ List.mapi
       (fun i line -> list_filter_mapi (fun j v -> filterfunc i j v) line)
       combined

let update_grid nearby accessible =
  PosSet.iter (fun (i, j) -> add_around nearby i j (-1)) accessible

let solve2 input_data =
  let grid = parse_grid input_data in
  let nearby = count_nearby grid in
  let new_accessible = ref @@ PosSet.of_list @@ find_accessible grid nearby in
  let all_accessible = ref @@ !new_accessible in
  while not @@ PosSet.is_empty !new_accessible do
    update_grid nearby !new_accessible;
    all_accessible := PosSet.union !all_accessible !new_accessible;
    new_accessible :=
      PosSet.diff
        (PosSet.of_list @@ find_accessible grid nearby)
        !all_accessible
  done;
  let ans = find_accessible grid nearby in
  string_of_int @@ List.length ans
