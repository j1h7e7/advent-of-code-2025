open Utils

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

let increment_with_cap arr i j =
  let m = Array.length arr in
  let n = Array.length arr.(0) in
  match (i, j) with
  | x, y when x < 0 || y < 0 -> ()
  | x, y when x >= m || y >= n -> ()
  | _ -> arr.(i).(j) <- arr.(i).(j) + 1

let increment_around arr i j =
  let dirs = [ -1; 0; 1 ] in
  dirs
  |> List.iter (fun d1 ->
      begin
        dirs |> List.iter (fun d2 -> increment_with_cap arr (i + d1) (j + d2))
      end)

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

let solve input_data =
  let grid = parse_grid input_data in
  let nearby = List.map Array.to_list @@ Array.to_list @@ count_nearby grid in
  let is_accessible =
   fun (grid_val, nearby_val) ->
    match (grid_val, nearby_val) with
    | 0, _ -> 0
    | 1, x when x < 5 -> 1
    | _ -> 0
  in
  let ans =
    List.combine grid nearby
    |> List.map (fun (grid_line, nearby_line) ->
        List.combine grid_line nearby_line |> List.map is_accessible |> sum)
    |> sum
  in
  string_of_int ans
