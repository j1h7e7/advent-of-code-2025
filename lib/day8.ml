open Utils
module IntMap = Map.Make (Int)

let top3 list = (List.nth list 0, List.nth list 1, List.nth list 2)

let parse_row row =
  let pos = Str.split (Str.regexp ",") row |> List.map int_of_string in
  top3 pos

let dist (x1, y1, z1) (x2, y2, z2) =
  let d1 = x1 - x2 in
  let d2 = y1 - y2 in
  let d3 = z1 - z2 in
  (d1 * d1) + (d2 * d2) + (d3 * d3)

let find idx x =
  let x' = ref x in
  while !x' != Array.get idx !x' do
    x' := Array.get idx !x'
  done;
  !x'

let union idx x y =
  let a = find idx x in
  let b = find idx y in
  idx.(a) <- b

let pos_pairs positions =
  let ipos = List.mapi (fun i pos -> (i, pos)) positions in
  let all_pairs = cartesian_product ipos ipos in
  List.filter (fun ((i, _), (j, _)) -> i < j) all_pairs

let%test _ = pos_pairs [ 0; 1; 2 ] = [ ((0, 0), (0, 0)) ]
let get_default_0 map key = Option.value (IntMap.find_opt key map) ~default:0

let get_component_counts idx =
  let cmps = Seq.map (find idx) @@ Seq.take (Array.length idx) @@ Seq.ints 0 in
  let count_map =
    Seq.fold_left
      (fun counts cmp -> IntMap.add cmp (get_default_0 counts cmp + 1) counts)
      IntMap.empty cmps
  in
  let counts = List.map (fun (_, x) -> x) @@ IntMap.to_list count_map in
  counts

let solve input_data =
  let lines = splitlines input_data in
  let positions = List.map parse_row lines in
  let idx = Array.init (List.length positions) (fun x -> x) in
  let pairs = pos_pairs positions in
  let dists = List.map (fun ((i, p1), (j, p2)) -> (i, j, dist p1 p2)) pairs in
  let dists' =
    List.sort (fun (_, _, d1) (_, _, d2) -> Stdlib.compare d1 d2) dists
  in
  let topn = List.take 1000 dists' in
  List.iter (fun (i, j, _) -> union idx i j) topn;
  let component_counts =
    List.sort (fun x y -> -Stdlib.compare x y) @@ get_component_counts idx
  in
  let c1, c2, c3 = top3 component_counts in
  let ans = c1 * c2 * c3 in
  string_of_int ans
