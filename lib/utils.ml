let rec pow10 pow = match pow with 0 -> 1 | n -> 10 * pow10 (n - 1)
let sum nums = List.fold_left Int.add 0 nums

let splitlines str =
  str |> String.split_on_char '\n'
  |> List.filter (fun x -> String.length @@ String.trim x > 0)
