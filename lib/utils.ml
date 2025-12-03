let rec pow10 pow = match pow with 0 -> 1 | n -> 10 * pow10 (n - 1)
let sum nums = List.fold_left Int.add 0 nums
