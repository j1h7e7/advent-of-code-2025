let read_file day_num =
  let filename = "input/day" ^ string_of_int day_num ^ ".txt" in
  let file = open_in filename in
  let buf = Buffer.create 10000 in
  let rec loop () =
    let line = input_line file in
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    loop ()
  in
  try loop () with End_of_file -> Buffer.contents buf
