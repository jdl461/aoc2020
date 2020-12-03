open Core

let extract_value lst idx = match lst with
[] -> ""
| h :: _ -> Re2.Match.get_exn ~sub:(`Index idx) h 

let evaluate_password min max ch pwd = 
  let regex = Re2.create_exn ch in
  let x = Re2.get_matches regex pwd in
  let cnt = match x with 
  | Ok x -> List.length x
  | Error _ -> (-1) in
  (cnt >= min) && (cnt <= max)

let entries = In_channel.read_lines "input1.txt"

let regex = Re2.create_exn "(\\d+)-(\\d+) ([a-z]): (.+)" 

let matches = List.map entries ~f:(fun entry -> 
let result = Re2.get_matches regex entry in
match result with
  | Ok x -> let min = extract_value x 1 in
  let max = extract_value x 2 in
  let ch = extract_value x 3 in
  let pwd = extract_value x 4 in
  evaluate_password (Int.of_string min) (Int.of_string max) ch pwd
  | Error _ -> false
)

let () = Stdio.print_endline (Int.to_string (List.length (List.filter matches ~f:(fun m -> m))))
