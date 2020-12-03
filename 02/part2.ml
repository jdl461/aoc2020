open Core

let (let*) o f = Option.bind o ~f:f

let entries = In_channel.read_lines "input1.txt"

let extract_value lst idx = match lst with
[] -> None
| h :: _ -> Re2.Match.get ~sub:(`Index idx) h

let xor a b =
  match (a,b) with
  | true, true -> false
  | true, false -> true
  | false, true -> true
  | false, false -> false

let regex = Re2.create_exn "(\\d+)-(\\d+) ([a-z]): (.+)" 
let matches = List.map entries ~f:(fun entry -> 
  let result = Re2.get_matches regex entry in
  match result with
    | Ok x -> 
      let* min = extract_value x 1 in
      let* max = extract_value x 2 in
      let* ch = extract_value x 3 in
      let* pwd = extract_value x 4 in
      let minc = Char.to_string (String.get pwd ((Int.of_string min) - 1)) in
      let maxc = Char.to_string (String.get pwd ((Int.of_string max) - 1)) in
      let matched = xor 
        ((String.compare minc ch) = 0)
        ((String.compare maxc ch) = 0) in
      (match matched with
      | true -> Some true
      | false -> None)
    | Error _ -> None
)

let () = Stdio.print_endline (Int.to_string (List.length (List.filter matches ~f:Option.is_some)))
