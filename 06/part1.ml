open Core

let input = In_channel.read_lines "input.txt"

let rec parse_input lst acc row = 
  match lst with
  | [] -> acc
  | h :: [] -> (
    match h with 
    | "" -> List.cons row acc
    | _ -> List.cons (h ^ row) acc
  )
  | h :: t -> 
    (
    match h with 
    | "" -> parse_input t (List.cons row acc) ""
    | _ -> parse_input t acc (h ^ row)
  )

let dedupe data = 
  let chars = String.to_list data in
  let uniques = List.dedup_and_sort chars ~compare:Char.compare in
  String.of_char_list uniques

let records = parse_input input [] ""

let result = 
  let processed = List.map records ~f:dedupe in
  let counts = List.map processed ~f:String.length in
  List.fold counts ~init:0 ~f:(+)

let () = Stdio.printf "%d\n" result

