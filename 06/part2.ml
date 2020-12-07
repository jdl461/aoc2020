open Core

type t = (string,int,Char.comparator_witness) Map.t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.set t ~key:s ~data:(count + 1)

let rec parse_input lst acc row = 
match lst with
| [] -> acc
| h :: [] -> (
  match h with 
  | "" -> List.cons row acc
  | _ -> List.cons (List.cons h row) acc
)
| h :: t -> 
  (
  match h with 
  | "" -> parse_input t (List.cons row acc) []
  | _ -> parse_input t acc (List.cons h row)
)

let count_group (group : string list) =
      let count = List.length group in
      let tmp = String.to_list (String.concat group) in
      let counts = List.fold tmp ~init:(Map.empty (module Char)) ~f:(
        fun acc el -> touch acc el
      ) in
      let tmp = Map.map counts ~f:(fun v1-> let is_complete = phys_equal count v1 in
      match is_complete with 
      | true -> 1 
      | false -> 0
      ) in
      Map.fold tmp ~init:(0) ~f:(fun ~key:_ ~data:v acc -> acc + v)

let input = In_channel.read_lines "input.txt"

let records = parse_input input [] []

let result = 
  let counts = List.map records ~f:count_group in
  List.fold counts ~init:0 ~f:(+)

let () = Stdio.printf "%d\n" result
