open Core
open Base

let rec n_cartesian_product = function
| [] -> [[]]
| h :: t ->
    let rest = n_cartesian_product t in
    List.concat
      (List.map ~f:(fun i -> List.map ~f:(fun r -> i :: r) rest) h)

let entries = List.map (In_channel.read_lines "input1.txt") ~f:Int.of_string

let trips = n_cartesian_product [entries; entries; entries]
let sums = List.map trips ~f:(fun (lst) -> (List.fold lst ~f:( + ) ~init:0, List.fold lst ~f:( * ) ~init:1))
let () = let result = List.find sums ~f:(fun (a,_) -> (phys_equal a 2020)) in
match result with
  | Some (_,b) -> Stdio.print_endline (Int.to_string (b))
  | None -> Stdio.print_endline "None"
