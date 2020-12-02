open Core
open Base

let entries = List.map (In_channel.read_lines "input1.txt") ~f:Int.of_string

let pairs = List.cartesian_product entries entries 
let sums = List.map pairs ~f:(fun (a,b) -> (a + b, a * b))
let () = let result = List.find sums ~f:(fun (a,_) -> (phys_equal a 2020)) in
match result with
  | Some (_,b) -> Stdio.print_endline (Int.to_string (b))
  | None -> Stdio.print_endline "None"
