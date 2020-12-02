open Core
open Base

let entries = List.map (In_channel.read_lines "input1.txt") ~f:Int.of_string

let () = 
List.iter entries ~f:(fun (a) -> 
  List.iter entries ~f:(fun (b) -> 
    List.iter entries ~f:(fun (c) -> if (a+b+c) = 2020 then Stdio.print_endline (Int.to_string (a*b*c)))))