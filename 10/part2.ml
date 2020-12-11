open Core

exception Wtf of string

let print_list_list lst =
  List.map lst ~f:(fun x ->
      "[" ^ String.concat ~sep:"; " (List.map x ~f:Int.to_string) ^ "]")
  |> String.concat ~sep:"; "

(* let input = String.split_lines lines |> List.map ~f:Int.of_string *)

let input = In_channel.read_lines "input.txt" |> List.map ~f:Int.of_string

let max =
  let maxopt = List.max_elt input ~compare:Int.compare in
  match maxopt with Some max -> max | None -> raise (Wtf "wtf")

let input = List.append [ 0; max + 3 ] input |> List.sort ~compare:Int.compare

let groups = List.group input ~break:(fun a b -> phys_equal (b - a) 3)

let () = Stdio.print_endline (print_list_list groups)

let lens = List.map groups ~f:List.length

let total =
  List.map lens ~f:(fun x ->
      match x with 1 -> 1 | 2 -> 1 | 3 -> 2 | 4 -> 4 | _ -> 7)
  |> List.fold ~init:1 ~f:( * )

let () = Stdio.printf "%d\n" total
