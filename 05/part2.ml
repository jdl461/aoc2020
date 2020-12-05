open Core

(* F = 0, B = 1 *)
(* R = 1, L = 0 *)

let input = In_channel.read_lines "input.txt"

let ex = "FBFBBFF" (* 0101100 = 44*)

let to_binary data = 
  let digits =
    let replace x =
    match x with
      | 'F' -> '0'
      | 'L' -> '0'
      | 'B' -> '1'
      | 'R' -> '1'
      | _ -> ' ' in
  String.map data ~f:(replace) in
  "0b" ^ digits

let rec find_seat prev lst =
  match lst with
  | [] -> 0
  | p :: _ when p = prev + 2 -> prev + 1
  | h :: t -> find_seat h t

let ids = List.map input ~f:(
  fun d -> 
    let row_input = String.slice d 0 7 in 
    let row = row_input |> to_binary |> Int.of_string in
    let seat_input = String.slice d 7 10 in
    let seat = seat_input |> to_binary |> Int.of_string in
    row * 8 + seat
)

let sorted = List.sort ids ~compare:Int.compare

let seat = find_seat (List.hd_exn sorted) (List.tl_exn sorted)

let () = Stdio.printf "%d\n" seat
