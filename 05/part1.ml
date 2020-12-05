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

let ids = List.map input ~f:(
  fun d -> 
    let row_input = String.slice d 0 7 in 
    let row = (to_binary (row_input)) |> Int.of_string in
    let seat_input = String.slice d 7 10 in
    let seat = (to_binary (seat_input)) |> Int.of_string in
    row * 8 + seat
)

let max = let opt = List.max_elt ids ~compare:Int.compare in Option.value_exn opt

let () = Stdio.printf "%d\n" max

