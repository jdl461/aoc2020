open Core

let preamble_len = 25

let input = In_channel.read_lines "input.txt" |> List.map ~f:Int.of_string

let find_sum lst target =
  let pairs = List.cartesian_product lst lst in
  List.find pairs ~f:(fun (a, b) -> phys_equal target (a + b))

let rec process preamble lst =
  let target = List.hd_exn lst in
  let opt = find_sum preamble target in
  match opt with
  | None -> target
  | Some _ ->
      process (List.append (List.tl_exn preamble) [ target ]) (List.tl_exn lst)

let preamble, lst = List.split_n input preamble_len

let result = process preamble lst

let () = Stdio.printf "%d\n" result
