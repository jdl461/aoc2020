open Core

(* let input = In_channel.read_lines "input.txt" *)

let diffs = Map.empty (module Int)

let add_diff diff map =
  let n_diffs_opt = Map.find map diff in
  let count =
    match n_diffs_opt with Some n_diffs -> n_diffs + 1 | None -> 1
  in
  Map.set map ~key:diff ~data:count

let rec c n lst diffs =
  let next_opt = List.hd lst in
  match next_opt with
  | Some next -> (
      let diff = next - n in
      let new_diffs = add_diff diff diffs in
      let tl_opt = List.tl lst in
      match tl_opt with
      | Some tl -> c next tl new_diffs
      | None -> add_diff 3 diffs )
  | None -> diffs

let rec calculate lst diffs =
  match lst with
  | a :: b :: _ -> (
      let diff = b - a in
      let tl_opt = List.tl lst in
      match tl_opt with
      | Some tl -> calculate tl (add_diff diff diffs)
      | None -> diffs )
  | [ _ ] -> add_diff 3 diffs |> add_diff 1
  | [] -> diffs

let lines = In_channel.read_lines "input.txt"

let input = lines |> List.map ~f:Int.of_string |> List.sort ~compare:Int.compare

let result = calculate input diffs

let seq =
  Map.to_sequence result |> Sequence.to_list
  |> List.map ~f:(fun (a, b) -> Int.to_string a ^ ", " ^ Int.to_string b)
  |> String.concat ~sep:", "

let () = Stdio.print_endline seq
