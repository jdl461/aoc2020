open Core

type operations = Jmp | Acc | Nop

let _input =
  [
    "nop +0";
    "acc +1";
    "jmp +4";
    "acc +3";
    "jmp -3";
    "acc -99";
    "acc +1";
    "jmp -4";
    "acc +6";
  ]
  |> List.to_array

let input = In_channel.read_lines "input.txt" |> List.to_array

let str_to_operation str =
  match str with "jmp" -> Jmp | "acc" -> Acc | "nop" -> Nop | _ -> Nop

let parse_instr line =
  let ops = String.split ~on:' ' line in
  match ops with
  | [ op; count ] -> (str_to_operation op, Int.of_string count)
  | _ -> (Nop, 0)

let flip_op idx =
  let i = input.(idx) in
  let op = parse_instr i in
  match op with
  | Nop, count ->
      let () = input.(idx) <- "jmp " ^ Int.to_string count in
      i
  | Jmp, count ->
      let () = input.(idx) <- "nop " ^ Int.to_string count in
      i
  | Acc, _ -> i

let rec run acc pc map =
  try
    let instr = input.(pc) in
    let seen_op = Map.find map pc in
    match seen_op with
    | Some true -> None
    | _ -> (
        let op = parse_instr instr in
        let updated = Map.set map ~key:pc ~data:true in
        match op with
        | Jmp, count -> run acc (pc + count) updated
        | Acc, count -> run (acc + count) (pc + 1) updated
        | Nop, _ -> run acc (pc + 1) updated )
  with Invalid_argument err ->
    let () = Stdio.print_endline err in
    Some acc

let rec brute_force line_count =
  match line_count with
  | -1 -> -1
  | _ -> (
      let _ = flip_op line_count in
      let result = run 0 0 (Map.empty (module Int)) in
      match result with
      | None ->
          let _ = flip_op line_count in
          brute_force (line_count - 1)
      | Some acc -> acc )

let result = brute_force (Array.length input - 1)

let () = Stdio.printf "%d\n" result
