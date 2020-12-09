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

let rec run acc pc map =
  try
    let instr = input.(pc) in
    let () = Stdio.print_endline ("looking for " ^ Int.to_string pc) in
    let seen_op = Map.find map pc in
    match seen_op with
    | Some true -> acc
    | _ -> (
        let op = parse_instr instr in
        let () = Stdio.print_endline ("Adding " ^ instr) in
        let updated = Map.set map ~key:pc ~data:true in
        match op with
        | Jmp, count -> run acc (pc + count) updated
        | Acc, count -> run (acc + count) (pc + 1) updated
        | Nop, _ -> run acc (pc + 1) updated )
  with Invalid_argument err ->
    let () = Stdio.print_endline err in
    acc

let result = run 0 0 (Map.empty (module Int))

let () = Stdio.printf "%d\n" result
