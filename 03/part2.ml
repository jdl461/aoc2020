open Core

let entries = In_channel.read_lines "input.txt"
let row_count = List.length entries
let row_width = String.length (List.hd_exn entries)


let get_rows lst n = 
  let tail = List.tl_exn lst in
  List.filteri tail ~f:(fun idx _ -> (idx+1) mod n = 0)

let solve lst right down =
  let rows = get_rows lst down in
    (* let () = Stdio.print_endline (String.concat ~sep:"\n" rows) in *)
    List.mapi rows ~f:(fun idx el -> 
      let i = ((idx+1) * right mod row_width) in
      (* let () = Stdio.printf "%d\n" i in *)
      let ch = String.get el i in
      phys_equal ch '#')

let inputs = [(1,1); (3,1); (5,1); (7,1); (1,2)]
let counts = List.map inputs ~f:(
  fun (right, down) -> 
    let filtered = List.filter (solve entries right down) ~f:(ident) in
    List.length filtered)

let total = List.fold counts ~init:1 ~f:(fun acc a -> acc * a)

let () = Stdio.print_endline (Int.to_string total)
