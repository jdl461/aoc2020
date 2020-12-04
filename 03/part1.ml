open Core

let entries = In_channel.read_lines "input.txt"

let row_count = List.length entries
let row_width = String.length (List.hd_exn entries)

let () = Stdio.printf "Grid (w x h) is (%i x %i)\n" row_width row_count

let coords = List.init (row_count - 1) ~f:(fun i -> ( (i + 1) * 3) mod row_width)

let count = 
let tmp = List.map (List.zip_exn coords (List.tl_exn entries)) 
  ~f:(fun (coord, row) -> phys_equal '#' (String.get row coord)) in
  let filtered = List.filter tmp ~f:ident in
  List.length filtered

let () = Stdio.printf "%d\n" count
