open Core

let input = In_channel.read_lines "input.txt" |> List.map ~f:Int.of_string

let target = 32321523

let find_sum lst t =
  List.fold_until lst ~init:(0, 0)
    ~f:(fun (count, idx) a ->
      let s = count + a in
      if s > t then Stop None
      else
        let b = phys_equal s t in
        if b then Stop (Some idx) else Continue (s, idx + 1))
    ~finish:(fun _ -> None)

let rec calculate lst t idx =
  let lst = lst in
  let o = find_sum lst t in
  match o with
  | Some idx ->
      let slice = List.sub lst ~pos:0 ~len:(idx + 1) in
      let min = List.min_elt slice ~compare:Int.compare in
      let max = List.max_elt slice ~compare:Int.compare in
      let add (a : int option) (b : int option) =
        match (a, b) with Some a, Some b -> Some (a + b) | _ -> None
      in
      add min max
  | None -> calculate (List.tl_exn lst) t (idx + 1)

let () =
  let result = calculate input target 1 in
  match result with
  | Some a -> Stdio.printf "%d\n" a
  | None -> Stdio.print_endline "nope"
