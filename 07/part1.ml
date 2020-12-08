open Core

let ex =
  [
    "light red bags contain 1 bright white bag, 2 muted yellow bags.";
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.";
    "bright white bags contain 1 shiny gold bag.";
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.";
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.";
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.";
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.";
    "faded blue bags contain no other bags.";
    "dotted black bags contain no other bags.";
  ]

let input = In_channel.read_lines "input.txt"

(* let input = ex *)

let re = Re2.create_exn "^(.+) bags contain (.+)+\\.+$"

let re2 = Re2.create_exn "\\d (.+) (bag|bags)"

let matches rule = Re2.get_matches_exn re rule

let bag_rules = List.map input ~f:matches

let add_to_map m k v =
  let container =
    let c = Map.find m k in
    match c with Some cntr -> cntr | None -> []
  in
  Map.set m ~key:k ~data:(List.cons v container)

let tree =
  List.fold bag_rules
    ~init:(Map.empty (module String))
    ~f:(fun acc rule ->
      match List.hd rule with
      | None -> acc
      | Some r ->
          let container = Re2.Match.get_exn ~sub:(`Index 1) r in
          let contents =
            String.split ~on:','
              (Re2.Match.get_exn ~sub:(`Index 2) (List.hd_exn rule))
          in
          let contents = List.map contents ~f:String.strip in
          let contents =
            List.map contents ~f:(fun rule ->
                match rule with
                | "no other bags" -> ""
                | _ -> (
                    let matches = Re2.get_matches_exn re2 rule in
                    let m =
                      Re2.Match.get ~sub:(`Index 1) (List.hd_exn matches)
                    in
                    match m with None -> "" | Some x -> x ))
          in
          let contents =
            List.filter contents ~f:(fun s -> not (String.is_empty s))
          in
          List.fold contents ~init:acc ~f:(fun acc2 a ->
              add_to_map acc2 a container))

(* let () =
  Map.iteri tree ~f:(fun ~key:k ~data:v ->
      Stdio.print_endline (k ^ ": " ^ String.concat ~sep:", " v)) *)

let rec resolve (bags : string list) (collection : string list)
    (t : (string, string list, Base.String.comparator_witness) Map_intf.Map.t) =
  let () = Stdio.print_endline ("bags: " ^ String.concat ~sep:", " bags) in
  let () =
    Stdio.print_endline ("collection: " ^ String.concat ~sep:", " collection)
  in
  let result =
    match bags with
    | [] -> collection
    | h :: tl -> (
        let () = Stdio.print_endline ("head: " ^ h) in
        let next = Map.find t h in
        match next with
        | None -> List.cons h (resolve tl collection t)
        | Some s ->
            (* let () = Stdio.print_endline (String.concat ~sep:", " s) in *)
            resolve (List.append s tl) (List.cons h collection) t )
  in
  List.dedup_and_sort result ~compare:String.compare

let result = resolve [ "shiny gold" ] [] tree

let () = Stdio.printf "%d\n" (List.length result - 1)
