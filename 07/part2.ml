open Core

let ex =
  [
    "shiny gold bags contain 2 dark red bags.";
    "dark red bags contain 2 dark orange bags.";
    "dark orange bags contain 2 dark yellow bags.";
    "dark yellow bags contain 2 dark green bags.";
    "dark green bags contain 2 dark blue bags.";
    "dark blue bags contain 2 dark violet bags.";
    "dark violet bags contain no other bags.";
  ]

let re = Re2.create_exn "^(.+) bags contain (.+)+\\.+$"

let re2 = Re2.create_exn "(\\d) (.+) (bag|bags)"

let input = In_channel.read_lines "input.txt"

(* let input = ex *)

let bag_rules = List.map input ~f:(Re2.get_matches_exn re)

let print_contents container contents =
  Stdio.print_endline
    ( "container: " ^ container ^ ", contents: "
    ^ String.concat ~sep:", "
        (List.map contents ~f:(fun (s, c) -> s ^ " x " ^ Int.to_string c)) )

let bag_contents contents =
  List.map contents ~f:(fun rule ->
      match rule with
      | "no other bags" -> ("", 0)
      | _ ->
          let matches = Re2.get_matches_exn re2 rule in
          let count =
            Int.of_string
              (Re2.Match.get_exn ~sub:(`Index 1) (List.hd_exn matches))
          in
          let bag = Re2.Match.get_exn ~sub:(`Index 2) (List.hd_exn matches) in
          (bag, count))

let tree =
  List.fold bag_rules ~init:[] ~f:(fun acc rule ->
      match List.hd rule with
      | None -> acc
      | Some r ->
          let container = Re2.Match.get_exn ~sub:(`Index 1) r in
          let contents =
            String.split ~on:','
              (Re2.Match.get_exn ~sub:(`Index 2) (List.hd_exn rule))
          in
          let contents = List.map contents ~f:String.strip in
          let contents = bag_contents contents in
          let contents =
            List.filter contents ~f:(fun (s, _) -> not (String.is_empty s))
          in
          (* let () = print_contents container contents in *)
          List.Assoc.add ~equal:String.equal acc container contents)

let t_to_string (s, c) = s ^ " x " ^ Int.to_string c

(* let () =
  List.iter tree ~f:(fun (s, c) ->
      Stdio.printf "%s: %s\n" s
        (String.concat ~sep:", " (List.map c ~f:t_to_string))) *)

let find_in_t bag t =
  let contents = List.Assoc.find ~equal:String.equal t bag in
  match contents with None -> [] | Some x -> x

let rec count_bags bags t =
  match bags with
  | [] -> 0
  | _ ->
      let counts =
        List.map bags ~f:(fun (bag, count) ->
            let children = List.Assoc.find ~equal:String.equal t bag in
            match children with
            | None -> count
            | Some lst -> count + (count * count_bags lst t))
      in
      List.fold counts ~init:0 ~f:( + )

(* let children = List.Assoc.find ~equal:String.equal t bag in
   match children with
   | None -> 0
   | Some lst ->
       List.fold lst ~init:count ~f:(fun acc (b, c) ->
           acc + (acc * count_bag (b, c) t)) *)

let result =
  count_bags (List.Assoc.find_exn ~equal:String.equal tree "shiny gold") tree

let () = Stdio.printf "%d\n" result
