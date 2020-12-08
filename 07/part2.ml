open Core

let re = Re2.create_exn "^(.+) bags contain (.+)+\\.+$"

let re2 = Re2.create_exn "(\\d) (.+) (bag|bags)"

let input = In_channel.read_lines "input.txt"

let bag_rules = List.map input ~f:(Re2.get_matches_exn re)

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

let result =
  count_bags (List.Assoc.find_exn ~equal:String.equal tree "shiny gold") tree

let () = Stdio.printf "%d\n" result
