open Core

let input = In_channel.read_lines "input.txt"

let add_to_passport data passport = 
  (String.substr_replace_all data ~pattern:" " ~with_:";") ^ ";" ^ passport

let rec parse_input lst acc ppt = 
  match lst with
  | [] -> acc
  | h :: [] -> (
    match h with 
    | "" -> List.cons ppt acc
    | _ -> List.cons (add_to_passport h ppt) acc
  )
  | h :: t -> 
    (
    match h with 
    | "" -> parse_input t (List.cons ppt acc) ""
    | _ -> parse_input t acc (add_to_passport h ppt)
  )

let passport_data = parse_input input [] ""

(* let () = Stdio.print_endline (String.concat ~sep:"\n" passport_data) *)

let parse_field field = 
  let kv = String.split field ~on:':' in
  match kv with
  | h :: t :: [] -> Some (h, t)
  | _ -> None

let passport_fields raw_passport = String.split raw_passport ~on:';'

let is_valid (records : (string * string) list) : bool = 
  let keys = List.map records ~f:(fun (a,_) -> a) in
  let sorted = List.sort keys ~compare:(String.compare) in
  let filtered = List.filter sorted ~f:(fun key -> not ((String.compare "cid" key) = 0)) in
  (* let () = Stdio.print_endline (String.concat ~sep:"|" filtered) in *)
  List.equal (fun a b -> (String.compare a b) = 0) filtered ["byr"; "ecl"; "eyr"; "hcl"; "hgt"; "iyr"; "pid";]

let results = 
let fields = List.map passport_data ~f:passport_fields in
let parsed = List.map fields ~f:(fun fields -> List.map fields ~f:parse_field) in
let filtered = List.map parsed ~f:(fun x -> List.filter_map x ~f:(ident)) in
List.map filtered ~f:is_valid

let count = List.length (List.filter results ~f:(ident))

let () = Stdio.print_endline (Int.to_string count)
