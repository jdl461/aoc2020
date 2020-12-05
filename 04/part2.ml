open Core

let passports = [
"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980";
"hcl:#623a2f";
"";
"eyr:2029 ecl:blu cid:129 byr:1989";
"iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm";
"";
"hcl:#888785";
"hgt:164cm byr:2001 iyr:2015 cid:88";
"pid:545766238 ecl:hzl";
"eyr:2022";
"";
"iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";
"";
"eyr:1972 cid:100";
"hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926";
"";
"iyr:2019";
"hcl:#602927 eyr:1967 hgt:170cm";
"ecl:grn pid:012533040 byr:1946";
"";
"hcl:dab227 iyr:2012";
"ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277";
"";
"hgt:59cm ecl:zzz";
"eyr:2038 hcl:74454a iyr:2023";
"pid:3556412378 byr:2007";
""
]

(* let input = In_channel.read_lines "input.txt" *)
let input = passports

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

let parse_field field = 
  let kv = String.split field ~on:':' in
  match kv with
  | h :: t :: [] -> Some (h, t)
  | _ -> None

let valid_height h = 
  let matches = Re2.get_matches_exn (Re2.create_exn "(\\d+)(cm|in)") h in
  match matches with
  | [] -> false
  | hd :: _ -> 
    let u = Re2.Match.get_exn ~sub:(`Index 2) hd in
    let v = Re2.Match.get_exn ~sub:(`Index 1) hd in
    match u with
    | "cm" -> let intval = Int.of_string v in (intval >= 150 && intval <= 193)
    | "in" -> let intval = Int.of_string v in (intval >= 59 && intval <= 76)
    | _ -> false


let is_valid (key, value) = 
  let () = Stdio.printf "%s %s \n" key value in
  match key with
  | "byr" -> let intval = Int.of_string value in (intval >= 1920 && intval <= 2002)
  | "ecl" -> List.exists ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth";] ~f:(String.equal value)
  | "eyr" -> let intval = Int.of_string value in (intval >= 2020 && intval <= 2030)
  | "hcl" -> Re2.matches (Re2.create_exn "#[0-9|a-f]{6}") value
  | "hgt" -> valid_height value
  | "iyr" -> let intval = Int.of_string value in (intval >= 2010 && intval <= 2020)
  | "pid" -> Re2.matches (Re2.create_exn "\\d{9}") value
  | _  -> true

let is_present (records : (string * string) list) : bool = 
  let keys = List.map records ~f:(fun (a,_) -> a) in
  let sorted = List.sort keys ~compare:(String.compare) in
  let filtered = List.filter sorted ~f:(fun key -> not ((String.equal "cid" key))) in
  let keys_ok = List.equal (fun a b -> (String.compare a b) = 0) filtered ["byr"; "ecl"; "eyr"; "hcl"; "hgt"; "iyr"; "pid";] in
  let values_ok = List.map records ~f:(is_valid) in
  (List.fold values_ok ~init:true ~f:(&&)) && keys_ok

let results = 
  let passport_data = parse_input input [] "" in
  let passport_fields raw_passport = String.split raw_passport ~on:';' in
  let fields = List.map passport_data ~f:passport_fields in
  let parsed = List.map fields ~f:(fun fields -> List.map fields ~f:parse_field) in
  let filtered = List.map parsed ~f:(fun x -> List.filter_map x ~f:(ident)) in
  List.map filtered ~f:is_present

let count = List.length (List.filter results ~f:(ident))

let () = Stdio.print_endline (Int.to_string count)
