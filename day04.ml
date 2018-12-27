(*Parse the input file into a list of list of strings*)
let parse_file path =
  let file = open_in path in
  let lines = ref [] in
  try
    while true; do
      let line = input_line file in
      lines := (Str.split (Str.regexp " ") line) :: !lines
    done; []
  with End_of_file ->
    close_in file;
    List.rev !lines;;


(*Check if element x appears in l*)
let rec is_in x l = match l with
  | [] -> false
  | t::q -> if t = x then true else is_in x q;;

(*Check if list l contains duplicates*)
let rec has_duplicates f l = match l with
  | [] -> false
  | t::q -> if f t q then true else has_duplicates f q;;

(*Counts number of valid passphrases based on test in function f*)
let rec num_passphrases f l = match l with
  | [] -> 0
  | t::q -> let res = if has_duplicates f t then 0 else 1 in res + num_passphrases f q;;


(*Check if x is an anagram of y*)
let sort_string s =
  let list = Str.split (Str.regexp "") s in
  List.sort compare list;;

let is_anagram x y =
  sort_string x = sort_string y;;

(*Check if element x appears as an anagram in l*)
let rec is_anagram_in x l = match l with
  | [] -> false
  | t::q -> if is_anagram t x then true else is_anagram_in x q;;


(*Main*)
(*ocamlopt -o day4 str.cmxa day04.ml; ./day4*)
let () =
  let data = parse_file "inputs/day4.txt" in
  let result = num_passphrases is_in data in
  print_endline (Printf.sprintf "Number of valid passphrases: %d" result);
  let result2 = num_passphrases is_anagram_in data in
  print_endline (Printf.sprintf "Number of valid passphrases: %d" result2);
