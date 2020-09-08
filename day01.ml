let file = open_in "inputs/day1.txt"

(* Convert input string to a list of integers*)
let rec str_to_ints s = match s with
  | "" -> []
  | _ -> ((int_of_char (String.get s 0)) - 48) :: (str_to_ints (String.sub s 1 ((String.length s) - 1)));;

(*Separate a list*)
let rec split l i = match l with
  |[] -> 0, [], []
  |t::q -> let (pivot, left, right) = split q i in
	   if pivot >= i then (pivot, t::left, right) else (pivot + 1, left, t::right);;

(* Solve the Captcha of Part 1*)
let rec solve_captcha l head = match l with
  | [] -> 0
  | [t] -> if t == head then t else 0
  | t::(s::q) -> (if t == s then t else 0) + (solve_captcha (s::q) head);;


(* Solve the Captcha of Part 2*)
let rec solve_captcha2 l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], t::q -> -1
  | t::q, [] -> -1
  | t::q, s::r -> (if t == s then t else 0) + (solve_captcha2 q r);;


(*Main*)

(*ocamlopt -o day1 day01.ml; ./day1*)
let () =
  let file = open_in "inputs/day1.txt" in
  let data = str_to_ints (input_line file) in
  let result = solve_captcha data (List.hd data) in
  print_endline (Printf.sprintf "Captcha of Part 1: %d" result);

  let _, l1, l2 = split data ( (List.length data) / 2) in
  let result = solve_captcha2 data (l2@l1) in
  print_endline (Printf.sprintf "Captcha of Part 2: %d" result);
  close_in file;;
