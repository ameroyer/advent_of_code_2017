(*Parse the input file into a list of list of integers*)
let parse_file path =
  let file = open_in path in
  let lines = ref [] in
  try
    while true; do
      let line = input_line file in
      lines := (List.map int_of_string (Str.split (Str.regexp "\t") line)) :: !lines
    done; []
  with End_of_file ->
    close_in file;
    List.rev !lines;;


(*Get the minimum and maximum values of a list of integers*)
let rec get_bounds l = match l with
  | [] -> (10000000, 0)
  | t::q -> let min, max = get_bounds q in
	    (if t < min then t else min), (if t > max then t else max);;

(*Return the checksum of an int list list as defined in part 1*)
let rec checksum l = match l with
  | [] -> 0
  | t::q -> let min, max = get_bounds t in
	    (max - min) + checksum q;;


(*Given integer i, find the first integer in list l that evenly divides i.
Also returns the result of the division*)
let rec evenly_divides_int i l = match l with
  | [] -> false, 0
  | t::q -> let min, max = if t > i then (i, t) else (t, i) in
	    if max mod min = 0 then (true, max / min) else evenly_divides_int i q;;

(*Given list l, find the first pair of integers such that one evenly
divides the other *)
let rec evenly_divides_list l = match l with
  | [] -> 0
  | t::q -> let success, result = evenly_divides_int t q in
	    if success then result else evenly_divides_list q;;

(*Return the checksum of an int list list as defined in part 2*)
let rec checksum_div l = match l with
  | [] -> 0
  | t::q -> evenly_divides_list t + checksum_div q;;


(*Main*)
(*ocamlopt -o day2 str.cmxa day02.ml; ./day2*)
let () =
  let data = parse_file "inputs/day2.txt" in
  let result = checksum data in
  print_endline (Printf.sprintf "Checksum of input list: %d" result);
  let result2 = checksum_div data in
  print_endline (Printf.sprintf "Checksum_div of input list: %d" result2);;
