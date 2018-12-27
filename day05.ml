(*Parse the input file into a list of integers*)
let parse_file path =
  let file = open_in path in
  let lines = ref [] in
  try
    while true; do
      let line = input_line file in
      lines := (int_of_string line) :: !lines
    done; []
  with End_of_file ->
    close_in file;
    List.rev !lines;;


(*Follow the Part 1 instructions jump until reaching the end of the list*)
let jump arr =
  let counter = ref 0 in
  let num_steps = ref 0 in
  try
    while true do
      begin
	arr.(!counter) <- arr.(!counter) + 1;
	counter := !counter + arr.(!counter) - 1;
	num_steps := !num_steps + 1;
      end
    done; 0
  with Invalid_argument("index out of bounds") ->
    !num_steps;;


(*Follow the Part 2 instructions jump until reaching the end of the list*)
let jump2 arr =
  let counter = ref 0 in
  let num_steps = ref 0 in
  try
    while true do
      begin
	if arr.(!counter) >= 3 then
	  begin
	    arr.(!counter) <- arr.(!counter) - 1;
	    counter := !counter + arr.(!counter) + 1;
	  end
	else
	  begin
	    arr.(!counter) <- arr.(!counter) + 1;
	    counter := !counter + arr.(!counter) - 1;
	  end;
	num_steps := !num_steps + 1;
      end
    done; 0
  with Invalid_argument("index out of bounds") ->
    !num_steps;;


(*Main*)
(*ocamlopt -o day5 str.cmxa day05.ml; ./day5*)
let () =
  let data = Array.of_list (parse_file "inputs/day5.txt") in
  let result = jump data in
  print_endline (Printf.sprintf "Number of steps required (part 1): %d" result);
  let data = Array.of_list (parse_file "inputs/day5.txt") in
  let result2 = jump2 data in
  print_endline (Printf.sprintf "Number of steps required (part 2): %d" result2);;
