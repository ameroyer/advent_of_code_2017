let input = [| 4; 1; 15; 12; 0; 9; 9; 5; 5; 8; 7; 3; 14; 5; 12; 3 |]


(*Returns argmax and max of an Array*)
let argmax arr =
  let max = ref 0 in
  let arg = ref 0 in
  for i=0 to (Array.length arr - 1) do
    if arr.(i) > !max then
      begin
	max := arr.(i);
	arg := i;
      end
  done;
  !arg, !max;;

(*Redistribute blocks in the memory banks*)
let redistribute_blocks arr =
  let start, num_blocks = argmax arr in
  let num_banks = Array.length arr in
  let new_arr = Array.make num_banks 0 in
  let base_num_blocks = num_blocks / num_banks and add_num_blocks = num_blocks mod num_banks in
  for i=0 to (Array.length arr - 1) do
    new_arr.(i) <- base_num_blocks + (if i = start then 0 else arr.(i));
    if (i > start && (i - start) <= add_num_blocks) || (i < start && (num_banks + i - start) <= add_num_blocks) then
      new_arr.(i) <- new_arr.(i) + 1;
    done;
  new_arr;;

(*Check if element x appears in list l and returns the distnce between the two repetition indices*)
let rec detect_loop l = match l with
  | [] -> -1
  | [t] -> -1
  | t::s::q -> if t = s then 1 else let index = detect_loop (t::q) in if index < 0 then index else index + 1;;

(*Find first duplicate in memory allocation*)
let find_loop_start input =
  let history = ref [input] in
  let loop_index = ref (detect_loop !history) in
  while !loop_index < 0 do
    history := (redistribute_blocks (List.hd !history))::!history;
    loop_index := detect_loop !history;
  done;
  List.length !history - 1, !loop_index;;


(*Main*)
(*ocamlopt -o day6 day06.ml; ./day6*)
let () =
  let num_steps, l = find_loop_start input in
  print_endline (Printf.sprintf "Number of steps to find a loop: %d" num_steps);
  print_endline (Printf.sprintf "Length of the detected loop: %d" l);
