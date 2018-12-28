let input = 368078

(*Find the ring the tile n will belong to*)
let find_ring n =
  let ring_index = ref 1 and ring_start_index = ref 2 and ring_length = ref 8 in
  while (!ring_start_index + !ring_length < input) do
      ring_start_index := !ring_start_index + !ring_length;
      ring_length := !ring_length + 8;
      ring_index := !ring_index + 1;
  done;
  !ring_index, !ring_start_index, !ring_length;;

(*absolute value*)
let abs x =
  if x > 0 then x else - x;;

(*Find the exact index of tile n in the snail board, and the L1 distance to origin*)
let find_carrying_weight n =
  let ring_index, ring_start_index, ring_length = find_ring n in
  let unit = ring_length / 4 in
  let position = (n - ring_start_index) mod unit - unit / 2 + 1  in
  abs(ring_index) + abs(position);;


(*Find the first tile that has a value bigger than the given input*)
let find_first_biggest_tile n =
  let current_ring = ref ([| 1; 2; 4; 5; 10; 11; 23; 25 |]) in
  let ring_length = ref 8 in
  let result = ref 0 in
  try
    while true do
      ring_length := !ring_length + 8;
      let break_point = !ring_length / 4 in
      let next_ring = Array.make !ring_length 0 in
      for i=0 to !ring_length - 1 do
	let side = i / break_point and position = i mod break_point in
	(*Match the different configurations whether the tile is a corner or not*)
	(*Tricky indexing happen for the few first and last tiles*)
	let v = if position = 0 then
		  if side = 0 then
		   !current_ring.(Array.length !current_ring - 1) + !current_ring.(i)
		  else
		   !current_ring.(i - side * 2 - 1) + !current_ring.(i - side * 2) + next_ring.(i - 1) + next_ring.(i - 2)
		 else if position = 1 then
		  if side = 0 then
		   !current_ring.(Array.length !current_ring - 1) + !current_ring.(i - 1) + !current_ring.(i) + next_ring.(i - 1)
		  else
		   !current_ring.(i - side * 2 - 2) + !current_ring.(i - side * 2 - 1) + !current_ring.(i - side * 2) + next_ring.(i - 1)
                 else if position = break_point - 2 then
		  if side = 3 then
		   !current_ring.(i - side * 2 - 2) + !current_ring.(i - side * 2 - 1) + next_ring.(i - 1) + next_ring.(0)
		  else
		   !current_ring.(i - side * 2 - 2) + !current_ring.(i - side * 2 - 1) + next_ring.(i - 1)
                 else if position = break_point - 1 then
		  if side = 3 then
		   !current_ring.(i - side * 2 - 2) + next_ring.(i - 1) + next_ring.(0)
		  else
		   !current_ring.(i - side * 2 - 2) + next_ring.(i - 1)
		 else
		   !current_ring.(i - side * 2 - 2) + !current_ring.(i - side * 2 - 1) + !current_ring.(i - side * 2) + next_ring.(i - 1)
	in
	next_ring.(i) <- v;
	if next_ring.(i) > n then
	  begin
	    result := next_ring.(i);
	    raise Exit
	  end;
      done;
      current_ring := next_ring;
    done; 0
  with Exit ->
    !result;;


(*Main*)
(*ocamlopt -o day3 day03.ml; ./day3*)
let () =
  let carrying_weight = find_carrying_weight input in
  print_endline (Printf.sprintf "Steps required to carry from the input tile: %d" carrying_weight);;
  let next_value = find_first_biggest_tile input in
  print_endline (Printf.sprintf "First largest value: %d" next_value);;
