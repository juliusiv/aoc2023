let is_digit = function '0' .. '9' -> true | _ -> false;;
let is_symbol ch =
  match ch with
  | '.' -> false
  | ch when is_digit ch -> false
  | _ -> true;;

let rec parse_numbers_and_symbols ch_list curr_num row col num_list symbol_list =
  let num_pos = ((row - 1, col - 1- String.length curr_num), (row + 1, col)) in
  let new_num_list = match curr_num with
    | "" -> num_list
    | _ -> (curr_num, num_pos) :: num_list
  in
  match ch_list, curr_num with
  | [], _ -> num_list, symbol_list
  | '\n' :: rest, _ ->
    parse_numbers_and_symbols rest "" (row + 1) 0 new_num_list symbol_list
  | digit :: rest, _ when is_digit digit ->
    let num = curr_num ^ String.make 1 digit in
    parse_numbers_and_symbols rest num row (col + 1) num_list symbol_list
  | symbol :: rest, _ when is_symbol symbol ->
    let new_symbol_list = (symbol, (row, col)) :: symbol_list in
    parse_numbers_and_symbols rest "" row (col + 1) new_num_list new_symbol_list
  | _ :: rest, _ -> parse_numbers_and_symbols rest "" row (col + 1) new_num_list symbol_list
;;

let explode str = str |> String.to_seq |> List.of_seq;;
let is_adjacent (_, ((rl, ct), (rr, cb))) (_, (r, c)) =
  rl <= r && rr >= r && ct <= c && cb >= c
;;

let part1 ch_list =
  let num_list, symbol_list = parse_numbers_and_symbols ch_list "" 0 0 [] [] in
  let has_adjacent num =
    let adj = List.find_opt (fun s -> is_adjacent num s) symbol_list in
    match adj with
    | None -> false
    | _ -> true
  in
  let adjacent_numbers = num_list
    |> List.filter (fun n -> has_adjacent n)
    |> List.map (fun (n, _) -> Stdlib.int_of_string n)
  in
  List.fold_left (+) 0 adjacent_numbers;;

let part2 ch_list =
  let num_list, symbol_list = parse_numbers_and_symbols ch_list "" 0 0 [] [] in
  let rec find_gear_ratio symbol numbers adjacents_found =
    match symbol, numbers, adjacents_found with
    | (s, _), _, _ when s <> '*' -> 0
    | _, [], [n1; n2] -> Stdlib.int_of_string n1 * Stdlib.int_of_string n2
    | _, [], _ -> 0
    | _, num :: rest, _ when is_adjacent num symbol -> find_gear_ratio symbol rest (fst num :: adjacents_found)
    | _,( _ :: rest), _ -> find_gear_ratio symbol rest adjacents_found
  in
  let gear_ratios = List.map (fun s -> find_gear_ratio s num_list []) symbol_list in
  List.fold_left (+) 0 gear_ratios
;;


let () =
  (* There's gotta be a way to go over the file character by character instead of doing this... *)
  let lines = Core.In_channel.read_lines "input.txt" |> Base.String.concat ~sep:"\n" in
  let part1_soln = part1 @@ explode lines in
  let part2_soln = part2 @@ explode lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;;

(* dune exec --no-print-directory ./day03 *)