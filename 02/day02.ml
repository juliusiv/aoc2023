let num_red_cubes = 12
let num_green_cubes = 13
let num_blue_cubes = 14

let part1 lines =
  let parse_line_into_picks line = line
    (* remove the game label *)
    |> String.split_on_char ':'
    |> Core.List.last_exn
    |> String.split_on_char ';'
    |> List.map (String.split_on_char ',')
    |> List.flatten
    |> List.map String.trim
  in
  let is_game_possible picks =
    let is_pick_possible cube_count_str =
      let split = String.split_on_char ' ' cube_count_str in
      match split with
      | n :: ["red"] ->  Stdlib.int_of_string @@ n <= num_red_cubes
      | n :: ["blue"] ->  Stdlib.int_of_string @@ n <= num_blue_cubes
      | n :: ["green"] -> Stdlib.int_of_string @@ n <= num_green_cubes
      | _ -> true
    in
    List.for_all is_pick_possible picks
  in
  lines
  |> List.map parse_line_into_picks
  |> List.mapi (fun i picks -> if is_game_possible picks then (i+1) else 0)
  |> List.fold_left (+) 0;;

let part2 lines =
  List.hd lines;;

let () =
  let lines = Core.In_channel.read_lines "input.txt" in
  let part1_soln = part1 lines in
  let part2_soln = part2 lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %s\n" part2_soln;;

(* dune exec --display quiet --no-print-directory ./day02 *)