let num_red_cubes = 12
let num_green_cubes = 13
let num_blue_cubes = 14

let parse_line_into_picks line =
  let parse_pick_string_into_tuple pick_string =
    let split = String.split_on_char ' ' pick_string in
    let count = Stdlib.int_of_string @@ List.hd split in
    let color = Core.List.last_exn split in
    (count, color)
  in
  line
  (* remove the game label *)
  |> String.split_on_char ':'
  |> Core.List.last_exn
  |> String.split_on_char ';'
  |> List.map (String.split_on_char ',')
  |> List.flatten
  |> List.map String.trim
  |> List.map parse_pick_string_into_tuple;;

let part1 lines =
  let is_game_possible picks =
    let is_pick_possible pick =
      match pick with
      | (count, "red") ->  count <= num_red_cubes
      | (count, "blue") ->  count <= num_blue_cubes
      | (count, "green") -> count <= num_green_cubes
      | _ -> true
    in
    List.for_all is_pick_possible picks
  in
  lines
  |> List.map parse_line_into_picks
  |> List.mapi (fun i picks -> if is_game_possible picks then (i+1) else 0)
  |> List.fold_left (+) 0;;

let part2 lines =
  let color_max picks color =
    picks
    |> List.filter_map (fun (n, c) -> if c = color then Some n else None)
    |> List.fold_left max 0
  in
  let game_power picks =
    let red_max = color_max picks "red" in
    let blue_max = color_max picks "blue" in
    let green_max = color_max picks "green" in
    red_max * blue_max * green_max
  in
  lines
  |> List.map parse_line_into_picks
  |> List.map game_power
  |> List.fold_left (+) 0;;

let () =
  (* let lines = Core.In_channel.read_lines "input-1-test.txt" in *)
  let lines = Core.In_channel.read_lines "input.txt" in
  let part1_soln = part1 lines in
  let part2_soln = part2 lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;;

(* dune exec --display quiet --no-print-directory ./day02 *)