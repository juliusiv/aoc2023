(* open Re *)

let is_numeric = function '0' .. '9' -> true | _ -> false;;

let extract_calibration_value line =
  let numeric_chars = String.to_seq line
    |> List.of_seq
    |> List.filter is_numeric
    |> List.map (String.make 1) in
  let first_numeric = List.hd numeric_chars |> Stdlib.int_of_string in
  let last_numeric = List.rev numeric_chars |> List.hd |> Stdlib.int_of_string in
  10 * first_numeric + last_numeric;;

let part1 lines =
  let calibration_values = List.map extract_calibration_value lines in
  List.fold_left (+) 0 calibration_values;;

(* splitting these out just makes things a bit less cluttered *)
let one_re = Re.compile @@ Re.str "one";;
let two_re = Re.compile @@ Re.str "two";;
let three_re = Re.compile @@ Re.str "three";;
let four_re = Re.compile @@ Re.str "four";;
let five_re = Re.compile @@ Re.str "five";;
let six_re = Re.compile @@ Re.str "six";;
let seven_re = Re.compile @@ Re.str "seven";;
let eight_re = Re.compile @@ Re.str "eight";;
let nine_re = Re.compile @@ Re.str "nine";;

let part2 lines =
  let replace_line line = line
    |> Re.replace_string one_re ~by:"one1one"
    |> Re.replace_string two_re ~by:"two2two"
    |> Re.replace_string three_re ~by:"three3three"
    |> Re.replace_string four_re ~by:"four4four"
    |> Re.replace_string five_re ~by:"five5five"
    |> Re.replace_string six_re ~by:"six6six"
    |> Re.replace_string seven_re ~by:"seven7seven"
    |> Re.replace_string eight_re ~by:"eight8eight"
    |> Re.replace_string nine_re ~by:"nine9nine"
  in
  let calibration_values =
    List.map replace_line lines |>
    List.map extract_calibration_value in
  List.fold_left (+) 0 calibration_values;;

let () =
  let lines = Core.In_channel.read_lines "input.txt" in
  let part1_soln = part1 lines in
  let part2_soln = part2 lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;;


(* dune exec --display quiet --no-print-directory ./day01.exe *)