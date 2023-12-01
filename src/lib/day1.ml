open Base
open Option.Let_syntax

(* Part 1 *)
let make_int_from_acc acc =
  try Int.of_string @@ String.of_char_list [List.hd_exn acc; List.last_exn acc]
  with _ -> 0

let%test "Make int from acc (1)" = make_int_from_acc ['1'; '2'] = 12
let%test "Make int from acc (2)" = make_int_from_acc [] = 0
let%test "Make int from acc (3)" = make_int_from_acc ['1'] = 11

let get_calibration row =
  let rec helper acc cs =
    match cs with
    | [] -> List.rev acc |> make_int_from_acc
    | (x::xs) when Char.is_digit x -> helper (x::acc) xs
    | (_::xs) -> helper acc xs in
  helper [] @@ String.to_list row

let%test "Calibration (1)" = get_calibration "a1b2c3d4e5f" = 15
let%test "Calibration (2)" = get_calibration "treb7uchet" = 77

(* Part 2 *)
let written_numbers =
  [ ("one", 1)
  ; ("two", 2)
  ; ("three", 3)
  ; ("four", 4)
  ; ("five", 5)
  ; ("six", 6)
  ; ("seven", 7)
  ; ("eight", 8)
  ; ("nine", 9)
  ]

let parse_written_number x =
  let%bind (_, n) = List.find ~f:(fun (wn, _) -> String.is_prefix ~prefix:wn x) written_numbers in
  Some n

let%test "Parsing written number (1)" = Option.equal Int.equal (parse_written_number "nine") @@ Some 9
let%test "Parsing written number (2)" = Option.equal Int.equal (parse_written_number "eight0z") @@ Some 8
let%test "Parsing written number (3)" = Option.equal Int.equal (parse_written_number "tone0") @@ None

let take_head_off s =
  try String.sub ~pos:1 ~len:(String.length s - 1) s
  with _ -> s

let%test "Take head off (1)" = String.equal (take_head_off "") "" 
let%test "Take head off (1)" = String.equal (take_head_off "a") "" 
let%test "Take head off (2)" = String.equal (take_head_off "abc") "bc" 

let get_calibration_bis row =
  let rec helper acc s =
    if String.is_empty s then List.rev acc |> make_int_from_acc
    else
      let s' = take_head_off s in
      match parse_written_number s with
      | Some x -> helper ((Int.to_string x |> (fun ix -> String.get ix 0))::acc) s'
      | None ->
        let h = String.get s 0 in
        if Char.is_digit h
          then helper (h::acc) s'
          else helper acc s'
      in
  helper [] row

let%test "Calibration bis (1)" = get_calibration_bis "two1nine" = 29
let%test "Calibration bis (2)" = get_calibration_bis "eightwothree" = 83
let%test "Calibration bis (3)" = get_calibration_bis "7pqrstsixteen" = 76
let%test "Calibration bis (4)" = get_calibration_bis "abcone2threexyz" = 13
let%test "Calibration bis (5)" = get_calibration_bis "eighthree" = 83

let main rows =
  let part1 = rows
    |> List.fold ~init:0 ~f:(fun acc x -> acc + get_calibration x)
    |> Printf.sprintf "Part 1: %d" in
  let part2 = rows
    |> List.fold ~init:0 ~f:(fun acc x -> acc + get_calibration_bis x)
    |> Printf.sprintf "Part 2: %d" in
  Stdio.print_endline part1;
  Stdio.print_endline part2;
