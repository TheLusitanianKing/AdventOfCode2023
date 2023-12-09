open Base

let parse_history row =
  row |> String.split ~on:' '
  |> List.filter_map ~f:Utils.Int.int_of_string_or_none
  |> List.to_array

let extrapolated_value history : int =
  let rec diff_until_all_zeros acc current =
    if current |> Array.for_all ~f:(Int.equal 0) then (0, acc)
    else if Array.length current = 1 then (current.(0), acc)
    else
      let next =
        Array.zip_exn
          (current |> Array.subo ~len:(Array.length current - 1))
          (current |> Array.subo ~pos:1)
        |> Array.map ~f:(fun (x, y) -> y - x) in
      diff_until_all_zeros (next :: acc) next in
  let i, xs = diff_until_all_zeros [ history ] history in
  xs |> List.rev
  |> List.fold ~init:i ~f:(fun last_added arr -> last_added + Array.last arr)

let main rows =
  let part_1 =
    rows
    |> List.map ~f:(fun x -> x |> parse_history |> extrapolated_value)
    |> List.fold ~init:0 ~f:( + ) in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1

(* testing *)
let histories =
  [ [ 0; 3; 6; 9; 12; 15 ]
  ; [ 1; 3; 6; 10; 15; 21 ]
  ; [ 10; 13; 16; 21; 30; 45 ]
  ]

let%test "Parsing" =
  let result = parse_history "-7 6 44 130" in
  let expected = [| -7; 6; 44; 130 |] in
  Array.equal Int.equal result expected

let%test "Predict histories" =
  let result =
    histories |> List.map ~f:(fun l -> l |> List.to_array |> extrapolated_value)
  in
  let expected = [ 18; 28; 68 ] in
  List.equal Int.equal result expected
