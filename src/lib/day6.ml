open Base

(* modules *)
module Race = struct
  type t =
    { race_duration_in_milliseconds : int
    ; race_record_distance_in_millimeters : int
    }

  let compare r1 r2 =
    let duration_cpt =
      Int.compare r1.race_duration_in_milliseconds
        r2.race_duration_in_milliseconds in
    if duration_cpt <> 0 then duration_cpt
    else
      Int.compare r1.race_record_distance_in_millimeters
        r2.race_record_distance_in_millimeters

  let equal r1 r2 = compare r1 r2 = 0

  let all_possible_distances r =
    let rec helper acc last_distance press_time =
      let rem_time = r.race_duration_in_milliseconds - press_time in
      let distance = rem_time * press_time in
      if
        press_time >= r.race_duration_in_milliseconds
        || distance <= last_distance
           && distance <= r.race_record_distance_in_millimeters
      then acc
      else if distance > r.race_record_distance_in_millimeters then
        helper (distance :: acc) distance (press_time + 1)
      else helper acc distance (press_time + 1) in
    helper [] 0 1

  let how_many_possible_distance_breaking_prev_record r =
    r |> all_possible_distances |> List.length
end

(* parsing stuff *)
let parse rows : Race.t list =
  let times =
    rows |> List.hd_exn
    |> (fun r -> String.subo r ~pos:(String.length "Time:"))
    |> String.split ~on:' '
    |> List.filter_map ~f:Utils.Int.int_of_string_or_none in
  let distances =
    rows
    |> (fun l -> List.drop l 1)
    |> List.hd_exn
    |> (fun r -> String.subo r ~pos:(String.length "Distance:"))
    |> String.split ~on:' '
    |> List.filter_map ~f:Utils.Int.int_of_string_or_none in
  List.zip_exn times distances
  |> List.map ~f:(fun (t, d) ->
         { Race.race_duration_in_milliseconds = t
         ; Race.race_record_distance_in_millimeters = d
         })

exception Parse_exception

let parse_longer rows : Race.t =
  let opt_time =
    rows |> List.hd_exn
    |> (fun r -> String.subo r ~pos:(String.length "Time:"))
    |> String.split ~on:' '
    |> List.filter ~f:(fun x -> not @@ String.is_empty x)
    |> String.concat |> Utils.Int.int_of_string_or_none in
  let opt_dist =
    rows
    |> (fun l -> List.drop l 1)
    |> List.hd_exn
    |> (fun r -> String.subo r ~pos:(String.length "Distance:"))
    |> String.split ~on:' '
    |> List.filter ~f:(fun x -> not @@ String.is_empty x)
    |> String.concat |> Utils.Int.int_of_string_or_none in
  match (opt_time, opt_dist) with
  | None, _ | _, None -> raise Parse_exception
  | Some time, Some dist ->
      { Race.race_duration_in_milliseconds = time
      ; Race.race_record_distance_in_millimeters = dist
      }

(* testing *)
let race_1 =
  { Race.race_duration_in_milliseconds = 7
  ; Race.race_record_distance_in_millimeters = 9
  }

let race_2 =
  { Race.race_duration_in_milliseconds = 15
  ; Race.race_record_distance_in_millimeters = 40
  }

let race_3 =
  { Race.race_duration_in_milliseconds = 30
  ; Race.race_record_distance_in_millimeters = 200
  }

let test_races = [ race_1; race_2; race_3 ]

let longer_race =
  { Race.race_duration_in_milliseconds = 71530
  ; Race.race_record_distance_in_millimeters = 940200
  }

let%test "Parse races" =
  let races_row = [ "Time:      7  15   30"; "Distance:  9  40  200" ] in
  List.equal Race.equal test_races @@ parse races_row

let%test "All possible distances" =
  let expected_distances_breaking_record = [ 10; 12; 12; 10 ] in
  List.equal Int.equal expected_distances_breaking_record
  @@ Race.all_possible_distances race_1

let%test "Breaking record number (1)" =
  Race.how_many_possible_distance_breaking_prev_record race_1 = 4

let%test "Breaking record number (2)" =
  Race.how_many_possible_distance_breaking_prev_record race_2 = 8

let%test "Breaking record number (3)" =
  Race.how_many_possible_distance_breaking_prev_record race_3 = 9

let%test "Parse longer" =
  let races_row = [ "Time:      7  15   30"; "Distance:  9  40  200" ] in
  Race.equal longer_race @@ parse_longer races_row

(* main *)
let main rows =
  try
    let part_1 =
      rows |> parse
      |> List.map ~f:Race.how_many_possible_distance_breaking_prev_record
      |> List.fold ~init:1 ~f:( * ) in
    let part_2 =
      rows |> parse_longer |> Race.how_many_possible_distance_breaking_prev_record
    in
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
    Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2
  with
  | Parse_exception -> Stdio.print_endline "Could not parse the given file for day 6."
