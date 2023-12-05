open Base

type rule =
  { dest_range_start : int; source_range_start : int; range_length : int }

let rule_equal r1 r2 =
  Int.equal r1.dest_range_start r2.dest_range_start
  && Int.equal r1.source_range_start r2.source_range_start
  && Int.equal r1.range_length r2.range_length

let rule_apply rule ~x =
  if
    x >= rule.source_range_start
    && x < rule.source_range_start + rule.range_length
  then
    let y = rule.dest_range_start + (x - rule.source_range_start) in
    Some y
  else None

let%test "Rule apply (1)" =
  let res =
    rule_apply ~x:79
      { dest_range_start = 50; source_range_start = 98; range_length = 2 } in
  Option.equal Int.equal res None

let%test "Rule apply (2)" =
  let res =
    rule_apply ~x:79
      { dest_range_start = 52; source_range_start = 50; range_length = 48 }
  in
  Option.equal Int.equal res @@ Some 81

let from_x_to_y convertion_rules_for_x_to_y x =
  convertion_rules_for_x_to_y
  |> List.find_map ~f:(rule_apply ~x)
  |> Option.value ~default:x

let%test "From x to y" =
  let conversion_rules =
    [ { dest_range_start = 50; source_range_start = 98; range_length = 2 }
    ; { dest_range_start = 52; source_range_start = 50; range_length = 48 }
    ] in
  from_x_to_y conversion_rules 79 = 81
  && from_x_to_y conversion_rules 14 = 14
  && from_x_to_y conversion_rules 55 = 57
  && from_x_to_y conversion_rules 99 = 51

let from_x_to_loc convertion_rule_set x =
  convertion_rule_set
  |> List.fold ~init:x ~f:(fun prev_x rule_set -> from_x_to_y rule_set prev_x)

type context = { seeds : int list; conversion_rules_set : rule list list }

let parse_group group_of_rules_raw : rule list =
  group_of_rules_raw
  |> List.filter_map ~f:(fun rule_raw ->
         match
           rule_raw |> String.split ~on:' '
           |> List.filter_map ~f:Utils.Int.int_of_string_or_none
         with
         | dest_range_start :: source_range_start :: range_length :: _ ->
             Some { dest_range_start; source_range_start; range_length }
         | _ -> None)

let%test "Group of rules parsing" =
  let expected =
    [ { dest_range_start = 50; source_range_start = 98; range_length = 2 }
    ; { dest_range_start = 52; source_range_start = 50; range_length = 48 }
    ] in
  let result = parse_group [ ""; "seeds-to-soil-map:"; "50 98 2"; "52 50 48" ] in
  List.equal rule_equal expected result

let parse rows =
  let seeds =
    rows |> List.hd_exn
    |> String.subo ~pos:(String.length "seeds: ")
    |> String.split ~on:' '
    |> List.filter_map ~f:Utils.Int.int_of_string_or_none in
  let conversion_rules_set =
    rows
    |> (fun l -> List.drop l 1)
    |> List.group ~break:(fun _ next -> String.is_empty next)
    |> List.map ~f:parse_group in
  { seeds; conversion_rules_set }

let solve context =
  context.seeds |> List.map ~f:(from_x_to_loc context.conversion_rules_set)

let seeds_range_from_context context : (int * int) list =
  let rec helper acc rem_seeds =
    match rem_seeds with
    | [] | [ _ ] -> acc
    | x :: y :: xs -> helper ((x, y)::acc) xs in
  helper [] context.seeds

(* let solve_with_range context =
  context |> seeds_range_from_context |> failwith "TODO" *)

let main rows =
  let context = parse rows in
  let part_1 = solve context |> List.min_elt ~compare:Int.compare in
  (* let part_2 = solve_with_range context |> List.min_elt ~compare:Int.compare in *)
  let part_2 = Some 0 in
  match (part_1, part_2) with
  | None, _ | _, None -> Stdio.print_endline "No seeds to solve"
  | Some p1, Some p2 ->
      Stdio.print_endline @@ Printf.sprintf "Part 1: %d" p1;
      Stdio.print_endline @@ Printf.sprintf "Part 2: %d" p2
    
