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

let rule_apply_direct rule ~x =
  rule.dest_range_start + (x - rule.source_range_start)

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

(* returns (intersected option, out_of_range)
   intersected being the part of the range where the rule applies, out_of_range the rest *)
let intersect_rule rule ~range:(x1, y1) : (int * int) option * (int * int) list
    =
  let x2, y2 =
    (rule.source_range_start, rule.source_range_start + rule.range_length - 1) in
  let start = max x1 x2 in
  let stop = min y1 y2 in
  if stop < start then (None, [ (x1, y1) ])
  else
    let out_of_range =
      List.filter_opt
        [ (if x1 < start then Some (x1, start - 1) else None)
        ; (if y1 > stop then Some (stop + 1, y1) else None)
        ] in
    (Some (start, stop), out_of_range)

let%test "Intersect rule (1)" =
  let rule =
    { dest_range_start = 50; source_range_start = 98; range_length = 2 } in
  let range = (95, 98) in
  Utils.Tuple.equal_tuple
    (Option.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (List.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (Some (98, 98), [ (95, 97) ])
  @@ intersect_rule rule ~range

let%test "Intersect rule (2)" =
  let rule =
    { dest_range_start = 50; source_range_start = 98; range_length = 2 } in
  let range = (98, 99) in
  Utils.Tuple.equal_tuple
    (Option.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (List.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (Some (98, 99), [])
  @@ intersect_rule rule ~range

let%test "Intersect rule (3)" =
  let rule =
    { dest_range_start = 50; source_range_start = 98; range_length = 2 } in
  let range = (50, 52) in
  Utils.Tuple.equal_tuple
    (Option.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (List.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (None, [ (50, 52) ])
  @@ intersect_rule rule ~range

let rule_apply_range rule ~range =
  let opt_intersect, out_of_range = intersect_rule rule ~range in
  match opt_intersect with
  | None -> (None, out_of_range)
  | Some (x, y) ->
      ( Some (rule_apply_direct rule ~x, rule_apply_direct rule ~x:y)
      , out_of_range )

let from_x_to_y conversion_rules_for_x_to_y x =
  conversion_rules_for_x_to_y
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

let from_x_ranges_to_y_ranges conversion_rules (x_ranges : (int * int) list) :
    (int * int) list =
  let rec helper acc ranges rules_to_apply =
    match (ranges, rules_to_apply) with
    | [], _ -> acc
    | rs, [] -> List.append acc rs
    | _, r :: rs ->
        let applies_rules_to_all_range =
          ranges |> List.map ~f:(fun range -> rule_apply_range r ~range) in
        let already_applied =
          applies_rules_to_all_range |> List.filter_map ~f:(fun (opt, _) -> opt)
        in
        let to_be_applied_yet =
          applies_rules_to_all_range
          |> List.concat_map ~f:(fun (_, out_of_range) -> out_of_range) in
        helper (List.append acc already_applied) to_be_applied_yet rs in
  helper [] x_ranges conversion_rules

(* This test is meh, should be comparing sets and not list but doesn't matter much anyway *)
let%test "From x-ranges to y-ranges" =
  let conversion_rules =
    [ { dest_range_start = 50; source_range_start = 98; range_length = 2 }
    ; { dest_range_start = 92; source_range_start = 90; range_length = 2 }
    ] in
  let x_ranges = [ (1, 3); (90, 98) ] in
  let expected_y_ranges = [ (50, 50); (92, 94); (1, 3); (93, 97) ] in
  List.equal (Utils.Tuple.equal_tuple' Int.compare) expected_y_ranges
  @@ from_x_ranges_to_y_ranges conversion_rules x_ranges

let from_x_to_loc conversion_rule_set x =
  conversion_rule_set
  |> List.fold ~init:x ~f:(fun prev_x rule_set -> from_x_to_y rule_set prev_x)

let from_x_ranges_to_loc conversion_rule_set x_ranges =
  conversion_rule_set
  |> List.fold ~init:x_ranges ~f:(fun prev_x_ranges rule_set ->
         from_x_ranges_to_y_ranges rule_set prev_x_ranges)

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
    | x :: y :: xs -> helper ((x, x + y - 1) :: acc) xs in
  helper [] context.seeds

let solve_with_range context =
  seeds_range_from_context context
  |> List.concat_map ~f:(fun x_range ->
         from_x_ranges_to_loc context.conversion_rules_set [ x_range ])

let main rows =
  let context = parse rows in
  let part_1 = solve context |> List.min_elt ~compare:Int.compare in
  let part_2 =
    solve_with_range context
    |> List.concat_map ~f:(fun (x, y) -> [ x; y ])
    |> List.min_elt ~compare:Int.compare in
  match (part_1, part_2) with
  | None, _ | _, None -> Stdio.print_endline "No seeds to solve"
  | Some p1, Some p2 ->
      Stdio.print_endline @@ Printf.sprintf "Part 1: %d" p1;
      Stdio.print_endline @@ Printf.sprintf "Part 2: %d" p2
