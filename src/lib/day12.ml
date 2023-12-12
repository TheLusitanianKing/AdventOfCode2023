open Base

module Spring = struct
  type t = Operational | Damaged | Unknown [@@deriving eq]

  exception Parse_exception

  let parse = function
    | '.' -> Operational
    | '#' -> Damaged
    | '?' -> Unknown
    | _ -> raise Parse_exception
end

module Condition_records = struct
  type t = { springs : Spring.t list; contiguous_group_damaged : int list }
  [@@deriving eq]

  exception Parse_exception

  let parse row =
    row |> String.split ~on:' ' |> function
    | [] | [ _ ] -> raise Parse_exception
    | springs_raw :: contiguous_groups_raw :: _ ->
        let springs =
          springs_raw |> String.to_list |> List.map ~f:Spring.parse in
        let contiguous_group_damaged =
          contiguous_groups_raw |> String.split ~on:','
          |> List.filter_map ~f:Utils.Int.int_of_string_or_none in
        { springs; contiguous_group_damaged }

  let has_no_unknown springs : bool =
    springs |> List.exists ~f:(Spring.equal Unknown) |> not

  let satisfies_contiguous_conditions cr : bool =
    has_no_unknown cr.springs
    && cr.springs
       |> List.group ~break:(fun x y -> not @@ Spring.equal x y)
       |> List.filter_map ~f:(fun l ->
              let h = List.hd_exn l in
              if Spring.equal h Damaged then Some (List.length l) else None)
       |> List.equal Int.equal cr.contiguous_group_damaged

  let rec all_combinations springs : Spring.t list list =
    match springs with
    | [] -> [ [] ]
    | x :: xs when Spring.(equal x Operational || equal x Damaged) ->
        all_combinations xs |> List.map ~f:(fun r -> x :: r)
    | _ :: xs ->
        (all_combinations xs |> List.map ~f:(fun r -> Spring.Damaged :: r))
        @ (all_combinations xs |> List.map ~f:(fun r -> Spring.Operational :: r))

  let all_valid_combinations cr : Spring.t list list =
    cr.springs |> all_combinations
    |> List.filter ~f:(fun springs ->
           satisfies_contiguous_conditions
             { springs; contiguous_group_damaged = cr.contiguous_group_damaged })
end

let main rows =
  try
    let part_1 =
      rows
      |> List.map ~f:(fun row ->
             row |> Condition_records.parse
             |> Condition_records.all_valid_combinations |> List.length)
      |> List.fold ~init:0 ~f:( + ) in
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1
  with
  | Spring.Parse_exception ->
      Stdio.print_endline "Found an odd spring that couldn't be parsed"
  | Condition_records.Parse_exception ->
      Stdio.print_endline
        "Found an add condition record that couldn't be parsed"

let unsolved_condition_group = Condition_records.parse "???.### 1,1,3"
let solved_condition_group = Condition_records.parse "#.#.### 1,1,3"
let badly_solved_condition_group = Condition_records.parse "#...### 1,1,3"

let%test "Parse condition record" =
  let expected : Condition_records.t =
    { springs =
        [ Unknown; Unknown; Unknown; Operational; Damaged; Damaged; Damaged ]
    ; contiguous_group_damaged = [ 1; 1; 3 ]
    } in
  Condition_records.equal expected unsolved_condition_group

let%test "Does not satisfy contiguous conditions" =
  not
  @@ Condition_records.satisfies_contiguous_conditions
       badly_solved_condition_group

let%test "Does satisfy contiguous conditions" =
  Condition_records.satisfies_contiguous_conditions solved_condition_group
