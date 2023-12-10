open Base

module Coordinate = struct
  module T = struct
    type t = { x : int; y : int }

    let compare { x = x1; y = y1 } { x = x2; y = y2 } =
      let x_diff = Int.compare x1 x2 in
      if x_diff <> 0 then x_diff else Int.compare y1 y2

    let sexp_of_t t : Sexp.t =
      List [ Atom (Int.to_string @@ t.x); Atom (Int.to_string @@ t.y) ]

    let equal c1 c2 = compare c1 c2 = 0
  end

  include T
  include Comparator.Make (T)
end

let neighbours ({ x; y } as c : Coordinate.t) =
  let xs = [ x - 1; x; x + 1 ] and ys = [ y - 1; y; y + 1 ] in
  let entire_set =
    List.cartesian_product xs ys
    |> List.map ~f:(fun (x, y) : Coordinate.t -> { x; y })
    |> Set.of_list (module Coordinate) in
  Set.remove entire_set c

let%test "Neighbours" =
  let expected_neighbours =
    Set.of_list
      (module Coordinate)
      [ { x = 0; y = 0 }
      ; { x = 1; y = 0 }
      ; { x = 2; y = 0 }
      ; { x = 0; y = 1 }
      ; { x = 2; y = 1 }
      ; { x = 0; y = 2 }
      ; { x = 1; y = 2 }
      ; { x = 2; y = 2 }
      ] in
  Set.equal expected_neighbours @@ neighbours ({ x = 1; y = 1 } : Coordinate.t)

let compare_parsed_row =
  List.equal (Utils.Tuple.equal_tuple Int.compare Set.compare_direct)

let parse_row ~y row_content =
  let rec helper acc acc_ns acc_cs x cs =
    if String.is_empty cs then
      if String.is_empty acc_ns then List.rev acc
      else List.rev @@ ((Int.of_string acc_ns, acc_cs) :: acc)
    else
      let next = String.get cs 0 in
      let x' = x + 1 in
      let cs' = cs |> String.subo ~pos:1 in
      if Char.is_digit next then
        helper acc
          (acc_ns ^ String.of_char next)
          (Set.add acc_cs ({ x; y } : Coordinate.t))
          x' cs'
      else if String.is_empty acc_ns then helper acc acc_ns acc_cs x' cs'
      else
        helper
          ((Int.of_string acc_ns, acc_cs) :: acc)
          ""
          (Set.empty (module Coordinate))
          x' cs' in
  helper [] "" (Set.empty (module Coordinate)) 0 row_content

let%test "Parse row" =
  let expected_parse =
    [ ( 733
      , Set.of_list
          (module Coordinate)
          [ { x = 3; y = 0 }; { x = 4; y = 0 }; { x = 5; y = 0 } ] )
    ; ( 289
      , Set.of_list
          (module Coordinate)
          [ { x = 13; y = 0 }; { x = 14; y = 0 }; { x = 15; y = 0 } ] )
    ; ( 262
      , Set.of_list
          (module Coordinate)
          [ { x = 18; y = 0 }; { x = 19; y = 0 }; { x = 20; y = 0 } ] )
    ; ( 92
      , Set.of_list (module Coordinate) [ { x = 26; y = 0 }; { x = 27; y = 0 } ]
      )
    ] in
  compare_parsed_row expected_parse
  @@ parse_row ~y:0 "...733.......289..262.....92"
  && compare_parsed_row expected_parse
     @@ parse_row ~y:0 "...733.......289..262.....92..."

let coordinate_is_symbol c =
  (not @@ Char.is_digit c) && (not @@ Char.equal c '.')

let get_number_if_valid matrix (n, cs) ~f =
  let cs' =
    cs |> Set.to_list |> List.map ~f:neighbours
    |> Set.union_list (module Coordinate)
    |> Set.to_list in
  let coordinates_asserting_f =
    cs'
    |> List.filter_map ~f:(fun ({ x; y } as c : Coordinate.t) ->
           try if f matrix.(y).(x) then Some c else None with _ -> None) in
  if List.is_empty coordinates_asserting_f then None
  else Some (n, Set.of_list (module Coordinate) coordinates_asserting_f)

let%test "Get number if valid (1)" =
  let m = [| [| '.'; '.'; '.' |]; [| '.'; '5'; '2' |]; [| '.'; '.'; '.' |] |] in
  let n =
    (52, Set.of_list (module Coordinate) [ { x = 1; y = 1 }; { x = 2; y = 1 } ])
  in
  Option.equal
    (Utils.Tuple.equal_tuple Int.compare Set.compare_direct)
    (get_number_if_valid ~f:coordinate_is_symbol m n)
  @@ None

let%test "Get number if valid (2)" =
  let m = [| [| '.'; '.'; '.' |]; [| '.'; '5'; '2' |]; [| '.'; '*'; '.' |] |] in
  let n =
    (52, Set.of_list (module Coordinate) [ { x = 1; y = 1 }; { x = 2; y = 1 } ])
  in
  Option.equal
    (Utils.Tuple.equal_tuple Int.compare Set.compare_direct)
    (get_number_if_valid ~f:coordinate_is_symbol m n)
  @@ Some (52, Set.of_list (module Coordinate) [ { x = 1; y = 2 } ])

let parse_matrix rows =
  rows
  |> List.map ~f:(fun s -> List.to_array @@ String.to_list s)
  |> List.to_array

let%test "Parse matrix" =
  let expected_matrix =
    [| [| '.'; '.'; '.' |]; [| '.'; '5'; '2' |]; [| '.'; '.'; '.' |] |] in
  Array.equal (Array.equal Char.equal) expected_matrix
  @@ parse_matrix [ "..."; ".52"; "..." ]

let parse_numbers rows =
  rows |> List.concat_mapi ~f:(fun i row -> row |> parse_row ~y:i)

let%test "Parse numbers" =
  let expected =
    [ ( 52
      , Set.of_list (module Coordinate) [ { x = 1; y = 1 }; { x = 2; y = 1 } ]
      )
    ] in
  compare_parsed_row expected @@ parse_numbers [ "..."; ".52"; "..." ]

let coordinate_is_gear = Char.equal '*'

let get_geared_ratios matrix ns =
  ns
  |> List.filter_map ~f:(get_number_if_valid ~f:coordinate_is_gear matrix)
  |> List.concat_map ~f:(fun (n, set_of_cs) ->
         Set.to_list set_of_cs |> List.map ~f:(fun c -> (c, n)))
  |> List.sort ~compare:(fun (c1, _) (c2, _) -> Coordinate.compare c1 c2)
  |> List.group ~break:(fun (c1, _) (c2, _) -> not @@ Coordinate.equal c1 c2)
  |> List.filter_map ~f:(fun list_of_same_coordinate ->
         if List.length list_of_same_coordinate = 2 then
           let gear_ratio =
             list_of_same_coordinate
             |> List.map ~f:(fun (_, n) -> n)
             |> List.fold ~init:1 ~f:(fun acc n -> acc * n) in
           Some gear_ratio
         else None)

let main rows =
  let m = rows |> parse_matrix in
  let ns = rows |> parse_numbers in
  let part_1 =
    ns
    |> List.fold ~init:0 ~f:(fun acc n ->
           acc
           + (Option.value_map ~default:0 ~f:(fun (n, _) -> n)
             @@ get_number_if_valid ~f:coordinate_is_symbol m n)) in
  let part_2 =
    ns |> get_geared_ratios m
    |> List.fold ~init:0 ~f:(fun acc geared_number -> acc + geared_number) in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
  Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2
