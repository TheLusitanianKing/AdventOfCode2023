open Base

module Cell = struct
  type t = Empty | Galaxy [@@deriving eq]

  exception Parse_exception

  let parse = function
    | '.' -> Empty
    | '#' -> Galaxy
    | _ -> raise Parse_exception
end

module Context = struct
  type t = Cell.t array array

  exception Parse_exception

  let parse rows : t =
    rows
    |> List.map ~f:(fun row ->
           row |> String.to_list |> List.map ~f:Cell.parse |> List.to_array)
    |> List.to_array

  (* all rows and cols extended because they're empty *)
  let expanded_rows (context : t) : int list =
    context
    |> Array.filter_mapi ~f:(fun i x ->
           if x |> Array.for_all ~f:(Cell.equal Empty) then Some i else None)
    |> Array.to_list

  let expanded_cols (context : t) : int list =
    (* it's basically the same with the matrix transposed *)
    context |> Array.transpose_exn |> expanded_rows

  let get_all_galaxies (context : t) : (int * int) list =
    context |> Utils.Matrix.find_points ~f:(Cell.equal Galaxy)

  let get_all_paired_galaxies (context : t) : ((int * int) * (int * int)) list =
    context |> get_all_galaxies
    |> (fun all_galaxies -> List.cartesian_product all_galaxies all_galaxies)
    |> List.map ~f:(fun (c1, c2) ->
           if Utils.Tuple.compare_tuple_simple Int.compare c1 c2 <= 0 then
             (c1, c2)
           else (c2, c1))
    |> List.dedup_and_sort ~compare:(fun xc1 xc2 ->
           Utils.Tuple.compare_tuple_simple
             (Utils.Tuple.compare_tuple_simple Int.compare)
             xc1 xc2)
    |> List.filter ~f:(fun (c1, c2) ->
           not @@ Utils.Tuple.equal_tuple' Int.compare c1 c2)

  let distance_between_galaxies (context : t) (galaxy_a : int * int)
      (galaxy_b : int * int) coef : int =
    let exp_rows = context |> expanded_rows in
    let exp_cols = context |> expanded_cols in
    let x1, y1 = galaxy_a in
    let x2, y2 = galaxy_b in
    let ds_x_start, ds_x_stop = (min x1 x2, max x1 x2) in
    let ds_y_start, ds_y_stop = (min y1 y2, max y1 y2) in
    let touched_exp_rows =
      exp_rows
      |> List.filter ~f:(fun exp_row ->
             exp_row > ds_y_start && exp_row < ds_y_stop) in
    let touched_exp_cols =
      exp_cols
      |> List.filter ~f:(fun exp_col ->
             exp_col > ds_x_start && exp_col < ds_x_stop) in
    ds_x_stop - ds_x_start + (ds_y_stop - ds_y_start)
    + (List.length touched_exp_cols * coef)
    + (List.length touched_exp_rows * coef)
end

let main rows =
  try
    let context = Context.parse rows in
    let paired_galaxies = context |> Context.get_all_paired_galaxies in
    let part_1 =
      paired_galaxies
      |> List.map ~f:(fun (g1, g2) ->
             Context.distance_between_galaxies context g1 g2 1)
      |> List.fold ~init:0 ~f:( + ) in
    let part_2 =
      paired_galaxies
      |> List.map ~f:(fun (g1, g2) ->
             Context.distance_between_galaxies context g1 g2 (1_000_000 - 1))
      |> List.fold ~init:0 ~f:( + ) in
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
    Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2
  with Cell.Parse_exception -> Stdio.print_endline "Could not parse a cell"

(* testing *)
let test_context =
  Context.parse
    [ "...#......"
    ; ".......#.."
    ; "#........."
    ; ".........."
    ; "......#..."
    ; ".#........"
    ; ".........#"
    ; ".........."
    ; ".......#.."
    ; "#...#....."
    ]

let%test "Expanded rows" =
  let expected = [ 3; 7 ] in
  let result = Context.expanded_rows test_context in
  List.equal Int.equal result expected

let%test "Expanded cols" =
  let expected = [ 2; 5; 8 ] in
  let result = Context.expanded_cols test_context in
  List.equal Int.equal result expected

let%test "All galaxies" =
  let expected =
    [ (3, 0); (7, 1); (0, 2); (6, 4); (1, 5); (9, 6); (7, 8); (0, 9); (4, 9) ]
  in
  let result = Context.get_all_galaxies test_context in
  List.equal (Utils.Tuple.equal_tuple' Int.compare) result expected

let%test "Distance between galaxies" =
  let expected = 9 in
  let result = Context.distance_between_galaxies test_context (1, 5) (4, 9) 1 in
  Int.equal result expected

let%test "Distance between galaxies (2)" =
  let expected = 9 in
  let result = Context.distance_between_galaxies test_context (4, 9) (1, 5) 1 in
  Int.equal result expected

let%test "Distance between galaxies (3)" =
  let expected = 15 in
  let result = Context.distance_between_galaxies test_context (3, 0) (7, 8) 1 in
  Int.equal result expected

let%test "Distance between galaxies (4)" =
  let expected = 17 in
  let result = Context.distance_between_galaxies test_context (0, 2) (9, 6) 1 in
  Int.equal result expected

let%test "Distance between galaxies (5)" =
  let expected = 5 in
  let result = Context.distance_between_galaxies test_context (0, 9) (4, 9) 1 in
  Int.equal result expected

let%test "Pair number" =
  let expected = 36 in
  let result = Context.get_all_paired_galaxies test_context |> List.length in
  Int.equal result expected
