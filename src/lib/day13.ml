open Base

module Cell = struct
  type t = Ash | Rock [@@deriving eq]

  exception Parse_exception

  let parse = function '.' -> Ash | '#' -> Rock | _ -> raise Parse_exception
end

let rec has_horizontal_reflection_at (m : Cell.t array array) nb_rows (x : int)
    (y : int) : bool =
  if x > 0 && x <= nb_rows && y > 0 && y <= nb_rows then
    let xs = m.(x - 1) and ys = m.(y - 1) in
    let m' =
      Array.append (m |> Array.sub ~pos:0 ~len:(x - 1)) (m |> Array.subo ~pos:y)
    in
    Array.equal Cell.equal xs ys
    && has_horizontal_reflection_at m' (nb_rows - 2) (x - 1) (y - 1)
  else true

let horizontal_reflections (m : Cell.t array array) : (int * int) list =
  let nb_rows = Array.length m in
  let xs = List.range 1 nb_rows ~start:`inclusive ~stop:`inclusive in
  let all_possible_axis =
    List.zip_exn (List.drop_last_exn xs) (List.drop xs 1) in
  all_possible_axis
  |> List.filter ~f:(fun (x, y) -> has_horizontal_reflection_at m nb_rows x y)

let reflections m : (int * int) list * (int * int) list =
  let vertical_rs = horizontal_reflections (Array.transpose_exn m) in
  let horizontal_rs = horizontal_reflections m in
  (vertical_rs, horizontal_rs)

let solve m =
  let vertical_rs, horizontal_rs = reflections m in
  let vs =
    vertical_rs |> List.hd |> Option.value_map ~default:0 ~f:(fun (x, _) -> x)
  in
  let hs =
    horizontal_rs |> List.hd |> Option.value_map ~default:0 ~f:(fun (x, _) -> x)
  in
  vs + (hs * 100)

let parse_matrix rows =
  rows
  |> List.filter ~f:(fun x -> not @@ String.is_empty x)
  |> List.map ~f:(fun row -> row |> String.to_array |> Array.map ~f:Cell.parse)
  |> List.to_array

let main rows =
  try
    let patterns =
      rows |> List.group ~break:(fun _ next -> String.is_empty next) in
    let ms = patterns |> List.map ~f:parse_matrix in
    let part_1 = ms |> List.map ~f:solve |> List.fold ~init:0 ~f:( + ) in
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1
  with Cell.Parse_exception -> Stdio.print_endline "Could not parse the cells"

let test_m_1 =
  parse_matrix
    [ "#.##..##."
    ; "..#.##.#."
    ; "##......#"
    ; "##......#"
    ; "..#.##.#."
    ; "..##..##."
    ; "#.#.##.#."
    ]

let test_m_2 =
  parse_matrix
    [ "#...##..#"
    ; "#....#..#"
    ; "..##..###"
    ; "#####.##."
    ; "#####.##."
    ; "..##..###"
    ; "#....#..#"
    ]

let%test "Solve (1)" =
  let expected = 5 in
  let result = solve test_m_1 in
  Int.(result = expected)

let%test "Solve (2)" =
  let expected = 400 in
  let result = solve test_m_2 in
  Int.(result = expected)
