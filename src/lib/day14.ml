open Base

module Cell = struct
  type t = Empty | Rounded | Cube [@@deriving eq, ord]

  exception Parse_exception

  let parse = function
    | 'O' -> Rounded
    | '#' -> Cube
    | '.' -> Empty
    | _ -> raise Parse_exception
end

module Context = struct
  type t = Cell.t array array [@@deriving eq]

  let parse rows : t =
    rows
    |> List.map ~f:(fun row ->
           row |> String.to_list |> List.map ~f:Cell.parse |> List.to_array)
    |> List.to_array

  exception Move_exception

  let move_rounded_rocks_as_far_right (row : Cell.t array) : Cell.t array =
    let r =
      row |> Array.to_list
      |> Utils.List.split ~f:(Cell.equal Cube)
      |> List.map ~f:(List.sort ~compare:Cell.compare)
      |> List.reduce_exn ~f:(fun l1 l2 -> l1 @ (Cell.Cube :: l2))
      |> List.to_array in
    if Int.equal (Array.length r) (Array.length row) then r
    else raise Move_exception

  let total_load (context : t) : int =
    context
    |> (fun r ->
         r |> Array.rev_inplace;
         r)
    |> Array.mapi ~f:(fun i xs ->
           (i + 1) * (xs |> Array.filter ~f:(Cell.equal Rounded) |> Array.length))
    |> Array.fold ~init:0 ~f:( + )

  let move_rocks_north (context : t) : t =
    context |> Array.transpose_exn
    |> Array.map ~f:(fun r ->
           r
           |> (fun r ->
                r |> Array.rev_inplace;
                r)
           |> move_rounded_rocks_as_far_right
           |> fun r ->
           r |> Array.rev_inplace;
           r)
    |> Array.transpose_exn
end

let main rows =
  try
    let context = rows |> Context.parse in
    let part_1 = context |> Context.move_rocks_north |> Context.total_load in
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1
  with
  | Cell.Parse_exception -> Stdio.print_endline "Could not parse the cell."
  | Context.Move_exception -> Stdio.print_endline "Could not move the rocks."

let test_context =
  Context.parse
    [ "O....#...."
    ; "O.OO#....#"
    ; ".....##..."
    ; "OO.#O....O"
    ; ".O.....O#."
    ; "O.#..O.#.#"
    ; "..O..#O..O"
    ; ".......O.."
    ; "#....###.."
    ; "#OO..#...."
    ]

let%test "Solve" =
  let expected =
    Context.parse
      [ "OOOO.#.O.."
      ; "OO..#....#"
      ; "OO..O##..O"
      ; "O..#.OO..."
      ; "........#."
      ; "..#....#.#"
      ; "..O..#.O.O"
      ; "..O......."
      ; "#....###.."
      ; "#....#...."
      ] in
  let result = Context.move_rocks_north test_context in
  Context.equal result expected

let%test "Total load" =
  let expected = 136 in
  let result = test_context |> Context.move_rocks_north |> Context.total_load in
  result = expected

let%test "Move far right" =
  let expected : Cell.t array =
    [| Empty
     ; Empty
     ; Rounded
     ; Rounded
     ; Cube
     ; Empty
     ; Empty
     ; Empty
     ; Empty
     ; Rounded
    |] in
  let result =
    Context.move_rounded_rocks_as_far_right
      ([| Rounded
        ; Empty
        ; Empty
        ; Rounded
        ; Cube
        ; Empty
        ; Empty
        ; Empty
        ; Rounded
        ; Empty
       |]
        : Cell.t array) in
  Array.equal Cell.equal result expected
