open Base

module Tile = struct
  type t =
    | Empty
    | RightMirror
    | LeftMirror
    | VerticalSplitter
    | HorizontalSplitter
  [@@deriving eq, show]

  exception Parse_exception

  let parse = function
    | '.' -> Empty
    | '/' -> RightMirror
    | '\\' -> LeftMirror
    | '|' -> VerticalSplitter
    | '-' -> HorizontalSplitter
    | _ -> raise Parse_exception
end

module Direction = struct
  type t = Left | Up | Right | Down [@@deriving show, ord]

  let next ~direction (x, y) =
    match direction with
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)

  let get_next_directions ~direction ~breaking_tile =
    match (breaking_tile : Tile.t) with
    | Empty -> [ direction ]
    | RightMirror -> (
        (* / *)
        match direction with
        | Left -> [ Down ]
        | Up -> [ Right ]
        | Down -> [ Left ]
        | Right -> [ Up ])
    | LeftMirror -> (
        (* \ *)
        match direction with
        | Left -> [ Up ]
        | Up -> [ Left ]
        | Down -> [ Right ]
        | Right -> [ Down ])
    | VerticalSplitter -> (
        match direction with
        | Left | Right -> [ Up; Down ]
        | Up | Down -> [ direction ])
    | HorizontalSplitter -> (
        match direction with
        | Left | Right -> [ direction ]
        | Up | Down -> [ Left; Right ])
end

module Contraption = struct
  type t = Tile.t array array

  let visited_points : ((int * int) * Direction.t) list ref = ref []

  let parse rows =
    rows
    |> List.map ~f:(fun row ->
           row |> String.to_array |> Array.map ~f:Tile.parse)
    |> List.to_array

  let rec take_until_breaking_point t f acc (cx, cy) :
      (int * int) list * (int * int) option =
    try
      let el : Tile.t = t.(cy).(cx) in
      if Tile.equal el Tile.Empty then
        take_until_breaking_point t f ((cx, cy) :: acc) (f (cx, cy))
      else (acc, Some (cx, cy))
    with _ -> (acc, None)

  let rec beem t ~start ~direction : (int * int) list =
    match
      !visited_points
      |> List.find
           ~f:
             (Utils.Tuple.equal_tuple
                (Utils.Tuple.compare_tuple_simple Int.compare)
                Direction.compare (start, direction))
    with
    | Some _ -> []
    | None -> (
        let x, y = start in
        let f = Direction.next ~direction in
        let cs, next_opt = take_until_breaking_point t f [] (x, y) in
        visited_points :=
          List.append
            (cs |> List.map ~f:(fun c -> (c, direction)))
            !visited_points;
        match next_opt with
        | None -> cs
        | Some breaking_point ->
            let bx, by = breaking_point in
            let next_dirs : Direction.t list =
              Direction.get_next_directions ~direction
                ~breaking_tile:t.(by).(bx) in

            List.append (breaking_point :: cs)
              (next_dirs
              |> List.concat_map ~f:(fun next_dir ->
                     beem t
                       ~start:
                         (Direction.next ~direction:next_dir breaking_point)
                       ~direction:next_dir)))

  let solve t =
    visited_points := [];
    let cs = beem t ~start:(0, 0) ~direction:Direction.Right in
    cs
    |> List.dedup_and_sort
         ~compare:(Utils.Tuple.compare_tuple_simple Int.compare)
    |> List.length
end

let main rows =
  let m = rows |> Utils.Matrix.parse ~f:Tile.parse in
  let part_1 = m |> Contraption.solve in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1

let test_contraption =
  Contraption.parse
    [ ".|...\\...."
    ; "|.-.\\....."
    ; ".....|-..."
    ; "........|."
    ; ".........."
    ; ".........\\"
    ; "..../.\\\\.."
    ; ".-.-/..|.."
    ; ".|....-|.\\"
    ; "..//.|...."
    ]

let%test "Take until breaking point" =
  let expected = ([ (0, 0) ], Some (1, 0)) in
  let result =
    Contraption.take_until_breaking_point test_contraption
      (Direction.next ~direction:Right)
      [] (0, 0) in
  Utils.Tuple.equal_tuple
    (List.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (Option.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    expected result

let%test "Take until breaking point (2)" =
  let expected =
    (List.rev [ (1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6) ], Some (1, 7))
  in
  let result =
    Contraption.take_until_breaking_point test_contraption
      (Direction.next ~direction:Down)
      [] (1, 1) in
  Utils.Tuple.equal_tuple
    (List.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    (Option.compare (Utils.Tuple.compare_tuple_simple Int.compare))
    expected result
