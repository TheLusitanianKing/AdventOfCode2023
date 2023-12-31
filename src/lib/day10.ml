open Base

module Direction = struct
  type t = West | North | East | South [@@deriving eq]

  let all_directions = [ West; North; East; South ]
end

module Pipe = struct
  type t = Start | Ground | Connection of Direction.t * Direction.t
  [@@deriving eq]

  exception Parse_exception

  let parse = function
    | '.' -> Ground
    | 'S' -> Start
    | '|' -> Connection (North, South)
    | '-' -> Connection (West, East)
    | 'L' -> Connection (North, East)
    | 'J' -> Connection (North, West)
    | '7' -> Connection (South, West)
    | 'F' -> Connection (South, East)
    | _ -> raise Parse_exception
end

module Context = struct
  type t = Pipe.t array array

  let parse_line row = row |> String.to_array |> Array.map ~f:Pipe.parse
  let parse rows : t = rows |> List.to_array |> Array.map ~f:parse_line

  exception No_starting_point_exception

  let find_starting_point (context : t) : int * int =
    try context |> Utils.Matrix.find_point ~f:(Pipe.equal Start)
    with Utils.Matrix.Could_not_find_point_in_matrix ->
      raise No_starting_point_exception

  exception No_path_exception

  let direction_to context next_pos (direction_from_prev : Direction.t) :
      Direction.t option =
    let x, y = next_pos in
    let next_pipe : Pipe.t = context.(y).(x) in
    match direction_from_prev with
    | West -> (
        (* west from prev, so it arrives east of next_pos/next_pipe *)
        match next_pipe with
        | Connection (West, East) -> Some West
        | Connection (North, East) -> Some North
        | Connection (South, East) -> Some South
        | _ -> None)
    | North -> (
        (* north from prev, so it arrives south of next_pos/next_pipe *)
        match next_pipe with
        | Connection (North, South) -> Some North
        | Connection (South, West) -> Some West
        | Connection (South, East) -> Some East
        | _ -> None)
    | East -> (
        (* east from prev, so it arrives west of next_pos/next_pipe *)
        match next_pipe with
        | Connection (West, East) -> Some East
        | Connection (South, West) -> Some South
        | Connection (North, West) -> Some North
        | _ -> None)
    | South -> (
        (* south from prev, so it arrives north of next_pos/next_pipe *)
        match next_pipe with
        | Connection (North, South) -> Some South
        | Connection (North, East) -> Some East
        | Connection (North, West) -> Some West
        | _ -> None)

  let next_coordinate current_coordinate (direction_to : Direction.t) :
      int * int =
    let x, y = current_coordinate in
    match direction_to with
    | North -> (x, y - 1)
    | East -> (x + 1, y)
    | South -> (x, y + 1)
    | West -> (x - 1, y)

  let find_direction_to_from_start (context : t) start =
    let first_available_dir =
      Direction.all_directions
      |> List.find_map ~f:(fun dir ->
             let next = next_coordinate start dir in
             direction_to context next dir) in
    match first_available_dir with
    | None -> raise No_path_exception
    | Some dir -> dir

  let loop context : (int * int) list =
    let start = context |> find_starting_point in
    let first_dir = find_direction_to_from_start context start in
    let rec helper acc current_pos dir =
      if
        Utils.Tuple.equal_tuple' Int.compare current_pos start
        && not (List.is_empty acc)
      then acc
      else
        let next_pos = next_coordinate current_pos dir in
        if Utils.Tuple.equal_tuple' Int.compare next_pos start then
          current_pos :: acc
        else
          let next_dir_opt = direction_to context next_pos dir in
          match next_dir_opt with
          | None -> raise No_path_exception
          | Some next_dir -> helper (current_pos :: acc) next_pos next_dir in
    start :: helper [] start first_dir

  let farthest_distance l : int = List.length l / 2

  let is_coordinate_inside_the_polygon (polygon_limits : (int * int) list)
      coordinate =
    let x, y = coordinate in
    let segments =
      List.zip_exn
        (List.drop_last_exn polygon_limits)
        (List.drop polygon_limits 1) in
    segments
    |> List.filter ~f:(fun ((p1_x, p1_y), (p2_x, p2_y)) ->
           if
             y > min p1_y p2_y
             && y <= max p1_y p2_y
             && x <= max p1_x p2_x
             && not (Int.equal p1_y p2_y)
           then
             let xinters = ((y - p1_y) * (p2_x - p1_x) / (p2_y - p1_y)) + p1_x in
             p1_x = p2_x || x <= xinters
           else false)
    |> fun filtered_segs ->
    let c = Int.rem (List.length filtered_segs) 2 in
    not (Int.equal c 0)

  let all_coordinates_inside_the_polygon context
      (polygon_limits : (int * int) list) : (int * int) list =
    let all_coordinates = context |> Utils.Matrix.all_coordinates in
    let all_coordinates_outside_limits =
      all_coordinates
      |> Array.filter ~f:(fun c ->
             not
             @@ Array.mem
                  (polygon_limits |> List.to_array)
                  c
                  ~equal:(Utils.Tuple.equal_tuple' Int.compare)) in
    all_coordinates_outside_limits
    |> Array.filter ~f:(is_coordinate_inside_the_polygon polygon_limits)
    |> Array.to_list
end

let main rows =
  let context = rows |> Context.parse in
  let limits = context |> Context.loop in
  let part_1 = limits |> Context.farthest_distance in
  let part_2 =
    limits |> Context.all_coordinates_inside_the_polygon context |> List.length
  in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
  Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2

(* testing *)
let context = Context.parse [ "-L|F7"; "7S-7|"; "L|7||"; "-L-J|"; "L|-JF" ]

let%test "Context find start" =
  let expected = (1, 1) in
  let result = context |> Context.find_starting_point in
  Utils.Tuple.equal_tuple' Int.compare result expected
