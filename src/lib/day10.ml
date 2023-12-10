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
    context
    |> Array.find_mapi ~f:(fun row_i column ->
           column
           |> Array.find_mapi ~f:(fun column_i p ->
                  if Pipe.equal p Start then Some column_i else None)
           |> Option.bind ~f:(fun column_i -> Some (column_i, row_i)))
    |> function
    | None -> raise No_starting_point_exception
    | Some c -> c

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
        if Utils.Tuple.equal_tuple' Int.compare next_pos start then acc
        else
          let next_dir_opt = direction_to context next_pos dir in
          match next_dir_opt with
          | None -> raise No_path_exception
          | Some next_dir -> helper (current_pos :: acc) next_pos next_dir in
    helper [] start first_dir

  let farthest_distance l : int = (List.length l / 2) + 1
end

let main rows =
  let context = rows |> Context.parse in
  let l = context |> Context.loop in
  let part_1 = Context.farthest_distance l in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1

(* testing *)
let context = Context.parse [ "-L|F7"; "7S-7|"; "L|7||"; "-L-J|"; "L|-JF" ]

let%test "Context find start" =
  let expected = (1, 1) in
  let result = context |> Context.find_starting_point in
  Utils.Tuple.equal_tuple' Int.compare result expected
