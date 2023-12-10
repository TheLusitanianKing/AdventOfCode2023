open Base

module Cube = struct
  module T = struct
    type t = { cube_colour : string; cube_quantity : int }

    let compare t1 t2 =
      let c = String.compare t1.cube_colour t2.cube_colour in
      if c <> 0 then c else Int.compare t2.cube_quantity t1.cube_quantity

    let equal t1 t2 = compare t1 t2 = 0

    let sexp_of_t t : Sexp.t =
      List [ Atom t.cube_colour; Atom (Int.to_string @@ t.cube_quantity) ]

    exception Parse_exception

    let parse s =
      let s' = String.strip s in
      let regex = Str.regexp {|\([0-9]+\) \([a-zA-Z]+\)|} in
      let success = Str.string_match regex s' 0 in
      if not success then raise Parse_exception
      else
        try
          let cube_quantity = Int.of_string @@ Str.matched_group 1 s' in
          let cube_colour = Str.matched_group 2 s' in
          { cube_colour; cube_quantity }
        with _ -> raise Parse_exception

    let%test "Parse (1)" =
      equal (parse "1 red") { cube_colour = "red"; cube_quantity = 1 }

    let%test "Parse (2)" =
      equal (parse " 2 blue") { cube_colour = "blue"; cube_quantity = 2 }

    let%test "Parse (3)" =
      equal (parse " 14 yellow ") { cube_colour = "yellow"; cube_quantity = 14 }

    let is_valid limitations x =
      limitations
      |> List.for_all ~f:(fun limit ->
             if String.equal limit.cube_colour x.cube_colour then
               x.cube_quantity <= limit.cube_quantity
             else true)
  end

  include T
  include Comparator.Make (T)
end

let part_1_limitations : Cube.t list =
  [ { cube_colour = "red"; cube_quantity = 12 }
  ; { cube_colour = "green"; cube_quantity = 13 }
  ; { cube_colour = "blue"; cube_quantity = 14 }
  ]

module Cube_set = struct
  (* Probably should have made this a Map instead of a Set but I didn't want to move back to it *)
  type t = { cubes : (Cube.t, Cube.comparator_witness) Set.t }

  let parse s =
    let cubes =
      s |> String.split ~on:',' |> List.map ~f:Cube.parse
      |> Set.of_list (module Cube) in
    { cubes }

  let is_valid limitations x =
    x.cubes |> Set.to_list |> List.for_all ~f:(Cube.is_valid limitations)
end

module Game = struct
  type t = { id : int; cube_sets : Cube_set.t list }

  exception Parse_exception

  let parse s =
    let regex = Str.regexp {|Game \([0-9]+\): |} in
    let success = Str.string_match regex s 0 in
    if not success then raise Parse_exception
    else
      try
        let id = Int.of_string @@ Str.matched_group 1 s in
        let reminder = Str.replace_first regex "" s |> String.split ~on:';' in
        let cube_sets = List.map ~f:Cube_set.parse reminder in
        { id; cube_sets }
      with _ -> raise Parse_exception

  let is_valid limitations x =
    x.cube_sets |> List.for_all ~f:(Cube_set.is_valid limitations)

  let%test "Is valid game (1)" =
    let g = parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
    is_valid part_1_limitations g

  let%test "Is valid game (2)" =
    let g =
      parse
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 \
         red" in
    not @@ is_valid part_1_limitations g

  let%test "Is valid game (2)" =
    let g =
      parse
        "Game 3: 8 green, 6 blue, 12 red; 5 blue, 4 red, 13 green; 5 green, 1 \
         red" in
    is_valid part_1_limitations g

  let power x =
    let set =
      Set.union_list (module Cube) (List.map ~f:(fun y -> y.cubes) x.cube_sets)
    in
    let grouped_sets =
      set
      |> Set.group_by ~equiv:(fun (c1 : Cube.t) (c2 : Cube.t) ->
             String.(c1.cube_colour = c2.cube_colour)) in
    grouped_sets
    |> List.map ~f:(fun set_of_same_colour ->
           set_of_same_colour |> Set.to_list
           |> List.map ~f:(fun (c : Cube.t) -> c.cube_quantity)
           |> fun xs ->
           Option.value ~default:1 @@ List.max_elt ~compare:Int.compare xs)
    |> List.fold ~init:1 ~f:(fun acc x -> acc * x)

  let%test "Power" =
    let g = parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
    power g = 48
end

let main rows =
  try
    let games = rows |> List.map ~f:Game.parse in
    let part_1 =
      games
      |> List.filter ~f:(Game.is_valid part_1_limitations)
      |> List.fold ~init:0 ~f:(fun acc (g : Game.t) -> acc + g.id) in

    let part_2 =
      games |> List.map ~f:Game.power
      |> List.fold ~init:0 ~f:(fun acc x -> acc + x) in

    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
    Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2
  with
  | Game.Parse_exception -> Stdio.print_endline "Failed parsing a game"
  | Cube.Parse_exception -> Stdio.print_endline "Failed parsing a cube"
