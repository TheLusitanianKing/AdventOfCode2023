open Base

module Direction = struct
  type t = Left | Right [@@deriving eq, show]

  exception Parse_exception

  let parse = function 'L' -> Left | 'R' -> Right | _ -> raise Parse_exception
end

module Tree = struct
  type t = (string, string * string, String.comparator_witness) Map.t

  exception Parse_exception

  let parse_single path : string * (string * string) =
    let regex = Str.regexp {|\([A-Z]+\) = (\([A-Z]+\), \([A-Z]+\))|} in
    let success = Str.string_match regex path 0 in
    if not success then raise Parse_exception
    else
      let key = Str.matched_group 1 path in
      let left = Str.matched_group 2 path in
      let right = Str.matched_group 3 path in
      (key, (left, right))

  let parse paths : t =
    paths
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun acc x ->
           let key, data = parse_single x in
           Map.add_exn acc ~key ~data)
end

module Context = struct
  type t = { tree : Tree.t; instructions : Direction.t list }

  exception Parse_exception

  let parse rows : t =
    let instructions_line = List.hd_exn rows in
    let path_lines = List.drop rows 2 in
    let instructions =
      instructions_line |> Batteries.String.to_list
      |> List.map ~f:Direction.parse in
    let tree = Tree.parse path_lines in
    { tree; instructions }

  let run_until context ~start ~stop =
    let rec helper (insts : Direction.t list) current_node step =
      if stop current_node then step
      else
        match insts with
        | [] -> helper context.instructions current_node step
        | Left :: is ->
            let k, _ = Map.find_exn context.tree current_node in
            helper is k (step + 1)
        | Right :: is ->
            let _, k = Map.find_exn context.tree current_node in
            helper is k (step + 1) in
    helper [] start 0

  let run_multi_until context ~start ~stop =
    let initial_nodes = context.tree |> Map.keys |> List.filter ~f:start in
    let first_zs = initial_nodes |> List.map ~f:(fun node -> run_until context ~start:node ~stop) in
    Utils.Arithmetic.lcm_multi first_zs
end

let main rows =
  try
    let context = rows |> Context.parse in
    let part_1 = context |> Context.run_until ~start:"AAA" ~stop:(String.equal "ZZZ") in
    let part_2 =
      context
      |> Context.run_multi_until
           ~start:(String.is_suffix ~suffix:"A")
           ~stop:(String.is_suffix ~suffix:"Z") in
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
    Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2
  with
  | Direction.Parse_exception ->
      Stdio.prerr_endline "Could not parse the direction."
  | Tree.Parse_exception -> Stdio.prerr_endline "Could not parse the tree."
  | Context.Parse_exception ->
      Stdio.prerr_endline "Could not parse the context."

(* testing *)
let%test "Parse tree" =
  let result = Tree.parse_single "DGZ = (BSL, BRS)" in
  let expected = ("DGZ", ("BSL", "BRS")) in
  Utils.Tuple.equal_tuple String.compare
    (Utils.Tuple.compare_tuple_simple String.compare)
    result expected

let%test "Parse context instructions" =
  let result =
    Context.parse [ "LRRL"; ""; "RBX = (TMF, KTP)" ] |> fun c -> c.instructions
  in
  let expected : Direction.t list = [ Left; Right; Right; Left ] in
  List.equal Direction.equal result expected
