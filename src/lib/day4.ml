open Base

module Card = struct
  type t =
    { id : int
    ; winning_numbers : (Int.t, Int.comparator_witness) Set.t
    ; picked_numbers : int list
    }

  let compare c1 c2 =
    let id_c = Int.compare c1.id c2.id in
    if id_c <> 0 then id_c
    else
      let wn_c = Set.compare_direct c1.winning_numbers c2.winning_numbers in
      if wn_c <> 0 then wn_c
      else List.compare Int.compare c1.picked_numbers c2.picked_numbers

  let equal c1 c2 = compare c1 c2 = 0

  exception Parse_exception

  let parse s =
    let card_regex = Str.regexp {|Card[ ]+\([0-9]+\): |} in
    let success = Str.string_match card_regex s 0 in
    if not success then raise Parse_exception
    else
      try
        let id = Int.of_string @@ Str.matched_group 1 s in
        let reminder =
          Str.replace_first card_regex "" s |> String.split ~on:'|' in
        if List.length reminder <> 2 then raise Parse_exception
        else
          let winning_numbers =
            List.nth_exn reminder 0 |> String.split ~on:' '
            |> List.filter_map ~f:(fun x ->
                   try Some (Int.of_string x) with _ -> None)
            |> Set.of_list (module Int) in
          let picked_numbers =
            List.nth_exn reminder 1 |> String.split ~on:' '
            |> List.filter_map ~f:(fun x ->
                   try Some (Int.of_string x) with _ -> None) in
          { id; winning_numbers; picked_numbers }
      with _ -> raise Parse_exception

  let%test "Parse row" =
    let result = parse "Card   6: 53 92  3 77 46 | 42  3 92  4 5 " in
    let expected =
      { id = 6
      ; winning_numbers = Set.of_list (module Int) [ 53; 92; 3; 77; 46 ]
      ; picked_numbers = [ 42; 3; 92; 4; 5 ]
      } in
    equal result expected

  let winning_numbers c =
    c.picked_numbers |> List.filter ~f:(fun n -> Set.mem c.winning_numbers n)

  let%test "Winning numbers" =
    let card = parse "Card   6: 53 92  3 77 46 | 42  3 92  4 5 " in
    List.equal Int.equal [ 3; 92 ] @@ winning_numbers card
end

let main rows =
  let cards = rows |> List.map ~f:Card.parse in
  let cards_with_winning_numbers_length =
    cards |> List.map ~f:(fun c -> c |> Card.winning_numbers |> List.length)
  in
  let part_1 =
    cards_with_winning_numbers_length
    |> List.map ~f:(fun l ->
           if l <> 0 then
             let open Batteries.Int in
             pow 2 (l - 1)
           else 0)
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x) in
  let part_2 =
    cards_with_winning_numbers_length
    |> List.foldi
         ~init:(Array.create ~len:(List.length cards) 1)
         ~f:(fun index acc c ->
           let how_many_cards = acc.(index) in
           List.range ~start:`exclusive ~stop:`inclusive index (index + c)
           |> List.fold ~init:acc ~f:(fun acc where_to_add ->
                  acc.(where_to_add) <- acc.(where_to_add) + how_many_cards;
                  acc))
    |> Array.fold ~init:0 ~f:(fun acc x -> acc + x) in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
  Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2
