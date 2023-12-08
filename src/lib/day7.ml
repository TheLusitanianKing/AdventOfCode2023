open Base

exception Parse_exception

(* Modules *)
module Card = struct
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
  [@@deriving eq, ord, show, enum]

  let parse = function
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | _ -> raise Parse_exception
end

module Type = struct
  type t =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
  [@@deriving eq, enum, ord, show]

  let type_of_play (cs : Card.t list) : t =
    let grouped_cards =
      cs
      |> List.sort ~compare:Card.compare
      |> List.group ~break:(fun x y -> not @@ Card.equal x y)
      |> List.map ~f:List.length
      |> List.sort ~compare:Int.compare
      |> List.rev in
    match grouped_cards with
    | [] -> failwith "Impossible unless the hand is empty which shouldn't parse"
    | 5 :: _ -> FiveOfAKind
    | 4 :: _ -> FourOfAKind
    | 3 :: 2 :: _ -> FullHouse
    | 3 :: _ -> ThreeOfAKind
    | 2 :: 2 :: _ -> TwoPair
    | 2 :: _ -> OnePair
    | 1 :: _ -> HighCard
    | _ ->
        failwith
          "Impossible unless the hand has more than 5 cards, which shouldn't \
           parse"

  let type_of_play_with_joker _cs : t = failwith "TODO"
end

module Play = struct
  type t = { hand : Card.t list; bid : int } [@@deriving eq, show]

  let parse_hand s : Card.t list =
    let hand = s |> Batteries.String.to_list |> List.map ~f:Card.parse in
    if List.length hand <> 5 then raise Parse_exception else hand

  let parse s =
    s |> String.split ~on:' ' |> function
    | [] | [ _ ] -> raise Parse_exception
    | x :: y :: _ -> (
        match y |> Utils.Int.int_of_string_or_none with
        | None -> raise Parse_exception
        | Some bid ->
            let hand = parse_hand x in
            { bid; hand })
end

let rank_plays ps get_type_from_play =
  ps
  |> List.map ~f:(fun (p : Play.t) -> (p, get_type_from_play p.hand))
  |> List.sort ~compare:(fun ((p1 : Play.t), t1) (p2, t2) ->
         let t_cpt = Type.compare t1 t2 in
         if t_cpt <> 0 then t_cpt else List.compare Card.compare p1.hand p2.hand)
  |> fun sorted_plays ->
  List.zip_exn
    (List.range 1 (List.length sorted_plays) ~start:`inclusive ~stop:`inclusive)
    sorted_plays

let solve ranked_plays =
  ranked_plays
  |> List.map ~f:(fun (rank, ((p : Play.t), _)) -> rank * p.bid)
  |> List.fold ~init:0 ~f:( + )

(* Main *)
let main rows =
  try
    let plays = rows |> List.map ~f:Play.parse in
    let part_1 = rank_plays plays Type.type_of_play |> solve in
    (* let part_2 = rank_plays plays Type.type_of_play_with_joker |> solve in *)
    Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1;
    (* Stdio.print_endline @@ Printf.sprintf "Part 2: %d" part_2; *)
  with Parse_exception -> Stdio.print_endline "Failed to parse."

(* Some testing *)
let play_1 : Play.t = { hand = [ Three; Two; Ten; Three; King ]; bid = 765 }
let play_2 : Play.t = { hand = [ Ten; Five; Five; Jack; Five ]; bid = 684 }
let play_3 : Play.t = { hand = [ King; King; Six; Seven; Seven ]; bid = 28 }
let play_4 : Play.t = { hand = [ King; Ten; Jack; Jack; Ten ]; bid = 220 }
let play_5 : Play.t = { hand = [ Queen; Queen; Queen; Jack; Ace ]; bid = 483 }

let%test "Parsing hands" =
  let expected = play_1.hand in
  let result = Play.parse_hand "32T3K" in
  List.equal Card.equal result expected

let%test "Parsing play" =
  let expected = play_3 in
  let result = Play.parse "KK677 28" in
  Play.equal result expected

let%test "Type of play" =
  let expected : Type.t = OnePair in
  let result = Type.type_of_play play_1.hand in
  Type.equal result expected

let%test "Type of play (2)" =
  let expected : Type.t = ThreeOfAKind in
  let result = Type.type_of_play play_2.hand in
  Type.equal result expected

let%test "Type of play (3)" =
  let expected : Type.t = TwoPair in
  let result = Type.type_of_play play_3.hand in
  Type.equal result expected

let%test "Type of play (4)" =
  let expected : Type.t = TwoPair in
  let result = Type.type_of_play play_4.hand in
  Type.equal result expected

let%test "Type of play (5)" =
  let expected : Type.t = ThreeOfAKind in
  let result = Type.type_of_play play_5.hand in
  Type.equal result expected
