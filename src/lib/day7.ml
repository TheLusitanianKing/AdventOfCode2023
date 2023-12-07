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
    | HighCard of Card.t
    | OnePair of Card.t
    | TwoPair of Card.t * Card.t
    | ThreeOfAKind of Card.t
    | FullHouse of Card.t * Card.t
    | FourOfAKind of Card.t
    | FiveOfAKind of Card.t
  [@@deriving eq, ord, show]

  let to_enum = function
    | HighCard _ -> 0
    | OnePair _ -> 1
    | TwoPair _ -> 2
    | ThreeOfAKind _ -> 3
    | FullHouse _ -> 4
    | FourOfAKind _ -> 5
    | FiveOfAKind _ -> 6

  let partial_compare t1 t2 = Int.compare (to_enum t1) (to_enum t2)
  let partial_eq t1 t2 = partial_compare t1 t2 = 0

  let type_of_play (cs : Card.t list) : t =
    let grouped_cards =
      cs
      |> List.sort ~compare:Card.compare
      |> List.group ~break:(fun x y -> not @@ Card.equal x y)
      |> List.map ~f:(fun grouped_cards ->
             (List.length grouped_cards, grouped_cards))
      |> List.sort ~compare:(fun (l1, gcs1) (l2, gcs2) ->
             let len_cpt = Int.compare l1 l2 in
             if len_cpt <> 0 then len_cpt
             else Card.compare (List.hd_exn gcs1) (List.hd_exn gcs2))
      |> List.rev in
    match grouped_cards with
    | [] -> failwith "Impossible unless the hand is empty which shouldn't parse"
    | (5, gcs) :: _ ->
        let c = List.hd_exn gcs in
        FiveOfAKind c
    | (4, gcs) :: _ ->
        let c = List.hd_exn gcs in
        FourOfAKind c
    | (3, gcs) :: (2, gcs') :: _ ->
        let c1 = List.hd_exn gcs in
        let c2 = List.hd_exn gcs' in
        FullHouse (c1, c2)
    | (3, gcs) :: _ ->
        let c = List.hd_exn gcs in
        ThreeOfAKind c
    | (2, gcs) :: (2, gcs') :: _ ->
        let c1 = List.hd_exn gcs and c2 = List.hd_exn gcs' in
        if Card.compare c1 c2 > 0 then TwoPair (c1, c2) else TwoPair (c2, c1)
    | (2, gcs) :: _ ->
        let c = List.hd_exn gcs in
        OnePair c
    | (1, gcs) :: _ ->
        let c = List.hd_exn gcs in
        HighCard c
    | _ ->
        failwith
          "Impossible unless the hand has more than 5 cards, which shouldn't \
           parse"
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

(* Main *)
let main rows =
  let plays =
    rows
    |> List.map ~f:(fun r ->
           r |> Play.parse |> fun p -> (p, Type.type_of_play p.hand)) in
  let sorted_plays =
    plays
    |> List.sort ~compare:(fun ((p1: Play.t), t1) (p2, t2) ->
           let t_cpt = Type.partial_compare t1 t2 in
           if t_cpt <> 0 then t_cpt
           else List.compare Card.compare p1.hand p2.hand) in
  let ranked_plays =
    List.zip_exn
      (List.range 1 (List.length sorted_plays) ~start:`inclusive
         ~stop:`inclusive)
      sorted_plays in
  let part_1 =
    ranked_plays
    |> List.map ~f:(fun (rank, ((p : Play.t), _)) -> rank * p.bid)
    |> List.fold ~init:0 ~f:( + ) in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1

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
  let expected : Type.t = OnePair Three in
  let result = Type.type_of_play play_1.hand in
  Type.equal result expected

let%test "Type of play (2)" =
  let expected : Type.t = ThreeOfAKind Five in
  let result = Type.type_of_play play_2.hand in
  Type.equal result expected

let%test "Type of play (3)" =
  let expected : Type.t = TwoPair (King, Seven) in
  let result = Type.type_of_play play_3.hand in
  Type.equal result expected

let%test "Type of play (4)" =
  let expected : Type.t = TwoPair (Jack, Ten) in
  let result = Type.type_of_play play_4.hand in
  Type.equal result expected

let%test "Type of play (5)" =
  let expected : Type.t = ThreeOfAKind Queen in
  let result = Type.type_of_play play_5.hand in
  Type.equal result expected
