open Base

let split (l : 'a list) ~f : 'a list list =
  let rec helper acc tmp = function
    | [] -> List.rev (List.rev tmp :: acc)
    | x :: xs when f x -> helper (List.rev tmp :: acc) [] xs
    | x :: xs -> helper acc (x :: tmp) xs in
  helper [] [] l

let%test "Split" =
  let expected = [ [ 1; 2 ]; [ 2; 1; 1; 2 ]; [] ] in
  let result = split ~f:(Int.equal 3) [ 1; 2; 3; 2; 1; 1; 2; 3 ] in
  List.equal (List.equal Int.equal) result expected

let%test "Split (2)" =
  let expected = [ [ 1; 2 ]; [ 2; 1; 1; 2 ] ] in
  let result = split ~f:(Int.equal 3) [ 1; 2; 3; 2; 1; 1; 2 ] in
  List.equal (List.equal Int.equal) result expected

let%test "Split (3)" =
  let expected = [ [] ] in
  let result = split ~f:(Bool.equal true) [] in
  List.equal (List.equal Bool.equal) result expected
