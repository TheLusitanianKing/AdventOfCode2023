open Base

let rec gcd a b = if b = 0 then a else gcd b (Int.rem a b)
let lcm a b = a * b / gcd a b
let lcm_multi is = is |> List.reduce_exn ~f:lcm
let%test "GCD" = gcd 54 24 = 6
let%test "GCD" = gcd 48 18 = 6
let%test "LCM" = lcm 4 6 = 12
let%test "LCM" = lcm 21 6 = 42
let%test "LCM multi" = lcm_multi [ 8; 9; 21 ] = 504
