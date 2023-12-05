open Base

let int_of_string_or_none x = try Some (Int.of_string x) with _ -> None
