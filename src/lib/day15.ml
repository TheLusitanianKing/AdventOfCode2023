open Base

module Hash = struct
  let get s =
    s |> String.to_list
    |> List.fold ~init:0 ~f:(fun acc x ->
           let x' = (acc + Char.to_int x) * 17 in
           Int.rem x' 256)
end

let main rows =
  let part_1 =
    rows |> List.hd_exn |> String.split ~on:',' |> List.map ~f:Hash.get
    |> List.fold ~init:0 ~f:( + ) in
  Stdio.print_endline @@ Printf.sprintf "Part 1: %d" part_1
