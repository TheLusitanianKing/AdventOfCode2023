open Base

exception Could_not_find_point_in_matrix

let find_point (matrix : 'a array array) ~f : int * int =
  matrix
  |> Array.find_mapi ~f:(fun row_i column ->
         column
         |> Array.find_mapi ~f:(fun column_i p ->
                if f p then Some column_i else None)
         |> Option.bind ~f:(fun column_i -> Some (column_i, row_i)))
  |> function
  | None -> raise Could_not_find_point_in_matrix
  | Some c -> c

exception Could_not_get_all_coordinates_from_matrix

let all_coordinates matrix =
  try
    let y_max = Array.length matrix in
    let x_max = Array.length matrix.(0) in
    Array.cartesian_product
      (List.range 0 x_max ~start:`inclusive ~stop:`exclusive |> List.to_array)
      (List.range 0 y_max ~start:`inclusive ~stop:`exclusive |> List.to_array)
  with Invalid_argument _ -> raise Could_not_get_all_coordinates_from_matrix
