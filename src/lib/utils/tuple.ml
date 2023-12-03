let compare_tuple cmp1 cmp2 (x, y) (x', y') =
  let x_cmp = cmp1 x x' in
  if x_cmp <> 0 then x_cmp else cmp2 y y'

let compare_tuple_simple cmp = compare_tuple cmp cmp
let equal_tuple cmp1 cmp2 t1 t2 = compare_tuple cmp1 cmp2 t1 t2 = 0
let equal_tuple' cmp = equal_tuple cmp cmp
