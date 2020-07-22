let rec fast_exp x k =
  if k = 0 then 1
  else if even k then fast_exp (x * x) (k / 2)
  else
    let n = fast_exp x (k - 1) in
    x * n

let rec fast_exp_aux x k acc =
  if k = 0 then acc
  else if even k then fast_exp_aux (x * x) (k / 2) acc
  else fast_exp_aux x (k - 1) (x * acc)

let fast_exp_tl x k =
  fast_exp_aux x k 1
