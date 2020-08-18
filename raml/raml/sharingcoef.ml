(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   analysis.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Compute the coefficient for polynomials obtained by sharing of variables.
 *)

open Core

open Indices      (* index *)
open Toolbox      (* binomial *)

(* Cartisian product with tuples and sets represented by lists and arrays,
   respectively.  (A little bit unintuitive, but it works best this way with
   its usages.)

   Array.map ~f:String.of_char_list @@
     cartisian_product (List.map ["123"; "abc"; "DEF"] String.to_array) =
      [| "1aD"; "1aE"; "1aF"; "1bD"; "1bE"; "1bF"; "1cD"; "1cE"; "1cF"
       ; "2aD"; "2aE"; "2aF"; "2bD"; "2bE"; "2bF"; "2cD"; "2cE"; "2cF"
       ; "3aD"; "3aE"; "3aF"; "3bD"; "3bE"; "3bF"; "3cD"; "3cE"; "3cF" |]
 *)
let rec cartisian_product : 'a Array.t list -> 'a list Array.t = function
  | [] -> [| [] |]
  | xs :: ys -> let ys' = cartisian_product ys
                in Array.concat_map xs (fun x -> Array.map ys' (List.cons x))

(* Input:  two lists of index-constructor pairs ((index * constr_id) list)
   Output: a list of all possible interleavings where two indices can
           be at identical positions if constructors match.  It is handy to
           distinguish the the origin of the indices by using pairs where
           Iunit means empty.

   Example:
     interleave [1, c; 2, d] [3, c; 4, d] =
       [ [( 1, *, c); ( 2, *, d); ( *, 3, c); ( *, 4, d)]
       ; [( 1, *, c); ( *, 3, c); ( 2, *, d); ( *, 4, d)]
       ; [( 1, *, c); ( *, 3, c); ( *, 4, d); ( 2, *, d)]
       ; [( 1, *, c); ( *, 3, c); ( 2, 4, d)]
       ; [( *, 3, c); ( 1, *, c); ( 2, *, d); ( *, 4, d)]
       ; [( *, 3, c); ( 1, *, c); ( *, 4, d); ( 2, *, d)]
       ; [( *, 3, c); ( 1, *, c); ( 2, 4, d)]
       ; [( *, 3, c); ( *, 4, d); ( 1, *, c); ( 2, *, d)]
       ; [( 1, 3, c); ( 2, *, c); ( *, 4, d)]
       ; [( 1, 3, c); ( 4, *, d); ( 2, *, d)]
       ; [( 1, 3, c); ( 2, 4, d)]]
 *)
let interleave xs ys =
  let memo = Array.make_matrix (1 + List.length xs) (1 + List.length ys) None in
  let rec dp xn yn xs ys = match memo.(xn).(yn) with
    | Some l -> l
    | None -> let l = dp' xn yn (xs, ys) in memo.(xn).(yn) <- Some l; l
  and dp' xn yn = function
    | xs, [] -> [ List.map xs (fun (x, c) -> x, Iunit, c) ]
    | [], ys -> [ List.map ys (fun (y, c) -> Iunit, y, c) ]
    | (x, xc) :: xs as xall, ((y, yc) :: ys as yall) ->
      let x_first = List.map (dp (xn + 1) yn xs yall) (List.cons (x, Iunit, xc)) in
      let y_first = List.map (dp xn (yn + 1) xall ys) (List.cons (Iunit, y, yc)) in
      if xc <> yc then
        x_first @ y_first
      else
        let xys = List.map (dp (xn + 1) (yn + 1) xs ys) (List.cons (x, y, xc))
        in x_first @ y_first @ xys
  in dp' 0 0 (xs, ys)

(* Input: A list of pairs of indices and multipliers ((index * int) list)
   Output: A pair where the first component is the index for a tuple per input
           indices, and the second component is the product of the multipliers.
 *)
let collect_Ituple_coef ls =
  let is, n = List.fold_right ls ~init:([], 1)
                           ~f:(fun (t, c) -> fun (ts, n) -> t :: ts, c * n)
  in Ituple is, n

(* Input: A list of pairs of indices, constructors, and multipliers
          (((index * constr_id) * int) list)
   Output: A pair where the first component is the index for a user-defined
           data type per input indices, and the second component is the product
           of the multipliers.
 *)
let collect_Iind_coef ls =
  let is, n = List.fold_right ls ~init:([], 1)
                           ~f:(fun (t, c) -> fun (ts, n) -> t :: ts, c * n)
  in Iind is, n

(* Input: Two indices i, j
   Output: A map k -> c^(i,j)_k where c^(i,j)_k are positive integers such that
           p_i * p_j = sum_k c^(i,j)_k p_k

   sharing_coefficient should be called with indices of the same type only.
   It is handy to allow one argument to be Iunit regardless of the other
   argument.  The sharingCoeffs are then 1, which is equal to p_*.

   Implementation detail: sharing_coefficient' returns
   [ `Array_result Array.t | `Map_result Ind_map.t ] to minimize conversion
   costs.  One should always use sharing_coefficient_array and
   sharing_coefficient_map that handle the conversion when the caller requires
   a different kind of representation that the most effecient computation
   yields.  The exported function sharing_coefficient is equivalent to
   sharing_coefficient_map.
 *)
let rec sharing_coefficient' a b = match a, b with
  | Iunit, b -> `Array_result [| b, 1 |]
  | a, Iunit -> `Array_result [| a, 1 |]

  | Inat i', Inat j' -> let i, j = if j' > i' then j', i' else i', j' in
    `Array_result (
      Array.init (j + 1) (* k' = 0 ~ j *)
                 (fun k' -> let k = k' + i
                            in Inat k, binomial i (j - k') * binomial k k'))
    (* c^(Inat i, Inat j)_(Inat k) = C(i, i+j-k) * C(k, i).
       Exercise for the reader: prove the binomial coefficient identity

         i+j  (   i   ) ( k ) ( n )     ( n ) ( n )
        \sum  (       ) (   ) (   )  =  (   ) (   )
         k=i  ( i+j-k ) ( i ) ( k )     ( i ) ( j )
     *)

  | Ituple xs, Ituple ys ->
    `Array_result (
      Array.map (cartisian_product (List.map2_exn xs ys sharing_coefficient_array))
 collect_Ituple_coef)
   (* c^(Ituple xs, Ituple ys)_(Ituple ks) = \prod_r c^(xs[r], ys[r])_ks[r] *)

  | Iind xs, Iind ys ->
    `Map_result (
      Ind_map.of_alist_reduce ~f:(+) @@
      List.concat_map (interleave xs ys) (fun xys ->
      Array.to_list @@ Array.map ~f:collect_Iind_coef @@ cartisian_product @@
        List.map xys (fun (x, y, c) ->
          Array.map (sharing_coefficient_array x y) (fun (i, n) -> (i, c), n) ))
    )
    (* We don't have a closed form for c^(Iind xs, Iind ys)_(Iind ks).
       To compute them (for all the ks),
         1. generates all the interleavings of xs and ys
            (List.concat_map ... (fun xys -> ...));
         2. for each interleaving, each element xys[s] correspond to xs[l],
            ys[r], or both (essentially xs[l] * ys[r]), use sharing_coefficient
            to figure out the k |-> c^(xs[l], ys[r])_k mapping, or xs[l] |-> 1
            or ys[r] |-> 1 for the former two cases (List.map xys ...);
         3. instantiate from s |-> k |-> c^..._k to ks |-> [c, c, ..., c]
            by selecting one from each index s (cartisian_product);
         4. group ks into Iind while multiply all the coefficients
            (collect_Iind_coef);
         5. insert all the Ind ks |-> c into an index map with the coefficients
            of the same key summed.
     *)

  | _, _ -> failwith
    "Attempt to compute sharing coefficient on indices of different types."

and sharing_coefficient_array a b = match sharing_coefficient' a b with
  | `Array_result arr -> arr
  | `Map_result m ->  (* Array.of_list @@ Map.to_alist m *)
    let arr = Array.create (Map.length m) (Iunit, 0) in
    let i = ref 0
    in Map.iteri m ~f:(fun ~key ~data -> arr.(!i) <- (key, data); incr i);
       arr

and sharing_coefficient_map a b = match sharing_coefficient' a b with
  | `Array_result arr ->
      Array.sort arr (fun (x, _) (y, _) -> compare_index x y)
    ; Or_error.ok_exn @@ Ind_map.of_sorted_array arr
  | `Map_result m -> m

let sharing_coefficient = sharing_coefficient_map
