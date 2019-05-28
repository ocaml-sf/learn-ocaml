let rec size t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> 1 + (size l) + (size r)

let size_tests = [
  (Empty, 0);
  (t, 3);
  (Node (Empty, 1, Empty), 1);
  (Node (t, 4, Empty), 4);
  (Node (Empty, 0, t), 4);
]

let rec height t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> 1 + max (height l) (height r)

let t' = Node (Empty, 5, Empty)

let height_tests = [
  (Empty, 0);
  (Node (Empty, 1, Empty), 1);
  (t, 2);
  (Node (t, 4, Empty), 3);
  (Node (t, 4, t' ), 3);
  (Node (Empty, 0, t), 3);
  (Node (t', 4, t), 3)
]

let rec num_leaves t =
  match t with
  | Empty -> 0
  | Node (Empty, _, Empty) -> 1
  | Node (l, _, r) -> (num_leaves l) + (num_leaves r)

let num_leaves_tests = [
  (Empty, 0);
  (Node (Empty, 1, Empty), 1);
  (t, 2);
  (Node (t, 4, Empty), 2);
  (Node (t, 4, t'), 3);
  (Node (t', 4, t), 3);
]
