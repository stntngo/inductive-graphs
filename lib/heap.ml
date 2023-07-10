module type Ordered = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module Make (O : Ordered) = struct
  module Elem = O

  type tree = Node of int * Elem.t * tree list
  type heap = tree list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t = function
    | [] -> [ t ]
    | t' :: ts' as ts ->
        if rank t < rank t' then t :: ts else ins_tree (link t t') ts'

  let insert x ts = ins_tree (Node (0, x, [])) ts

  let rec merge ts1 ts2 =
    match (ts1, ts2) with
    | ts1, [] -> ts1
    | [], ts2 -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: merge ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
        else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree = function
    | [] -> failwith "empty"
    | [ t ] -> (t, [])
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then (t, ts) else (t', t :: ts')

  let find_min ts =
    let t, _ = remove_min_tree ts in
    root t

  let delete_min ts =
    let Node (_, _, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2
end

module FloatHeap = Make (struct
  type t = float

  let eq = Float.equal
  let lt x y = Float.compare x y < 0
  let leq x y = Float.compare x y <= 0
end)

let h =
  [ 3.0; 4.0; 2.0; 1.0; 0.0; 3.0; 4.0; 5.0 ]
  |> List.fold_left (Fun.flip FloatHeap.insert) FloatHeap.empty

let%test_unit "find_min" =
  let open Base in
  [%test_eq: float] (FloatHeap.find_min h) 0.0;
  let h = FloatHeap.delete_min h in
  [%test_eq: float] (FloatHeap.find_min h) 1.0;
  let h = FloatHeap.delete_min h in
  [%test_eq: float] (FloatHeap.find_min h) 2.0;
  let h = FloatHeap.delete_min h in
  [%test_eq: float] (FloatHeap.find_min h) 3.0;
  let h = FloatHeap.delete_min h in
  [%test_eq: float] (FloatHeap.find_min h) 3.0;
  let h = FloatHeap.delete_min h in
  [%test_eq: float] (FloatHeap.find_min h) 4.0;
  let h = FloatHeap.delete_min h in
  [%test_eq: float] (FloatHeap.find_min h) 4.0
