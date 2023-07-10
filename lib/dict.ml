module type Dict = sig
  type key
  type 'a dict

  val empty : 'a dict
  val lookup : key -> 'a dict -> 'a option
  val insert : ('a * 'a -> 'a) -> key -> 'a -> 'a dict -> 'a dict
  val merge : ('a * 'a -> 'a) -> 'a dict -> 'a dict -> 'a dict
end

module BinaryTrie = struct
  type key = int [@@deriving show]

  type 'a dict = Empty | Lf of key * 'a | Br of key * key * 'a dict * 'a dict
  [@@deriving show]

  let empty = Empty

  let mask k m =
    let k' = k land (m - 1) in
    k'

  let match_prefix k p m = mask k m == p
  let lowest_bit x = x land -x
  let branching_bit p q = lowest_bit (p lxor q)
  let zero_bit k m = k land m == 0

  let rec lookup key d =
    match (key, d) with
    | _, Empty -> None
    | k, Lf (j, x) -> if j == k then Some x else None
    | k, Br (p, m, a, b) ->
        if not (match_prefix k p m) then None
        else if zero_bit k m then lookup k a
        else lookup k b

  let br = function
    | _, _, Empty, t -> t
    | _, _, t, Empty -> t
    | p, m, a, b -> Br (p, m, a, b)

  let join p0 t0 p1 t1 =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then Br (mask p0 m, m, t0, t1)
    else Br (mask p0 m, m, t1, t0)

  let insert c k v t =
    let rec ins = function
      | Empty -> Lf (k, v)
      | Lf (j, y) as t ->
          if j == k then Lf (k, c (v, y)) else join k (Lf (k, v)) j t
      | Br (p, m, t0, t1) as t ->
          if match_prefix k p m then
            if zero_bit k m then Br (p, m, ins t0, t1) else Br (p, m, t0, ins t1)
          else join k (Lf (k, v)) p t
    in
    ins t

  let merge c s t =
    let swap (x, y) = (y, x) in
    let ( << ) f g x = f (g x) in
    let rec mrg = function
      | Empty, t -> t
      | t, Empty -> t
      | Lf (k, v), t -> insert c k v t
      | t, Lf (k, v) -> insert (c << swap) k v t
      | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
          if m == n && p == q then Br (p, m, mrg (s0, t0), mrg (s1, t1))
          else if m < n && match_prefix q p m then
            if zero_bit q m then Br (p, m, mrg (s0, t), s1)
            else Br (p, m, s0, mrg (s1, t))
          else if m > n && match_prefix p q n then
            if zero_bit p n then Br (q, n, mrg (s, t0), t1)
            else Br (q, n, t0, mrg (s, t1))
          else join p s q t
    in
    mrg (s, t)

  let remove k t =
    let rec rmv = function
      | Empty -> (None, Empty)
      | Lf (j, v) as t -> if j == k then (Some v, Empty) else (None, t)
      | Br (p, m, left, right) as t ->
          if match_prefix k p m then
            if zero_bit k m then
              let node, left = rmv left in
              (node, br (p, m, left, right))
            else
              let node, right = rmv right in
              (node, br (p, m, left, right))
          else (None, t)
    in
    rmv t

  let from_list l = List.fold_left (fun d (k, v) -> insert fst k v d) empty l

  let to_list d =
    let rec lst = function
      | Empty -> []
      | Lf (k, v) -> [ (k, v) ]
      | Br (_, _, l, r) -> List.append (lst l) (lst r)
    in
    lst d

  let fold f init d =
    let rec aux init = function
      | Empty -> init
      | Lf (k, v) -> f init (k, v)
      | Br (_, _, l, r) -> aux (aux init l) r
    in
    aux init d

  let map f d = fold (fun a e -> Seq.cons (f e) a) Seq.empty d

  let%test_unit "construct and retrieve" =
    let in' = [ (1, "a"); (2, "b"); (3, "c"); (4, "d"); (5, "e") ] in
    let out' = List.map snd in' in
    let d = from_list in' in
    let got = List.map (fun (k, _) -> Option.get (lookup k d)) in' in
    let open Base in
    [%test_eq: string list] got out'

  let%test_unit "delete node" =
    let in' = [ (1, "a"); (2, "b"); (3, "c"); (4, "d"); (5, "e") ] in
    let d = from_list in' in
    let open Base in
    let got_deleted, d = remove 2 d in
    [%test_eq: string option] got_deleted (Some "b");
    let got_missing = lookup 2 d in
    [%test_eq: string option] got_missing None;
    let got_there = lookup 5 d in
    [%test_eq: string option] got_there (Some "e")
end
