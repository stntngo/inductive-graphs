module InductiveList = struct
  (* Inductive definition of our list type. The 'a argument essentially
     represents generic type parameter in our constructor and 'a * 'a t
     represents a two element tuple in the ocaml type system.
     Our list type is either the special constructor Nil which holds no other
     elements in it, or it is Cons which holds a tuple whose first element is
     some object of type 'a and whose second object is our list object holding
     elements of type 'a. So either the second element will be Nil, or it will be
     another tuple holding an element of the list and the remainder of the list.

     Imagine the array [1, 2, 3]. We can express this as a `int t` type. The generic
     type 'a has been made concrete into the int type. Our list's representation of
     this same data would look something like Cons (1, Cons (2, Cons (3, Nil)))

     As you might have noticed this is in effect a linked list.*)
  type 'a t = Nil | Cons of 'a * 'a t

  (* empty might seem a little pointless now but it will make sense later on. It defines
     which of the constructors is the special "Empty List" element *)
  let empty = Nil

  (* is_empty returns whether a provided list is empty or not *)
  let is_empty = function Nil -> true | _ -> false

  (* hd returns the first element of the list *)
  let hd = function Nil -> failwith "hd" | Cons (a, _) -> a

  (* tl returns a list representing every element _but_ the first element of the list *)
  let tl = function Nil -> failwith "tl" | Cons (_, l) -> l

  (* cons adds a new element as the first element in the list. Its name comes from lisp and
     cons cells. The definition is the same in the actual List implementation of ocaml. *)
  let cons a l = Cons (a, l)
end

module List (L : sig
  (* Using the same module signature we defined above we can create an abstract "interface" to build
     larger list constructs on top of. This lets us use all of the power of the inductively defined
     List type -- specifically being able to break up a list into its hd and tl and being able to
     combine a new element and an existing list into a new list -- while still leaving us with
     the opportunity to explore potential improvements by exposing an inductive interface on top of
     a more exotic underlying data structure. *)
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
end) =
struct
  include L

  let rec append l1 l2 =
    if is_empty l1 then l2 else cons (hd l1) (append (tl l1) l2)

  let rec fold_left f u l =
    if is_empty l then u else fold_left f (f u (hd l)) (tl l)

  let reduce = fold_left

  let filter pred l =
    fold_left (fun u e -> if pred e then cons e u else u) empty l

  let rec fold_right f l u =
    if is_empty l then u else f (hd l) (fold_right f (tl l) u)

  let map f l = fold_right (fun e u -> cons (f e) u) l empty

  let iter f l =
    fold_left
      (fun u a ->
        let _ = f a in
        u)
      () l

  let length l = fold_left (fun u _ -> 1 + u) 0 l

  let nth_opt l n =
    if n < 0 then None
    else
      let rec aux l n =
        if is_empty l then None
        else if n == 0 then Some (hd l)
        else aux (tl l) (n - 1)
      in
      aux l n

  let nth l n = match nth_opt l n with None -> failwith "nth" | Some a -> a
  let for_all pred l = fold_left ( && ) true (map pred l)
  let exists pred l = fold_left ( || ) false (map pred l)

  let%test_unit "map" =
    let rec of_list l =
      match l with [] -> empty | a :: l -> cons a (of_list l)
    in
    let rec to_list l = if is_empty l then [] else hd l :: to_list (tl l) in
    let l' = of_list [ 0; 1; 2; 3; 4 ] in
    let open Base in
    [%test_eq: int list] (to_list (map (( + ) 1) l')) [ 1; 2; 3; 4; 5 ]
end

module ArrayList = List (struct
  type 'a t = 'a array

  let empty = [||]
  let is_empty = function [||] -> true | _ -> false
  let hd l = l.(0)
  let tl l = Array.sub l 1 (Array.length l - 1)
  let cons a l = Array.append [| a |] l
end)

module InductiveList' = List (InductiveList)
