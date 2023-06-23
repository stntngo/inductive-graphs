module Natural = struct
  type t = Zero | Succ of t

  let rec of_int = function 0 -> Zero | n -> Succ (of_int (n - 1))
  let rec to_int = function Zero -> 0 | Succ n -> 1 + to_int n
  let inc n = Succ n

  let dec n =
    match n with
    | Zero -> failwith "natural numbers cannot go below zero"
    | Succ n -> n

  let rec plus m n = match n with Zero -> m | Succ n -> plus (inc m) n

  let multiply m n =
    let rec aux m n acc =
      match n with Zero -> acc | Succ n' -> aux m n' (plus acc m)
    in
    aux m n Zero

  let rec minus m n = match n with Zero -> m | Succ n' -> minus (dec m) n'
end

let%test_unit "of_int/to_int" =
  let open Base in
  [%test_eq: int] (Natural.to_int (Natural.of_int 0)) 0;
  [%test_eq: int] (Natural.to_int (Natural.of_int 10)) 10

let%test_unit "inc/dec" =
  let x = Natural.of_int 5 in
  let open Base in
  [%test_eq: int] (Natural.to_int (Natural.inc x)) 6;
  [%test_eq: int] (Natural.to_int (Natural.dec x)) 4

let%test_unit "plus" =
  let x = Natural.of_int 5 in
  let y = Natural.of_int 6 in
  let open Base in
  [%test_eq: int] (Natural.to_int (Natural.plus x y)) 11

let%test_unit "multiply" =
  let x = Natural.of_int 5 in
  let y = Natural.of_int 6 in
  let open Base in
  [%test_eq: int] (Natural.to_int (Natural.multiply x y)) 30

let%test_unit "minus" =
  let x = Natural.of_int 6 in
  let y = Natural.of_int 5 in
  let open Base in
  [%test_eq: int] (Natural.to_int (Natural.minus x y)) 1
