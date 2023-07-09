module Natural = struct
  (* Natural.t defines the natural numbers as an inductive data type *)
  type t = Zero | Succ of t

  (* of_int constructs the inductive natural type by recursively accumulating
     calls to the Succ constructor until we reach our base case of 0 *)
  let rec of_int = function 0 -> Zero | n -> Succ (of_int (n - 1))

  (* to_int converts the inductive natural type to an integer by effectively
     recursively counting the number of times we see a Succ constructor until
     we hit the Zero variant of the natural type *)
  let rec to_int = function Zero -> 0 | Succ n -> 1 + to_int n

  (* inc adds one to the natural type n by wrapping n in a Succ constructor *)
  let inc n = Succ n

  (* dec substracts one from the natural type n by unwrapping the inner inductive
     natural type. In the church encoding dec Zero == Zero so we've kept that
     behavior here as well. *)
  let dec n = match n with Zero -> Zero | Succ n -> n

  (* plus implements addition for our inductive natural type.
     Expressed using integers this function boils down to
     m + n = (m + 1) + (n - 1) with a special case for
     m + 0 = m *)
  let rec plus m n = match n with Zero -> m | Succ n' -> plus (inc m) n'

  (* multiply implements multiplication for our inductive natural type.
     Expressed using integers this function boils down to
     m * n = (m * (n - 1)) + m with a special case for
     m * 0 = m *)
  let rec multiply m n =
    match n with Zero -> Zero | Succ n' -> plus (multiply m n') m

  (* minus implements subtraction for our inductive natural type.
     Expressed using integers this function boils down to
     m - n = (m - 1) - (n - 1) with a special case for
     m - 0 = m *)
  let rec minus m n = match n with Zero -> m | Succ n' -> minus (dec m) n'

  (* < implements a less than comparator for our inductive natural type.
     It has three branches:
             1. If n == 0, either m is 0 as well or m is greater than 0 as Zero is the
                smallest possible natural number. Therefore m < 0 == false.
             2. If m == 0 n must be > 0 since the n == 0 case is handled by the first
                branch. Therefore 0 < n where n > 0 == true
             3. If neither number is zero, then we can still compare them by recursively
                calling (m - 1) < (m - 1) because if m < n then so to is (m - 1) < (n - 1) *)
  let rec ( < ) m n =
    match (m, n) with
    | _, Zero -> false
    | Zero, Succ _ -> true
    | _ -> dec m < dec n

  (* == implements an equality comparator for our inductive natural type. Like < it
     has three(ish) branches:
             1. If m == 0 and n == 0, these two values are equal.
             2. If m == 0 and n does not or n == 0 and m does not, then those two values
                are not equal.
             3. If neither m nor n is zero, then we compare (m - 1) == (n - 1) as if
                m == n then m - 1 == n - 1 as well. *)
  let rec ( == ) m n =
    match (m, n) with
    | Zero, Zero -> true
    | Succ _, Zero -> false
    | Zero, Succ _ -> false
    | Succ m', Succ n' -> m' == n'

  (* divide implements integer divison for our inductive natural type.
     Expressed using integers this function boils down to
     m / n = 1 + ((m - n) / n) with a special case for
     m / n = 0 when m < n *)
  let rec divide m n = if m < n then Zero else inc (divide (minus m n) n)

  (* rem implements a remainder operator for our inductive natural type.
     Expressed using integers this function boils down to
     rem m n = rem (m - n) n with a special case for
     rem m n = m when m < n *)
  let rec rem m n = if m < n then m else rem (minus m n) n

  (* is_event returns whether an inductive natural type is zero by checking
     whether the remainder of rem n 2 == 0 *)
  let is_even n = match rem n (inc (inc Zero)) with Zero -> true | _ -> false

  let%test_unit "of_int/to_int" =
    let open Base in
    [%test_eq: int] (to_int (of_int 0)) 0;
    [%test_eq: int] (to_int (of_int 10)) 10

  let%test_unit "inc/dec" =
    let x = of_int 5 in
    let open Base in
    [%test_eq: int] (to_int (inc x)) 6;
    [%test_eq: int] (to_int (dec x)) 4

  let%test_unit "plus" =
    let x = of_int 5 in
    let y = of_int 6 in
    let open Base in
    [%test_eq: int] (to_int (plus x y)) 11

  let%test_unit "multiply" =
    let x = of_int 5 in
    let y = of_int 6 in
    let open Base in
    [%test_eq: int] (to_int (multiply x y)) 30

  let%test_unit "divide" =
    let x = of_int 10 in
    let y = of_int 2 in
    let z = of_int 8 in
    let open Base in
    [%test_eq: int] (to_int (divide x y)) 5;
    [%test_eq: int] (to_int (rem x z)) 2

  let%test_unit "minus" =
    let x = of_int 6 in
    let y = of_int 5 in
    let open Base in
    [%test_eq: int] (to_int (minus x y)) 1

  let%test_unit "less_than" =
    let x = of_int 6 in
    let y = of_int 5 in
    let x_lt_y = x < y in
    let y_lt_x = y < x in
    let open Base in
    [%test_eq: bool] x_lt_y false;
    [%test_eq: bool] y_lt_x true

  let%test_unit "is_even" =
    let open Base in
    [%test_eq: bool] (is_even (of_int 5)) false;
    [%test_eq: bool] (is_even (of_int 26)) true
end
