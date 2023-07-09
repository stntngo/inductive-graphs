module Composition = struct
  let ( @ ) f f' x = f' (f x)

  let double_strings l =
    l |> List.map int_of_string |> List.map (( * ) 2) |> List.map string_of_int

  let double_strings' = List.map ((int_of_string @ ( * ) 2) @ string_of_int)

  let%test_unit "fusion composition" =
    let open Base in
    [%test_eq: string list]
      (double_strings [ "0"; "1"; "2"; "3" ])
      [ "0"; "2"; "4"; "6" ];
    [%test_eq: string list]
      (double_strings' [ "0"; "1"; "2"; "3" ])
      [ "0"; "2"; "4"; "6" ];
    [%test_eq: string list]
      (double_strings' [ "0"; "1"; "2"; "3" ])
      (double_strings [ "0"; "1"; "2"; "3" ])
end
