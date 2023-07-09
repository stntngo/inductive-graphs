let ( @ ) f f' x = f' (f x)

type node = int
type 'b adj = ('b * node) list
type ('a, 'b) context = 'b adj * node * 'a * 'b adj
type path = node list
type path_tree = path list

module type Graph = sig
  type ('a, 'b) t

  val empty : ('a, 'b) t
  val is_empty : ('a, 'b) t -> bool
  val ( & ) : ('a, 'b) context -> ('a, 'b) t -> ('a, 'b) t
  val match_any : ('a, 'b) t -> ('a, 'b) context * ('a, 'b) t
  val match_node : node -> ('a, 'b) t -> ('a, 'b) context option * ('a, 'b) t
end

module Make (G : Graph) : sig
  include Graph

  val nodes : ('a, 'b) t -> node list
  val topsort : ('a, 'b) t -> node list
  val bft : node -> ('a, 'b) t -> path_tree
  val esp : node -> node -> ('a, 'b) t -> path
end = struct
  include G

  type 'a tree = Br of 'a * 'a tree list

  let rec ufold f u g =
    if is_empty g then u
    else
      let c, g = match_any g in
      f c (ufold f u g)

  let gmap f = ufold (fun c u -> f c & u) empty

  let grev g =
    let swap (p, v, l, s) = (s, v, l, p) in
    gmap swap g

  let nodes g = ufold (fun (_, v, _, _) u -> v :: u) [] g
  let suc (_, _, _, s) = List.map snd s

  let rec dfs vs g =
    match vs with
    | [] -> []
    | v :: vs' -> (
        match match_node v g with
        | Some c, g' -> v :: dfs (List.append (suc c) vs') g'
        | _ -> dfs vs g)

  let rec postorder (Br (v, ts)) =
    List.append (List.concat_map postorder ts) [ v ]

  let rec df vs g =
    match vs with
    | [] -> ([], g)
    | v :: vs -> (
        match match_node v g with
        | Some c, g ->
            let f, g1 = df (suc c) g in
            let f', g2 = df vs g1 in
            (Br (v, f) :: f', g2)
        | _ -> df vs g)

  let dff vs g = fst (df vs g)
  let topsort g = ((dff (nodes g) @ List.concat_map postorder) @ List.rev) g
  let scc g = dff (topsort g) (grev g)

  let topsort' g =
    (* Compute the depth first spanning forests *)
    dff (nodes g) g
    (* Traverse each tree in postorder and concatenate orders into single output *)
    |> List.concat_map postorder
    (* The order of the sorted list is in the wrong direction so reverse it *)
    |> List.rev

  let rec bfs vs g =
    match vs with
    | [] -> []
    | v :: vs' -> (
        match match_node v g with
        | Some c, g' -> v :: bfs (List.append vs' (suc c)) g'
        | _ -> bfs vs g)

  let rec bf ps g =
    match ps with
    | [] -> []
    | (v :: _ as p) :: ps -> (
        match match_node v g with
        | Some c, g ->
            p :: bf (List.append ps (List.map (fun q -> q :: p) (suc c))) g
        | None, g -> bf ps g)
    | _ :: ps -> bf ps g

  let bft v = bf [ [ v ] ]

  let rec bf ps g =
    match ps with
    | [] -> []
    | (v :: _ as p) :: ps -> (
        match match_node v g with
        | Some c, g ->
            p :: bf (List.append ps (List.map (fun q -> q :: p) (suc c))) g
        | None, g -> bf ps g)
    | _ :: ps -> bf ps g

  let bft v = bf [ [ v ] ]
  let first p = List.find p

  let esp s t g =
    bft s g
    |> List.find (fun vs -> match vs with v :: _ -> v == t | _ -> false)
    |> List.rev
end
