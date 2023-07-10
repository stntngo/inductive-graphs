let ( @ ) f f' x = f' (f x)

type node = int
type 'b adj = ('b * node) list
type ('a, 'b) context = 'b adj * node * 'a * 'b adj
type path = node list
type path_tree = path list
type 'a lnode = node * 'a
type 'a lpath = 'a lnode list
type 'a lrtree = 'a lpath list

module type Graph = sig
  type ('a, 'b) t

  val empty : ('a, 'b) t
  val is_empty : ('a, 'b) t -> bool
  val ( & ) : ('a, 'b) context -> ('a, 'b) t -> ('a, 'b) t
  val match_any : ('a, 'b) t -> ('a, 'b) context * ('a, 'b) t
  val match_node : node -> ('a, 'b) t -> ('a, 'b) context option * ('a, 'b) t
end

module Make (G : Graph) = struct
  module H = Heap.Make (struct
    type t = float lpath

    let eq (x : t) (y : t) =
      match (x, y) with
      | (_, a1) :: _, (_, a2) :: _ -> Float.equal a1 a2
      | _ -> false

    let lt (x : t) (y : t) =
      match (x, y) with
      | (_, a1) :: _, (_, a2) :: _ -> Float.compare a1 a2 < 0
      | _ -> false

    let leq (x : t) (y : t) =
      match (x, y) with
      | (_, a1) :: _, (_, a2) :: _ -> Float.compare a1 a2 <= 0
      | _ -> false
  end)

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

  let esp s t g =
    bft s g
    |> List.find (fun vs -> match vs with v :: _ -> v == t | _ -> false)
    |> List.rev

  let get_path (v : node) (t : 'a lrtree) : path =
    List.find (function (w, _) :: _ -> w == v | _ -> false) t
    |> List.map fst |> List.rev

  let expand (d : float) (p : float lpath) ((_, _, _, s) : ('a, float) context)
      =
    List.map (fun (l, v) -> H.unit (l +. d) ((v, l +. d) :: p)) s

  let rec dijkstra (h : H.heap) (g : ('a, float) t) : float lrtree =
    if H.is_empty h || is_empty g then []
    else
      let Node (_, p, _), h' = H.remove_min_tree h in
      match p with
      | [] -> dijkstra h' g
      | (v, d) :: _ -> (
          match match_node v g with
          | Some c, g' -> p :: dijkstra (H.merge h' (expand d p c)) g'
          | None, g' -> dijkstra h' g')
end
