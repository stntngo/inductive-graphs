# Inductive Graphs and Functional Graph Algorithms

Martin Erwig

Oregon State University, 2001

---

## Caveats

- I'm going to be showing a lot of `ocaml` in this presentation.
  - If any of the syntax is confusing let me know and I'll do my best to explain it.
- This paper is written by a Functional Programming True Believer.
  - While I agree with many of his points on the benefits of functional programming, I am not quite as aligned with him where he denigrates imperative programming.
- We're gonna end up getting into some wonkish areas of mathematics. Feel free to let your eyes glaze over when we get there.

---

# Inductive Graphs and Functional Graph Algorithms
### Abstract

> "... a new style of writing graph algorithms in functional languages ... based on an alternative view of graphs as inductively defined data types."

> "[This model] can be implemented efficiently, ... and [these] graph algorithms can be succinctly given by recursive function definitions based on the inductive graph view."

---

# Inductive Graphs and Functional Graph Algorithms
### Abstract

> "... a new style of writing graph algorithms in functional languages ... based on an alternative view of graphs as inductively defined data types."

> "[This model] can be implemented efficiently, ... and [these] graph algorithms can be succinctly given by recursive function definitions based on the inductive graph view."

### 1. Intro

> "How should I implement a graph algorithm in a functional programming language?"

```
Erwig admits this is not really that difficult of a question.

You can always hack something together and get your algorithm working regardless of the language.
```

---

# Inductive Graphs and Functional Graph Algorithms
### Abstract

> "... a new style of writing graph algorithms in functional languages ... based on an alternative view of graphs as inductively defined data types."

> "[This model] can be implemented efficiently, ... and [these] graph algorithms can be succinctly given by recursive function definitions based on the inductive graph view."

### 1. Intro

> "How should I implement a graph algorithm in a functional programming language?"

```
Erwig admits this is not really that difficult of a question.

You can always hack something together and get your algorithm working regardless of the language.
```

> "The real challenge is to obtain *clear* and *efficient* programs..."

> "...programs that do not lose their elegance and simplicity [while still having] the same asymptotic complexity as imperative ones."

> "The main difficulties arise when dealing ... with the fact that a node might be reachable via different edges, whereas the algorithm requires that it must be visited at most once."

> "In traditional descriptions of graph algorithms and in imperative languages this behavior is achieved by ... marking a node as ... 'visited' after it has been encountered the first time. When a node is reached again, checking this mark prevents the algorithm re-processing it."

---

# Inductive Graphs and Functional Graph Algorithms
### 1. Intro (Cont.)

> "In traditional descriptions of graph algorithms and in imperative languages this behavior is achieved by ... marking a node as ... 'visited' after it has been encountered the first time. When a node is reached again, checking this mark prevents the algorithm re-processing it."

```go
// Reachable performs a breadth-first search to determine
// whether a path exists connecting the provided start
// and target nodes.
func Reachable(start, target Node) bool {
  visited := make(map[int]bool)

  queue := []Node{start}

  var node Node
  for len(queue) > 0 {
    node, queue = queue[0], queue[1:]

    visited[node.ID()] = true

    if node.ID() == target.ID() {
      return true
    }

    for _, neighbor := range node.Neighbors() {
      if _, seen := visited[node.ID()]; !seen {
        queue = append(queue, neighbor)
      }
    }
  }

  return false
}
```

---

# Inductive Graphs and Functional Graph Algorithms
### 1. Intro (Cont.)

> "This node-marking strategy can easily be mimicked in functional languages: remember visited nodes in a data structure and pass this data structure through all function calls that occur in the context of the algorithm."

```
let reachable g start target =
  let rec aux queue visited =
    match queue with
    | [] -> false
    | node :: queue ->
        if node == target then true
        else
          let neighbors =
            List.filter (fun n -> not (Set.mem n)) (Graph.neighbors node)
          in
          aux (List.append queue neighbors) (Set.add visited node)
  in
  aux [ start ] Set.empty
```

> "This approach bears two problems with regard to efficiency and clarity..."

> "[using mutable data structures] testing for a node mark can be performed in constant time ... [using immutable functional data structures] set insertion and membership test take *O(log n)* time."

> "... the threading of data structures requires all participating functions to have an additional parameter for passing the state around ... [affecting] the readability of the algorithms ... [and] the ease of manipulating programs and proving program properties."

---

# Inductive Graphs and Functional Graph Algorithms
### 3. Inductive Graphs

> "The prevailing view of graphs in programming is that of a monolithic block: ... a graph is viewed as a pair *G = (V, E)* where *V* is a set of nodes and *E ⊆ V x V*."

> "Algorithms that work iteratively on [this view of] graphs ... then need an additional data structure for remembering the parts of 
> the graph that have already been dealt with."

> "This node marking strategy reflects an inherently imperative style of algorithms ... [and] moreover it complicates
> correctness proofs and program transformations considerably."

> "This is in contrast to lists or tree algorithms that have beautiful and simple definitions not needing additional bookkeeping..."

> "The reason is that lists and trees are __inductively defined data types__ and function definitions which can follow [their data type definition] are inductive in style too."

---

# Inductive Graphs and Functional Graph Algorithms
### (Aside) Inductive Data Types

```
What is an inductive data type?
```

> Mathematical induction proves that we can climb as high as we like on a ladder, by proving that we can climb onto the bottom rung (the basis) and that from each rung we can climb up to the next one (the step).

-- *Concrete Mathematics* (stolen from Wikipedia)

1. Prove the statement for the basis case of *n = 0*.
2. Assume the statement holds true for some case *n = k*.
3. Prove that if the statement holds true for *n = k* it must also hold true for *n = k + 1*.
4. The truth of the statement has now been proven for all *n ∈ ℕ*.
5. Somehow connect back proof by induction to Type Theory in Programming Languages.

```
While mathematical induction is concerned with "cases" of some theorem, we are concerned with instances of some data type. 
And while mathematical induction wants to know how to prove the truth of each case of that theorem, we want to know how to
construct each instance of that data type.
```

| Math                          | Code                                         |
| ----------------------------- | -------------------------------------------- |
| 0+1+2+...+n = n(n+1)/2        | type 'a list                                 |
| 0 = 0(0+1)/2                  | Empty                                        |
| Proof of Inductive Step       | Cons of 'a * 'a list                         |
| QED                           | type 'a list = Empty or Cons of 'a * 'a list |

---

# Inductive Graphs and Functional Graph Algorithms
### (Aside) Inductive Data Types (Cont.)

Inductive Data Types are also sometimes called *recursive data types* since the definition of *T(n+1)* is built on top of the definition of *T(n)* and both constructors produce the same data type.

```
type 'a list = Empty | Cons of 'a * 'a list
type 'a tree = Leaf of 'a | Branch of 'a tree list
```

Often times the inductive representation of the data type is sufficiently performant. But even when it isn't, we can still leverage the *API* of an inductive data type and reap all the same benefits.

Take the linked list example above, what are the operations we can perform on `type 'a list`?

```
(* We can insert a new value v at the front of the list l *)
let insert l v = Cons (v l)

(* We can separate the front of the list from the back of the list *)
let head l = 
  match l with
  | Empty -> failwith "empty list"
  | Cons (v, _) -> v

(* We can separate the back of the list from the front *)
let tail l = 
  match l with
  | Empty -> failwith "empty list"
  | Cons (_, l) -> l

(* We can provide a function for producing an empty list *)
let empty () = Empty

(* We can provide a function for checking whether a given list is empty *)
let is_empty l =
  match l with
  | Empty -> true
  | _ -> false
```

---

# Inductive Graphs and Functional Graph Algorithms
### (Aside) Inductive Data Types (Cont.)

If we are given a module that satisfies the API of our inductive data type, we can perform *any* operation on that module that we're capable of performing on our linked list data type.

```
module type List = sig
  type 'a t

  val empty : () -> 'a t
  val is_empty : 'a t -> bool

  val insert : 'a t -> 'a -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
end
```

As a result we get all the performance benefits under the hood, but retain all the ergonomic benefits of the inductive data type above.

Given their structure, our inductive data types are also closely related to *Monoids*.

> "A monoid is a set equipped with an associative binary operation and an identity element."

-- Wikpedia again

```
module type Foo = sig
  type 'a t

  val empty : 'a t
  val insert : 'a t -> 'a -> 'a t

  val multiply : 'a t -> 'a t -> 'a t
end

(* So long as Foo.multiply empty foo == foo and 
   (Foo.multiply (Foo.multiply foo1 foo2) foo3) == (Foo.multiply foo1 (Foo.multiply foo2 foo3))
   in our implementation then Foo is also a monoid *)
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.1 Graph Constructors

> "A graph is either the empty graph or a graph extended by a new node *v* together with its label and with edges to those of *v*'s successors and predecessors that are already in the graph."

```
type node = int
type 'b adj = ('b * node) list
type ('a, 'b) context = ('b adj *  node * 'a * 'b adj)
```

- A `node` is identified by an integer ID.
- A `'b adj` is a list of adjacent edges with an associated label of type `'b'`.
- A `('a, 'b) context` is a full record of information about a node, its inflowing predecessor edges, its ID, its label of type `'a`, and its outflowing successor edges.

Using *(&)* infix notation, our inductive constructor for a graph reflects the observed definition quoted above.

```
type ('a, 'b) graph = Empty | ('a, 'b) context & ('a, 'b) graph
```

An example of a `(string, char) graph`.
```
 ([("down", 2)], 3, 'c', [("up", 1)])   &
([("right", 1)], 2, 'b', [("left", 1)]) &
            ([], 1, 'a', [])            & Empty
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.1 Graph Constructors (Cont.)

Key to the foundation of the algorithms are the following two facts:

1. *Completeness:* Each labeled multi-graph can be represented by a `graph` term.
2. *Choice of Representation:* For each graph *g* and each node *v* contained in *g* there exist *p*, *l*, *s*, and *g'* such that *(p, v, l, s) & g'* denotes *g*. 

__Node 3 Representation__
```
 ([("down", 2)], 3, 'c', [("up", 1)])   &
([("right", 1)], 2, 'b', [("left", 1)]) &
            ([], 1, 'a', [])            & Empty
```

__Node 2 Representation__
```
([("right", 1)], 2, 'b', [("left", 1); ("down", 3)]) &
            ([], 3, 'c', [("up", 1)])                &
            ([], 1, 'a', [])                         & Empty
```

__Node 1 Representation__
```
([("up", 3); ("left", 2)], 1, 'a', [("right", 2)]) &
           ([("down", 2)], 3, 'c', [])             &
                      ([], 2, 'b', [])             & Empty
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs

Now we can start defining elementary functions on the inductive graph view.

```
val is_empty : ('a, 'b) graph -> bool
let is_empty g =
  match g with
  | Empty -> true
  | _ -> false
```

```
val gmap : (('a, 'b) context -> ('c, 'd) context) -> ('a. b') graph -> ('c, 'd) graph
let rec gmap f g =
  match g with
  | Empty -> Empty
  | c & g -> f c & gmap f g
```

__A graph reversal function can be succinctly defined as:__

*(We'll be making use of ocaml's support of partial application)*

```
val grev : ('a, 'b) graph -> ('a, 'b) graph
let grev =
  let swap (p, v, l, s) = (s, v, l, p) in
  gmap swap
```

---

# Inductive Graphs and Functional Graph Algorithms
### (Aside) Fusion in Functional Programming

Consider the following function in which we parse a list of string values into integers, multiply each value by two, and write the values back into a new list of string values.

```
val double_strings : string list -> string list
let double_strings l =
  l
  |> List.map int_of_string
  |> List.map (( * ) 2)
  |> List.map string_of_int
```

Although our function signature involves only two data structures, there are two more hidden, intermediate data structures within the function as we parse the strings into integers and multiply the integers by two. Additionally, due to the inclusion of these intermediate data structures we are traversing our list two extra times.

The process of eliminating these intermediate data structures in our code is called *Fusion*.

We are able to eliminate these intermediate data structures in this example due to the fusion law for `List.map`

```
val ( @ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(List.map f) @ (List.map f') = List.map (f @ f')
```

Given this provable rule we can rewrite our function in the following way, eliminating both intermediate data structures and excess iteration:

```
let double_strings = List.map ((int_of_string @ ((*) 2)) @ string_of_int)
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)

So let's put the induction into our inductive data types and prove the fusion law for `gmap` that we just defined.

```
Prove: ((gmap f) @ (gmap f')) g = gmap (f @ f')

Base Case g = Empty:
gmap f (gmap f' Empty) = gmap f Empty
                       = Empty
                       = gmap (f @ f') Empty

Otherwise g = c & g':
gmap f (gmap f' g) = gmap f (gmap f' (c & g'))      -- (Def. g)
                   = gmap f (f' c & (gmap f' g'))   -- (Def. gmap)
                   = f (f' c) & gmap f (gmap f' g') -- (Def. gmap)
                   = (f @ f') c & gmap (f @ f') g'  -- (Ind. Hyp.)
                   = gmap (f @ f') (c & g')         -- (Def. gmap)
                   = gmap (f @ f') g                -- (Def. g)
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)


And let's prove that *grev @ grev = id*. 

```
In order to do that we need to prove a couple extra things first.

val id : 'a -> 'a:
let id x = x

Prove swap @ swap = id -- (swap idempotency)
(swap @ swap) (p, v, l, s) = id (p, v, l, s)
  swap (swap (p, v, l, s)) = id (p, v, l, s) -- (Def. @)
         swap (s, v, l, p) = id (p, v, l, s) -- (Def. swap)
              (p, v, l, s) = id (p, v, l, s) -- (Def. swap)
              (p, v, l, s) = (p, v, l, s)    -- (Def. id)


Prove gmap id = id -- (gmap unit)

Base Case g = Empty:
gmap id Empty = id Empty
        Empty = id Empty -- (Def. gmap)
        Empty = Empty    -- (Def. id)


Case g = (c & g'):
        gmap id g = id g
 gmap id (c & g') = id g -- (Def. g)
id c & gmap id g' = id g -- (Def. gmap)
   c & gmap id g' = g    -- (Def. id)
           c & g' = g    -- (Ind. Hyp.)
                g = g    -- (Def. g)
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)

```
Prove grev @ grev = id -- (grev inversion)

grev @ grev = (gmap swap) @ (gmap swap) -- (Def. grev)
            = gmap (swap @ swap)        -- (gmap fusion)
            = gmap id                   -- (swap idempotency)
            = id                        -- (gmap unit)
```

> "To really appreciate the elegance of this proof, ... try to prove the same property for the imperative graph reversal algorithm that works by iterating over all adjacency lists."

-- Erwig being snarky in the paper

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)

```
val ufold : (('a, 'b) context -> 'c -> 'c) -> 'c -> ('a, 'b) graph -> 'c
let rec ufold f u g =
  match g with
  | Empty -> u
  | c & g' -> f c (ufold u g)

val nodes : ('a, 'b) graph -> node list
nodes = ufold (fun (_, v, _, _) u -> v :: u) []

val uniq : ('b -> 'b -> bool) -> 'b node list -> 'b node list

val undir : ('b -> 'b -> bool) -> ('a, 'b) graph -> ('a, 'b) graph
let undir cmp =
  gmap (fun (p, v, l, s) ->
      let ps = uniq cmp (List.append p s) in
      (ps, v, l, ps))
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)

Within the actual implementation of the graph `&` will be a function and it won't be possible to pattern match on. But we can still replicate the same functionality of our inductive view given two auxillary functions.

```
val is_empty : ('a, 'b) graph -> bool
val match_any : ('a, 'b) graph -> (('a, 'b) context) * ('a, 'b) graph)

let rec gmap f g =
  if is_empty g then g
  else
    let c, g' = match_any g in
    f c & gmap f g'
```

Given another auxillary function `match_node` capable of potentially separating a node's context from the remainder of the graph we can define even more functions.

```
val match_node : node -> ('a, 'b) graph -> (('a, 'b) context option * ('a, 'b) graph)

val gsuc : Node -> ('a, 'b) graph -> node list
let gsuc v g =
  match match_node v g with
  | (None, _) -> []
  | (Some (_, _, _, s), _) = List.map snd s

val deg : Node -> ('a, 'b) graph -> int
let deg v g =
  match match_node v g with
  | (None, _) -> []
  | (Some (p, _, _, s), _) = List.length p + List.length s

val del : Node -> ('a, 'b) graph -> ('a, 'b) graph
let del v g =
  match match_node v g with
  | (None, _) -> g
  | (Some _, g') = g'

val suc : ('a, 'b) context -> node list
let suc (_, _, _, s) = List.map snd s
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)

Recall our `List` data type from earlier. We were able to extract an API representing the inductive view of the data type without necessarily tying the implementation to an inductive representation. 

When we did so, each component of the API fell into one of two categories, it was either related to *construction* or *decomposition*.

| Construction         | Decomposition                        |
| -------------------- | ------------------------------------ |
| Empty list (empty)   | Test for empty list (is_empty)       |
| Insert item (insert) | Extract first element (head)         |
|                      | Extract all but first element (tail) |

Looking at the operations we've been using on our graph structure in the examples above, we can detail an API with a similar breakdown between *construction* and *decomposition*.

| Construction         | Decomposition                         |
| -------------------- | ------------------------------------- |
| Empty graph (empty)  | Test for empty graph (is_empty)       |
| Add context (&)      | Extract arbitrary context (match_any) |
|                      | Extract specific context (match_node) |

---

# Inductive Graphs and Functional Graph Algorithms
### 3.2 Pattern Matching on Graphs (Cont.)

Expressed as an ocaml type:

```
type node = int
type 'b adj = ('b * node) list
type ('a, 'b) context = ('b adj *  node * 'a * 'b adj)

module type Graph = sig
  type ('a, 'b) t

  val empty : ('a, 'b) t
  val is_empty : ('a, 'b) t -> bool

  val ( & ) : ('a, 'b) context -> ('a, 'b) t -> ('a, 'b) t

  val match_any : ('a, 'b) t -> (('a, 'b) context * ('a, 'b) t)
  val match_node : node -> ('a, 'b) t -> (('a, 'b) context option * ('a, 'b) t)
end
```

---

# Inductive Graphs and Functional Graph Algorithms
### 3.3 Implementation and Complexity

Our inductive type is incredibly inefficient. The underlying data structure is a variant of a linked list and processing something like `match_node` will require linear probing of the graph structure.

To resolve this, Erwig replaces all instances of lists in the initial inductive type with binary trees. Binary trees to store the node contexts, binary trees to store the edges.

I've implemented this same design in ocaml in the associated git repository, but will not be diving into it in this presentation.

Feel free to take a look at it and reach out to me if you have any questions.

---

# Inductive Graphs and Functional Graph Algorithms
### 4. Functional Graph Algorithms

Finally! We've reached the portion of the program in which we actually implement graph algorithms.

Starting with a function *dfs* which yields a list of nodes in depth-first order:

```
val dfs : node list -> ('a, 'b) graph -> node list
let rec dfs vs g =
  match vs with
  | [] -> []
  | v :: vs' -> (
      match match_node v g with
      | Some c, g' -> v :: dfs (List.append (suc c) vs) g
      | _ -> dfs vs g)
```

> The algorithm works as follows. If there are no nodes left to be visited (first case), *dfs* stops without returning any nodes. 
> In contrast, if there are still nodes that must be visited, *dfs* tries to locate the context of the first of these nodes (*v*) 
> in the argument graph. If this is possible ([first case of inner match]), which is the case whenever *v* is contained in the 
> argument graph, *v* is the next node on the resulting node list, and the search contianues on the remaining graph *g'* with the
> successors of *v* to be visited before the remaining list of nodes *vs*. The fact that the successors are put in front of all 
> other nodes causes *dfs* to favor searching in the depth and not in the breadth. Finally if *v* cannot be matched ([last line]), 
> *dfs* continues the search with the remaining list of nodes *vs*.

---

# Inductive Graphs and Functional Graph Algorithms
### 4. Functional Graph Algorithms (Cont.)

> Computing a depth-first spanning forest is slightly more complex because we have to distinguish between the relationships of a node to its successors and that to its siblings to obtain the spanning tree structure.

> First, we need a definition of multi-way trees together with a postorder traversal function that visits the nodes of all subtrees before the root.

```
type 'a tree = Br of 'a * 'a tree list

let rec postorder (Br (v, ts)) =
  List.append (List.concat_map postorder ts) [ v ]
```

> Now we can define a function to compute spanning forests.

```
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

(* Helper function to return just the forest since the graph 
   component is only used internally *)

let dff vs g = fst (df vs g)
```

> Being able to compute depth-first spanning forests we can now implement ... topologically sorting a graph and computing strongly connected components.

```
let topsort g = ((dff (nodes g) @ List.concat_map postorder) @ List.rev) g
let scc g = dff (topsort g) (grev g)
```

---

# Inductive Graphs and Functional Graph Algorithms
### 4. Functional Graph Algorithms (Cont.)

That topological sorting definition was a little *too* fused, so breaking it apart we can get something easier to read.

```
let topsort' g =
  (* Compute the depth first spanning forests *)
  dff (nodes g) g
  (* Traverse each tree in postorder and concatenate orders into single output *)
  |> List.concat_map postorder
  (* The order of the sorted list is in the wrong direction so reverse it *)
  |> List.rev
```

---

# Inductive Graphs and Functional Graph Algorithms
### 4.2 Breadth-First Search

> Breadth-first search .. means visiting siblings before successors. This has the effect of first visiting nodes of a certain distance ... from the start node ... This property is exploited by the shortest-path algorithm *esp*.

```
let rec bfs vs g =
  match vs with
  | [] -> []
  | v :: vs' -> (
      match match_node v g with
      | Some c, g' -> v :: bfs (List.append vs' (suc c)) g'
      | _ -> bfs vs g)
```

> This algorithm works very much like depth-first search ... for *dfs* [newly found successors] are kept in a stack ... for *bfs* the nodes are kept in queue.

```
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
```

---
# Inductive Graphs and Functional Graph Algorithms
### Thank You!

That's it! We've worked our way up from initial definitions to succinct algorithms capable of finding the shortest paths between two nodes in an arbitrary multi-graph.

There's lots more in the paper and elsewhere that we didn't cover but is still touched upon in the `inductive_data_types` codebase.


- Implementation code and tests for Peano Natural Numbers (Inductive Data Type).
- Implementation code and tests for Inductive List API.
- Implementation of "Fast Mergeable Integer Maps"
- Implementation of the Graph Data Structure using the Fast Mergeable Integer Map instead of Lists.
- Implementation code and tests for all graph algorithms presented in the paper:
  - Includes implementations for Minimum Spanning Tree algorithms and Dijkstra's algorithm.

__LINKS__

- [Inductive Graphs and Functional Graph Algorithms](https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf)
- [Fast Mergeable Integer Maps](TODO)
