(* Balanced Binary Search Tree with Red-Black invariants *)
type color = Red | Black

type 'a tree =
  | Leaf
  | Node of color * 'a tree * 'a * 'a tree

exception Not_found_tree

let rec member x = function
  | Leaf -> false
  | Node (_, left, v, right) ->
    if x < v then member x left
    else if x > v then member x right
    else true

let balance = function
  | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
  | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
  | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
  | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | color, left, value, right ->
    Node (color, left, value, right)

let insert x tree =
  let rec ins = function
    | Leaf -> Node (Red, Leaf, x, Leaf)
    | Node (color, left, v, right) ->
      if x < v then balance (color, ins left, v, right)
      else if x > v then balance (color, left, v, ins right)
      else Node (color, left, v, right)
  in
  match ins tree with
  | Node (_, left, v, right) -> Node (Black, left, v, right)
  | Leaf -> failwith "impossible"

let rec fold f acc = function
  | Leaf -> acc
  | Node (_, left, v, right) ->
    let acc = fold f acc left in
    let acc = f acc v in
    fold f acc right

let to_list tree = List.rev (fold (fun acc x -> x :: acc) [] tree)
let size tree = fold (fun acc _ -> acc + 1) 0 tree
let height tree =
  let rec h = function
    | Leaf -> 0
    | Node (_, l, _, r) -> 1 + max (h l) (h r)
  in h tree

(* Functor for ordered types *)
module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make (Ord : ORDERED) = struct
  type elt = Ord.t
  type t = elt tree

  let empty = Leaf
  let mem x = member x
  let add x = insert x
  let elements = to_list
  let cardinal = size
end

(* Usage *)
let () =
  let tree = Leaf
    |> insert 5 |> insert 3 |> insert 8
    |> insert 1 |> insert 4 |> insert 7 in
  Printf.printf "Size: %d, Height: %d\n" (size tree) (height tree);
  List.iter (Printf.printf "%d ") (to_list tree);
  print_newline ()
