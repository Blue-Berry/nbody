open Owl.Maths
open Nbody
open Sexplib.Std

let ( +. ) = add
let ( -. ) = sub
let ( /. ) = div

type centroid = float * Nbody.point [@@deriving sexp_of]

let centroid_sum (c1 : centroid) (c2 : centroid) : centroid =
  let open Nbody in
  let m1, p1 = c1 in
  let m2, p2 = c2 in
  let msum = m1 +. m2 in
  msum, 1.0 /. msum *$ ((m1 *$ p1) ++ (m2 *$ p2))
;;

module Bbox = struct
  open Nbody

  type t =
    { minx : float
    ; miny : float
    ; maxx : float
    ; maxy : float
    }
  [@@deriving sexp_of]

  let contains_point ((x, y) : point) (bb : t) : bool =
    bb.minx <= x && x <= bb.maxx && bb.miny <= y && y <= bb.maxy
  ;;

  let midx (bb : t) : float = bb.minx +. ((bb.maxx -. bb.minx) /. 2.)
  let midy (bb : t) : float = bb.miny +. ((bb.maxy -. bb.miny) /. 2.)
end

module Quadrant = struct
  open Nbody

  type t =
    | UL
    | UR
    | LL
    | LR

  let contains ((x, y) : point) (bb : Bbox.t) : t =
    match x <= Bbox.midx bb, y <= Bbox.midy bb with
    | true, false -> UL
    | false, false -> UR
    | true, true -> LL
    | false, true -> LR
  ;;

  let to_bbox (quad : t) (bb : Bbox.t) : Bbox.t =
    let mx = Bbox.midx bb in
    let my = Bbox.midy bb in
    match quad with
    | UL -> { bb with miny = my; maxx = mx }
    | UR -> { bb with minx = mx; miny = my }
    | LL -> { bb with maxx = mx; maxy = my }
    | LR -> { bb with minx = mx; maxy = my }
  ;;

  let to_index (quad : t) : int =
    match quad with
    | UL -> 0
    | UR -> 1
    | LL -> 2
    | LR -> 3
  ;;

  let of_index (idx : int) : t =
    match idx with
    | 0 -> UL
    | 1 -> UR
    | 2 -> LL
    | 3 -> LR
    | _ -> failwith "Quadrant.of_index"
  ;;
end

module Node = struct
  (** node of the quadtree

  [centroid] is the centroid of the quadtree
  [children] is the index of the first child in the qtree array
  *)
  type t =
    { mutable centroid : centroid
    ; mutable children : int
    ; mutable next : int
    ; bbox : Bbox.t
    }
  [@@deriving sexp_of]

  let new_node centroid bbox = { centroid; children = 0; next = 0; bbox }

  type kind =
    | Empty
    | Leaf
    | Node

  let node_type (node : t) : kind =
    match node.centroid with
    | 0.0, _ -> Empty
    | _ ->
      (match node.children with
       | 0 -> Leaf
       | _ -> Node)
  ;;

  let equal (a : t) (b : t) : bool =
    let same_centroid (a_cm, a_cp) (b_cm, b_cp) =
      close_enough a_cp b_cp && a_cm -. b_cm < 0.00001
    in
    let same_bbox (a_bbox : Bbox.t) (b_bbox : Bbox.t) =
      a_bbox.miny -. b_bbox.miny < 0.00001
      && a_bbox.maxy -. b_bbox.maxy < 0.00001
      && a_bbox.minx -. b_bbox.minx < 0.00001
      && a_bbox.maxx -. b_bbox.maxx < 0.00001
    in
    let a_child, b_child = a.children, b.children in
    let a_next, b_next = a.next, b.next in
    let a_bbox, b_bbox = a.bbox, b.bbox in
    Printf.printf
      "Node equalities: centroid: %b; bbox: %b; children: %b; next: %b\n"
      (same_centroid a.centroid b.centroid)
      (same_bbox a_bbox b_bbox)
      (a_child = b_child)
      (a_next = b_next);
    let result =
      (same_centroid a.centroid b.centroid && same_bbox a_bbox b_bbox)
      && a_child = b_child
      && a_next = b_next
    in
    if not result
    then (
      Printf.printf "Node A: %s\n" (sexp_of_t a |> Sexplib.Sexp.to_string_hum);
      Printf.printf "Node B: %s\n" (sexp_of_t b |> Sexplib.Sexp.to_string_hum);
      result)
    else result
  ;;
end

module Qtree = struct
  type t = { nodes : Node.t Dynarray.t }

  let capacity = 10

  let new_t (bbox : Bbox.t) =
    let nodes = Dynarray.create () in
    Dynarray.set_capacity nodes capacity;
    let node = Node.new_node (0.0, zero) bbox in
    Dynarray.add_last nodes node;
    { nodes }
  ;;

  let subdivide (qt : t) (node_idx : int) : int =
    let children = qt.nodes |> Dynarray.length in
    let node = Dynarray.get qt.nodes node_idx in
    assert (Node.(node_type node != Node));
    Dynarray.set qt.nodes node_idx { node with children };
    let next_nodes = [| children + 1; children + 2; children + 3; node.next |] in
    for i = 0 to 3 do
      let bbox = node.bbox |> Quadrant.to_bbox (Quadrant.of_index i) in
      Dynarray.add_last
        qt.nodes
        { centroid = 0.0, zero; children = 0; next = next_nodes.(i); bbox }
    done;
    children
  ;;

  let subdivide_leaf (qt : t) (node_idx : int) : int =
    let children = qt.nodes |> Dynarray.length in
    let node = Dynarray.get qt.nodes node_idx in
    assert (Node.(node_type node = Leaf));
    let lm, lp = node.centroid in
    Dynarray.set qt.nodes node_idx { node with children };
    let next_nodes = [| children + 1; children + 2; children + 3; node.next |] in
    for i = 0 to 3 do
      let bbox = node.bbox |> Quadrant.to_bbox (Quadrant.of_index i) in
      Dynarray.add_last
        qt.nodes
        { centroid = (if Bbox.contains_point lp bbox then lm, lp else 0.0, zero)
        ; children = 0
        ; next = next_nodes.(i)
        ; bbox
        }
    done;
    children
  ;;

  (* Calculate the bounding box if decebging divide the bounding box into quadrants if ascending multiple the bounding box *)
  let acc_by_qtree (pos1 : point) (q : t) (thresh : float) : vec =
    let rec aux (q : t) (node_idx : int) (acc : vec) : vec =
      let node = Dynarray.get q.nodes node_idx in
      if node.next = 0
      then acc
      else (
        let cm, cp = node.centroid in
        (* TODO: Calculate bounding box of node *)
        match Node.node_type node with
        | Empty -> aux q node.next acc
        | Leaf -> aux q node.next (acc_on pos1 cm cp)
        | Node when pos1 --> cp |> mag > thresh -> acc_on pos1 cm cp
        | Node -> aux q node.children acc)
    in
    aux q 0 zero
  ;;

  (* TODO: calculate new centroid *)
  let insert (qt : t) ((m, pos) : centroid) =
    let rec aux (q : t) (node_idx : int) =
      let node = Dynarray.get q.nodes node_idx in
      match Node.node_type node with
      | Node ->
        let i = Quadrant.contains pos node.bbox |> Quadrant.to_index in
        node.centroid <- centroid_sum node.centroid (m, pos);
        aux q (node.children + i)
      | Empty ->
        node.centroid <- m, pos;
        Dynarray.set q.nodes node_idx node
      | Leaf ->
        let cm, cp = node.centroid in
        if close_enough cp pos
        then (
          node.centroid <- m +. cm, pos;
          Dynarray.set q.nodes node_idx node)
        else (
          (* Is this right?*)
          node.centroid <- centroid_sum node.centroid (m, pos);
          let children = subdivide_leaf q node_idx in
          let q_idx = Quadrant.contains pos node.bbox |> Quadrant.to_index in
          aux q (children + q_idx))
    in
    aux qt 0
  ;;
end
