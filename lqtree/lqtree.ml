open Owl.Maths
module Nbody = Nbody
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
  let size (bb : t) : float = bb.maxx -. bb.minx
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

  (* Hotspot *)
  let node_type (node : t) : kind =
    if node.children = 0
    then (
      let m, _ = node.centroid in
      if m = 0.0 then Empty else Leaf)
    else Node
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

  let thresh_factor = 3.0

  let sexp_of_t qt =
    let nodes = Dynarray.to_list qt.nodes in
    Sexplib.Std.sexp_of_list Node.sexp_of_t nodes
  ;;

  let capacity = 1000

  let get_node (qt : t) (idx : int) : Node.t =
    assert (idx <= Dynarray.length qt.nodes);
    Dynarray.get qt.nodes idx
  [@@inline]
  ;;

  let new_t (bbox : Bbox.t) =
    let nodes = Dynarray.create () in
    Dynarray.set_capacity nodes capacity;
    let node = Node.new_node (0.0, zero) bbox in
    Dynarray.add_last nodes node;
    { nodes }
  ;;

  let subdivide (qt : t) (node_idx : int) : int =
    let children = qt.nodes |> Dynarray.length in
    let node = get_node qt node_idx in
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
    let node = get_node qt node_idx in
    assert (Node.(node_type node = Leaf));
    let lm, lp = node.centroid in
    Dynarray.set qt.nodes node_idx { node with children };
    let next_nodes = [| children + 1; children + 2; children + 3; node.next |] in
    for i = 0 to 3 do
      let bbox = node.bbox |> Quadrant.to_bbox (Quadrant.of_index i) in
      let centroid = if Bbox.contains_point lp bbox then lm, lp else 0.0, zero in
      Dynarray.add_last qt.nodes { centroid; children = 0; next = next_nodes.(i); bbox }
    done;
    children
  ;;

  let node_check pos1 cp bbox =
    let dist_sq = pos1 --> cp |> mag_squared in
    let inside_bbox = Bbox.contains_point pos1 bbox in
    let bbox_size_sqr = Bbox.size bbox |> sqr in
    dist_sq *. thresh_factor > bbox_size_sqr && not inside_bbox
  ;;

  (* let node_check pos1 cp bbox = *)
  (*   pos1 --> cp |> mag > 1000000.0 && (not @@ Bbox.contains_point pos1 bbox) *)

  (* let[@inline] node_check pos1 cp thresh bbox = *)
  (*   pos1 --> cp |> mag > thresh && (not @@ Bbox.contains_point pos1 bbox) *)
  (* ;; *)

  let acc_by_qtree (pos1 : point) (q : t) : vec =
    let rec acc_aux (q : t) (node_idx : int) (acc : vec) : vec =
      let node = get_node q node_idx in
      let cm, cp = node.centroid in
      match Node.node_type node with
      | Empty when node.next = 0 -> acc
      | Empty -> acc_aux q node.next acc
      | Leaf when node.next = 0 -> acc ++ acc_on pos1 cm cp
      | Leaf -> acc_aux q node.next (acc ++ acc_on pos1 cm cp)
      | Node when node_check pos1 cp node.bbox && node.next = 0 ->
        acc ++ acc_on pos1 cm cp
      | Node when node_check pos1 cp node.bbox ->
        acc_aux q node.next (acc ++ acc_on pos1 cm cp)
      | Node -> acc_aux q node.children acc
    in
    acc_aux q 0 zero
  ;;

  let insert (qt : t) ((m, pos) : centroid) =
    let rec insert_aux (q : t) (node_idx : int) =
      let node = get_node q node_idx in
      match Node.node_type node with
      | Node ->
        let i = Quadrant.contains pos node.bbox |> Quadrant.to_index in
        node.centroid <- centroid_sum node.centroid (m, pos);
        insert_aux q (node.children + i)
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
          (* Convert leaf to node. calculate new centroid, split into quadrants, and recurse. *)
          let children = subdivide_leaf q node_idx in
          let new_centroid = centroid_sum node.centroid (m, pos) in
          node.centroid <- new_centroid;
          node.children <- children;
          Dynarray.set q.nodes node_idx node;
          let q_idx = Quadrant.contains pos node.bbox |> Quadrant.to_index in
          insert_aux q (children + q_idx))
    in
    insert_aux qt 0
  ;;

  let build_qtree_in (bodies : body list) (bb : Bbox.t) : t =
    let q = new_t bb in
    List.iter
      (fun b ->
         let cen : centroid = b.mass, b.pos in
         insert q cen)
      bodies;
    q
  ;;

  let populate (qt : t) (bodies : body list) : unit =
    List.iter
      (fun b ->
         let cen : centroid = b.mass, b.pos in
         insert qt cen)
      bodies
  ;;

  let clear (q : t) : unit =
    if Dynarray.length q.nodes = 0
    then ()
    else (
      let node = Dynarray.get q.nodes 0 in
      let bbox = node.bbox in
      Dynarray.clear q.nodes;
      let node = Node.new_node (0.0, zero) bbox in
      Dynarray.add_last q.nodes node)
  ;;
end
