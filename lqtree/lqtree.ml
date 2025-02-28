open Owl.Maths
module Nbody = Nbody
open Nbody
open Sexplib.Std

let ( +. ) = add
let ( -. ) = sub
let ( /. ) = div

type centroid = float * Nbody.point

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

  type ts = Vec_array.Float4.t

  let contains_point ((x, y) : point) (bb : t) : bool =
    bb.minx <= x && x <= bb.maxx && bb.miny <= y && y <= bb.maxy
  ;;

  let midx (bb : t) : float = bb.minx +. ((bb.maxx -. bb.minx) /. 2.)
  let midy (bb : t) : float = bb.miny +. ((bb.maxy -. bb.miny) /. 2.)
  let size (bb : t) : float = bb.maxx -. bb.minx

  let get (bbs : ts) (i : int) : t =
    let bb = Vec_array.Float4.get bbs i in
    { minx = bb.v1; miny = bb.v2; maxx = bb.v3; maxy = bb.v4 }
  ;;

  let set (bbs : ts) (i : int) (bb : t) : unit =
    let open Vec_array.Float4 in
    Vec_array.Float4.set bbs i { v1 = bb.minx; v2 = bb.miny; v3 = bb.maxx; v4 = bb.maxy }
  ;;

  let add (bbs : ts) (bb : t) : unit =
    let open Vec_array.Float4 in
    Vec_array.Float4.add_last
      bbs
      { v1 = bb.minx; v2 = bb.miny; v3 = bb.maxx; v4 = bb.maxy }
  ;;
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

  type ts =
    { mutable centroids : Vec_array.Float3.t
    ; mutable children : Vec_array.Int.t
    ; mutable next : Vec_array.Int.t
    ; mutable bboxs : Bbox.ts
    }

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
    result
  ;;

  type _ node_field =
    | Centroid : centroid node_field
    | Children : int node_field
    | Next : int node_field
    | Bbox : Bbox.t node_field
    | Full : t node_field

  let get (type a) (node : ts) (idx : int) (field : a node_field) : a =
    let open Vec_array in
    match field with
    | Centroid ->
      let Float3.{ v1; v2; v3 } = Float3.get node.centroids idx in
      v1, (v2, v3)
    | Children -> Int.get node.children idx
    | Next -> Int.get node.next idx
    | Bbox -> Bbox.get node.bboxs idx
    | Full ->
      let Float3.{ v1; v2; v3 } = Float3.get node.centroids idx in
      let centroid = v1, (v2, v3) in
      { centroid
      ; children = Int.get node.children idx
      ; next = Int.get node.next idx
      ; bbox = Bbox.get node.bboxs idx
      }
  ;;

  let set (type a) (node : ts) (idx : int) (field : a node_field) (value : a) : unit =
    let open Vec_array in
    match field with
    | Centroid ->
      let v1, (v2, v3) = value in
      Float3.set node.centroids idx Float3.{ v1; v2; v3 }
    | Children -> Int.set node.children idx value
    | Next -> Int.set node.next idx value
    | Bbox -> Bbox.set node.bboxs idx value
    | Full ->
      let v1, (v2, v3) = value.centroid in
      Float3.set node.centroids idx Float3.{ v1; v2; v3 };
      Int.set node.children idx value.children;
      Int.set node.next idx value.next;
      Bbox.set node.bboxs idx value.bbox
  ;;

  let add (node : ts) (value : t) : unit =
    let open Vec_array in
    let v1, (v2, v3) = value.centroid in
    Float3.add_last node.centroids Float3.{ v1; v2; v3 };
    Int.add_last node.children value.children;
    Int.add_last node.next value.next;
    Bbox.add node.bboxs value.bbox
  ;;

  let capacity = 1000

  let new_ts () : ts =
    let centroids = Vec_array.Float3.create capacity in
    let children = Vec_array.Int.create capacity in
    let next = Vec_array.Int.create capacity in
    let bboxs = Vec_array.Float4.create capacity in
    { centroids; children; next; bboxs }
  ;;

  let validate (node : ts) : unit =
    let open Vec_array in
    let centroids = Float3.len node.centroids in
    let children = Int.len node.children in
    let next = Int.len node.next in
    let bboxs = Float4.len node.bboxs in
    assert (centroids = children);
    assert (centroids = next);
    assert (centroids = bboxs);
    ()
  ;;

  let nodes_len (nodes : ts) : int =
    let open Vec_array in
    Int.len nodes.children
  ;;

  let clear (nodes : ts) : unit =
    let open Vec_array in
    Float3.clear nodes.centroids;
    Int.clear nodes.children;
    Int.clear nodes.next;
    Float4.clear nodes.bboxs
  ;;
end

module Qtree = struct
  type t = { nodes : Node.ts }

  let thresh_factor = 3.0

  let new_t (bbox : Bbox.t) =
    let nodes = Node.new_ts () in
    { nodes }
  ;;

  let subdivide (qt : t) (node_idx : int) : int =
    let children = Node.nodes_len qt.nodes in
    let node = Node.get qt.nodes node_idx Full in
    assert (Node.(node_type node != Node));
    Node.set qt.nodes node_idx Full { node with children };
    let next_nodes = [| children + 1; children + 2; children + 3; node.next |] in
    for i = 0 to 3 do
      let bbox = node.bbox |> Quadrant.to_bbox (Quadrant.of_index i) in
      Node.add
        qt.nodes
        { centroid = 0.0, zero; children = 0; next = next_nodes.(i); bbox }
    done;
    children
  ;;

  let subdivide_leaf (qt : t) (node_idx : int) : int =
    (* TODO: get only the valid fields *)
    let children = Node.nodes_len qt.nodes in
    let node = Node.get qt.nodes node_idx Full in
    assert (Node.(node_type node = Leaf));
    let lm, lp = node.centroid in
    Node.set qt.nodes node_idx Full { node with children };
    let next_nodes = [| children + 1; children + 2; children + 3; node.next |] in
    for i = 0 to 3 do
      let bbox = node.bbox |> Quadrant.to_bbox (Quadrant.of_index i) in
      let centroid = if Bbox.contains_point lp bbox then lm, lp else 0.0, zero in
      Node.add qt.nodes { centroid; children = 0; next = next_nodes.(i); bbox }
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
      let node = Node.get q.nodes node_idx Full in
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
      let node = Node.get q.nodes node_idx Full in
      match Node.node_type node with
      | Node ->
        let i = Quadrant.contains pos node.bbox |> Quadrant.to_index in
        node.centroid <- centroid_sum node.centroid (m, pos);
        insert_aux q (node.children + i)
      | Empty ->
        node.centroid <- m, pos;
        Node.set q.nodes node_idx Full node
      | Leaf ->
        let cm, cp = node.centroid in
        if close_enough cp pos
        then (
          node.centroid <- m +. cm, pos;
          Node.set q.nodes node_idx Full node)
        else (
          (* Convert leaf to node. calculate new centroid, split into quadrants, and recurse. *)
          let children = subdivide_leaf q node_idx in
          let new_centroid = centroid_sum node.centroid (m, pos) in
          node.centroid <- new_centroid;
          node.children <- children;
          Node.set q.nodes node_idx Full node;
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
    if Node.nodes_len q.nodes = 0
    then ()
    else (
      let node = Node.get q.nodes 0 Full in
      let bbox = node.bbox in
      Node.clear q.nodes;
      let node = Node.new_node (0.0, zero) bbox in
      Node.add q.nodes node)
  ;;
end
