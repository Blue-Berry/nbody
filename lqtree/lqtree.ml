open Owl.Maths
open Nbody

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
end

(** node of the quadtree

[centroid] is the centroid of the quadtree
[children] is the index of the first child in the qtree array
*)
type node =
  { mutable centroid : centroid
  ; mutable children : int
  ; mutable next : int;
  }

type node_type =
  | Empty
  | Leaf
  | Node

type qtree = { nodes : node Dynarray.t }

let subdivide (qt : qtree) (node_idx : int) : int =
  let children = qt.nodes |> Dynarray.length in
  let node = Dynarray.get qt.nodes node_idx in
  Dynarray.set qt.nodes node_idx { node with children = children };
  let next_nodes = [| children +1; children +2; children +3; node.next |] in
  for i = 0 to 3 do
      Dynarray.add_last qt.nodes { centroid =  (0.0, zero); children = 0; next = next_nodes.(i) }
  done;
  children
;;

let node_type (node : node) : node_type =
    match node.centroid with
    | 0.0, _ -> Empty
    | _ -> match node.children with
      | 0 -> Leaf
      | _-> Node
;;

let  acc_by_qtree (pos1 : point) (q : qtree) (bb : Bbox.t) (thresh : float) : vec =
    let rec aux (q : qtree) (node_idx : int) (bb : Bbox.t) (acc: vec) : vec =
        let node = Dynarray.get q.nodes node_idx in
        if node.next = 0 then acc else
        let cm, cp = node.centroid in
        (* TODO: Calculate bounding box of node *)
        match node_type node with
        | Empty -> aux q node.next bb acc 
        | Leaf -> aux q node.next bb (acc_on pos1 cm cp)
        | Node when (pos1 --> cp |> mag) > thresh -> acc_on pos1 cm cp
        | Node ->
                aux q node.children bb acc
    in
    aux q 0 bb zero
