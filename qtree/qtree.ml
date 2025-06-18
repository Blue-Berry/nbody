open Owl.Maths
open Nbody
open Sexplib.Std

let ( +. ) = add
let ( -. ) = sub
let ( /. ) = div

type centroid = float * Nbody.point [@@deriving sexp]

let centroid_sum (c1 : centroid) (c2 : centroid) : centroid =
  let m1, p1 = c1 in
  let m2, p2 = c2 in
  let msum = m1 +. m2 in
  msum, 1.0 /. msum *$ ((m1 *$ p1) ++ (m2 *$ p2))
;;

type qtree =
  | Empty
  | Leaf of float * point
  | Node of centroid * quads
[@@deriving sexp]

and quads =
  { ul : qtree
  ; ur : qtree
  ; ll : qtree
  ; lr : qtree
  }
[@@deriving sexp]

module Bbox = struct
  type t =
    { minx : float
    ; miny : float
    ; maxx : float
    ; maxy : float
    }
  [@@deriving sexp]

  let contains_point ((x, y) : point) (bb : t) : bool =
    bb.minx <= x && x <= bb.maxx && bb.miny <= y && y <= bb.maxy
  ;;

  let midx (bb : t) : float = bb.minx +. ((bb.maxx -. bb.minx) /. 2.)
  let midy (bb : t) : float = bb.miny +. ((bb.maxy -. bb.miny) /. 2.)
end

module Quadrant = struct
  type t =
    | UL
    | UR
    | LL
    | LR
  [@@deriving sexp]

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
end

let acc_by_qtree (pos1 : point) (q : qtree) (bb : Bbox.t) (thresh : float): vec =
  let rec aux (pos1 : point) (q : qtree) (bb : Bbox.t) (thresh : float) (k : vec -> vec) =
  match q with
  | Empty -> k zero
  | Leaf (m, p) -> k @@ (zero ++ acc_on pos1 m p)
  | Node (c, qs) ->
    let cm, cp = c in
    let d = pos1 --> cp |> mag in
    if d > thresh && Bbox.contains_point pos1 bb |> not
    then k @@ acc_on pos1 cm cp
    else
      (* recurse on quadrants *)
      (
      (aux[@tailcall]) pos1 qs.ur (Quadrant.to_bbox UR bb) thresh (fun acc_ur ->
        (aux[@tailcall]) pos1 qs.ll (Quadrant.to_bbox LL bb) thresh (fun acc_ll ->
          (aux[@tailcall]) pos1 qs.ul (Quadrant.to_bbox UL bb) thresh (fun acc_ul ->
            (aux[@tailcall]) pos1 qs.lr (Quadrant.to_bbox LR bb) thresh (fun acc_lr ->
              (* sum the four quadrants *)
              acc_ur ++ acc_ll ++ (acc_ul ++ acc_lr))))))
  in
  aux pos1 q bb thresh (fun x -> x)
;;

let rec qinsert (q : qtree) (m : float) (p : point) (bb : Bbox.t) : qtree =
  match q with
  | Empty -> Leaf (m, p)
  | Leaf (lm, lp) when close_enough lp p -> Leaf (m +. lm, p)
  (* In this case a leaf needs to be split up into quadrants. And then the new Point needs to be inserted into the correct quadrant. *)
  | Leaf (lm, lp) ->
    let qs = { ul = Empty; ur = Empty; ll = Empty; lr = Empty } in
    let qs =
      match Quadrant.contains lp bb with
      | UL -> { qs with ul = Leaf (lm, lp) }
      | UR -> { qs with ur = Leaf (lm, lp) }
      | LL -> { qs with ll = Leaf (lm, lp) }
      | LR -> { qs with lr = Leaf (lm, lp) }
    in
    let node = Node ((lm, lp), qs) in
    (qinsert[@tailcall]) node m p bb
  | Node (c, qs) ->
    let qs =
      match Quadrant.contains p bb with
      | UL -> { qs with ul = qinsert qs.ul m p (Quadrant.to_bbox UL bb) }
      | UR -> { qs with ur = qinsert qs.ur m p (Quadrant.to_bbox UR bb) }
      | LL -> { qs with ll = qinsert qs.ll m p (Quadrant.to_bbox LL bb) }
      | LR -> { qs with lr = qinsert qs.lr m p (Quadrant.to_bbox LR bb) }
    in
    Node (centroid_sum c (m, p), qs)
;;

let build_qtree_in (bodies : body list) (bb : Bbox.t) : qtree =
  List.fold_left (fun acc b -> qinsert acc b.mass b.pos bb) Empty bodies
;;

let string_of_qtree (qtree : qtree) : string =
  sexp_of_qtree qtree |> Sexplib.Sexp.to_string_hum
;;
