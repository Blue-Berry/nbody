type centroid = float * Nbody.point

val centroid_sum : centroid -> centroid -> centroid

type qtree =
  | Empty
  | Leaf of float * Nbody.point
  | Node of centroid * quads

and quads =
  { ul : qtree
  ; ur : qtree
  ; ll : qtree
  ; lr : qtree
  }

module Bbox : sig
  type t =
    { minx : float
    ; miny : float
    ; maxx : float
    ; maxy : float
    }

  val contains_point : Nbody.point -> t -> bool
  val midx : t -> float
  val midy : t -> float
end

module Quadrant : sig
  type t =
    | UL
    | UR
    | LL
    | LR

  val to_bbox : t -> Bbox.t -> Bbox.t
end

val acc_by_qtree : Nbody.point -> qtree -> Bbox.t -> float -> Nbody.vec
val qinsert : qtree -> float -> Nbody.point -> Bbox.t -> qtree
val build_qtree_in : Nbody.body list -> Bbox.t -> qtree
val string_of_qtree : qtree -> string
