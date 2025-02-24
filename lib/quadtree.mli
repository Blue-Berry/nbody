type centroid = float * Nbody.point
type qtree = Empty | Leaf of float * Nbody.point | Node of centroid * quads
and quads = { ul : qtree; ur : qtree; ll : qtree; lr : qtree; }
type bounding_box = {
  minx : float;
  miny : float;
  maxx : float;
  maxy : float;
}
val in_bounding_box : Nbody.point -> bounding_box -> bool
val bb0to4 : bounding_box
val midx : bounding_box -> float
val midy : bounding_box -> float
type quadrant = UL | UR | LL | LR
val quadrant_of : quadrant -> bounding_box -> bounding_box
val quadrant : Nbody.point -> bounding_box -> quadrant
val centroid_sum : centroid -> centroid -> centroid
val q0 : qtree
val q1 : qtree
val q2 : qtree
val q3 : qtree
val q4 : qtree
val quadtree_example_leaf : unit -> qtree
val quadtree_example_node1 : unit -> qtree
val quadtree_example_node2 : unit -> qtree
val qlookup : qtree -> Nbody.point -> bounding_box -> bool
val acc_by_qtree : Nbody.point -> qtree -> bounding_box -> float -> Nbody.vec
val qinsert_empty : qtree -> float -> Nbody.point -> bounding_box -> qtree
val qinsert_node : qtree -> float -> Nbody.point -> bounding_box -> qtree
val qinsert : qtree -> float -> Nbody.point -> bounding_box -> qtree
val remarkably_similar : qtree -> qtree -> bool
val build_qtree_in : Nbody.body list -> bounding_box -> qtree

