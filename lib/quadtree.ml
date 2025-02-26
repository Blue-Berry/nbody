[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-39"]
(*----------------------------------------------------------------------------*)
(*-- Quadtrees ---------------------------------------------------------------*)
(*----------------------------------------------------------------------------*)

;; open Assert
;; open Nbody

(* The simple algorithm defined in the n-body module is too slow to
   handle large systems, such as the one called "collision". For this
   purpose, we will develop a new data structure called a "quadtree"
   that will help us speed up the simulation by approximating the
   solution. *)

(*----------------------------------------------------------------------------*)
(*-- Introduction: Quad Trees, Quadrants, and Bounding Boxes, Oh My! ---------*)
(*----------------------------------------------------------------------------*)

(* IMPORTANT: READ THE "Quad Trees To The Rescue" PORTION OF THE
   ASSIGNMENT INSTRUCTIONS ON THE COURSE WEBSITE BEFORE
   CONTINUING. THIS PART OF THE ASSIGNMENT WILL BE INCREDIBLY
   DIFFICULT TO UNDERSTAND OTHERWISE.

   The assignment instructions provide a high-level overview of the
   quadtree datatype that will be used for the rest of the
   assignment. Below, we present and explain the code that defines
   quadtrees, their auxiliary datatypes, and some helper functions
   that will help in defining functions that operate over quadtrees.

   MAKE SURE THAT YOU READ AND UNDERSTAND THE DEFINITIONS AND
   FUNCTIONS BELOW BEFORE CONTINUING. You will only need to fill in a
   few test cases below, but the later functions will operate on these
   datatypes, and will be made MUCH easier by employing the functions
   below as helper functions. *)

(* The strength of quadtrees comes from being able to estimate a
   center of mass for a given section of space. The 'centroid' type
   below defines such a center of mass of a given section of space;
   the float represents the actual mass, while the point represents
   the location of the center of mass. *)
type centroid = float * point

(* Now, with the centroid type, we can define the type of quad trees.

   Quadtrees are like binary trees except that each node has four
   children instead of two. For this datatype, we define three
   constructors...

   Empty: Self-explanatory; this constructor carries no information
   and represents a quadtree containing no bodies.

   Leaf: A constructor for a tree with no children. Every body in
   simulation will be contained in one leaf of the quadtree. The float
   and point passed to this constructor represent the mass and
   position of the contained body. (We don't need the body's velocity
   in order to calculate its gravitational pull on other bodies in 
   the system.)

   Node: A constructor for a tree with children; this will happen
   whenever the region of space encompassed by a particular quadtree
   contains two or more bodies.  This constructor takes in a
   'centroid' representing the center of mass of all of the points in
   the leaves of this node's children. This constructor also takes in
   a 'quads', defined below, which is a record specifying the four
   children of the node. *)

type qtree =
| Empty
| Leaf of float * point     (* mass and position of a body *)
| Node of centroid * quads  (* Center of mass and children of a Node *)

(* quads is just a record of the four subtrees, one for each quadrant. *)
and quads = {
  ul: qtree;
  ur: qtree;
  ll: qtree;
  lr: qtree;
} 

(* Quadtrees divide up (2-dimensional) space into four quadrants called
   bounding boxes. Before we can begin working with quadtrees, it will be
   useful to create some functions to work with these divisions. *)

(* A bounding box is defined by the coordinates of the lower-left and
   upper-right corners of the box.  An invariant of bounding boxes is
   that minx <= maxx and miny <= maxy.  (You don't need to check for
   the invariant.). We use bounding boxes in the quadtree functions
   below to represent the boundaries of an area encompassed by a
   quadtree.
*)
type bounding_box = {
  minx: float;
  miny: float;
  maxx: float;
  maxy: float;
}

(* Below, we provide you a function that will determine whether a given
   point is within a given bounding box *)
let in_bounding_box (p: point) (bb: bounding_box) : bool =
  begin match p with
  | (x, y) -> bb.minx <= x && x <= bb.maxx && bb.miny <= y && y <= bb.maxy
  end

(* Now, let's define a common bounding box we can use for testing *)
let bb0to4 = { minx = 0.0; maxx = 4.0; miny = 0.0; maxy = 4.0 }

(* Below are a few test cases that demonstrate the functionality of
in_bounding_box. *)
let test () : bool =
  in_bounding_box (1.0, 2.0) bb0to4
;; run_test "bounding box: (1,2) is within bb0to4" test

let test () : bool =
  not (in_bounding_box (9.0, 2.0) bb0to4)
;; run_test "bounding box: (9,2) is not in bb0to4" test

(* Below are two functions that are useful for finding the center of a
given bounding box *)
let midx (bb: bounding_box) : float =
  (bb.minx +. bb.maxx) /. 2.

let midy (bb: bounding_box) : float =
  (bb.miny +. bb.maxy) /. 2.

(* This datatype enumerates the four quadrants. Remember that, as with
   nucleotides in HW02, you can pattern match against values of this
   type. *)
type quadrant =
| UL (* upper left  *)
| UR (* upper right *)
| LL (* lower left  *)
| LR (* lower right *)


(* One thing to note is that a bounding box can be thought of as being
   composed of four smaller bounding boxes, representing the
   top-right, top-left, etc. quadrants of the box. This subdivision will
   be useful, so below we give you a function for dividing a bounding
   box into a smaller one representing one quadrant of the original
   box.
*)
let quadrant_of (quad:quadrant) (bb:bounding_box) : bounding_box =
  let mx = midx bb in
  let my = midy bb in
  begin match quad with 
  | UL -> {minx=bb.minx; miny=my     ; maxx=mx     ; maxy=bb.maxy;}
  | UR -> {minx=mx     ; miny=my     ; maxx=bb.maxx; maxy=bb.maxy;}
  | LL -> {minx=bb.minx; miny=bb.miny; maxx=mx     ; maxy=my;}
  | LR -> {minx=mx     ; miny=bb.miny; maxx=bb.maxx; maxy=my;}
  end

(* Given a point, it's simple to figure out in what quadrant of a
   bounding box the point should go. When a point lies exactly between
   two quadrants, prefer the lower and/or left quadrant. *)
let quadrant (p: point) (bb: bounding_box) : quadrant =
  let (x, y) = p in
  begin match x <= midx bb, y <= midy bb with
  | true, false  -> UL
  | false, false -> UR
  | true, true   -> LL
  | false, true  -> LR
  end

(* Below are a few tests that demonstrate how quadrant works *)
let test () : bool =
  LR = quadrant (3.0, 1.0) bb0to4
;; run_test "quadrant test 1" test

let test () : bool =
  LL = quadrant (1.0, 1.0) bb0to4
;; run_test "quadrant test 2" test

let test () : bool =
  UR = quadrant (3.0, 3.0) bb0to4
;; run_test "quadrant test 3" test

let test () : bool =
  UL = quadrant (1.0, 3.0) bb0to4
;; run_test "quadrant test 4" test



(* Combining two centroids is a useful operation for incrementally
   building a quadtree. The mass of the combined centroid is simply
   the sum of the two input centroids, and the position is the
   midpoint, weighted by mass.  *)
let centroid_sum (c1: centroid) (c2: centroid) : centroid =
  let (m1, p1) = c1 in
  let (m2, p2) = c2 in
  let msum = m1 +. m2 in
  (msum, (1.0 /. msum) *$ ((m1 *$ p1) ++ (m2 *$ p2)))

let test () : bool =
  (2.0, (2.0, 1.0)) = centroid_sum (1.0, (1.0, 1.0)) (1.0, (3.0, 1.0))
;; run_test "centroid_sum test 1" test

let test () : bool =
  (4.0, (4.0, 1.0)) = centroid_sum (1.0, (1.0, 1.0)) (3.0, (5.0, 1.0))
;; run_test "centroid_sum test 2" test

let test () : bool =
  (10.0, (7.4, 2.6)) = centroid_sum (8.0, (8.0, 3.0)) (2.0, (5.0, 1.0))
;; run_test "centroid_sum test 3" test

(*----------------------------------------------------------------------------*)
(* Problem 5 (hand-rolled quadtrees) -----------------------------------------*)
(*----------------------------------------------------------------------------*)

(* The following example quadtrees are described under the "Centroids"
   heading on the assignment webpage. Make sure that you understand
   these examples before continuing. *)

let b1 : centroid = (1.0, (1.5, 2.5))
let b2 : centroid = (1.0, (2.1, 2.1))
let b3 : centroid = (2.0, (1.0, 1.0))
let b4 : centroid = (1.0, (2.6, 2.8))

let q0 : qtree = Empty
let q1 : qtree = Leaf (1.0, (1.5, 2.5))
let q2 : qtree = Node (centroid_sum b1 b2,
                       { ul = Leaf (1.0, (1.5, 2.5));
                         ur = Leaf (1.0, (2.1, 2.1));
                         ll = Empty;
                         lr = Empty })
let q3 : qtree = Node (centroid_sum (centroid_sum b1 b2) b3,
                       { ul = Leaf (1.0, (1.5, 2.5));
                         ur = Leaf (1.0, (2.1, 2.1));
                         ll = Leaf (2.0, (1.0, 1.0));
                         lr = Empty })
let q4 : qtree =
  let c1 = centroid_sum b2 b4 in
  let c2 = centroid_sum (centroid_sum c1 b3) b1 in
  Node (c2, {
    ul = Leaf (1.0, (1.5, 2.5));
    ur = Node (c1, {
      ul = Empty;
      ur = Empty;
      ll = Node (c1, {
        ul = Empty;
        ur = Leaf (1.0, (2.6, 2.8));
        ll = Leaf (1.0, (2.1, 2.1));
        lr = Empty;
      });
      lr = Empty;
    });
    ll = Leaf (2.0, (1.0, 1.0));
    lr = Empty;
  })



(* Now it is your turn. For practice, we ask you to hand-code a few
   quadtrees to make sure you understand how the structure of the tree
   code maps back to a collection of bodies. You can also use these
   example quadtrees in the test cases that you write for the
   functions below. 
*)

(* Provide a hand-generated quad tree meeting the specifications
   below.  For all trees, you should use the bounding box 'bb0to4'
   defined above.  *)

(* First, generate a leaf that contains a single point with mass 1.0
at (1.0, 1.0). *)
let quadtree_example_leaf () = 
  Leaf (1.0 , (1.0, 1.0))
  


(* Now, make it a little more interesting by generating a quadtree
   containing two different bodies ...

   - Mass 2.0 at (3.0, 3.0)
   - Mass 4.0 at (1.5, 1.5)

 *)

let quadtree_example_node1 () = 
  let b1: centroid = (2.0 , (3.0, 3.0)) in
  let b2: centroid = (4.0 , (1.5, 1.5)) in
  Node (centroid_sum b1 b2, {
    ul = Empty;
    ur = Leaf (2.0 , (3.0, 3.0));
    ll = Leaf (4.0 , (1.5, 1.5));
    lr = Empty;
  })


(* Now, let's consider something just a bit more complex. Generate a
   quadtree containing the following bodies

   - Mass 2.0 at (1.25, 1.25)
   - Mass 4.0 at (0.75, 0.75)
 *)
let quadtree_example_node2 () = 
  let b1 : centroid = (2.0 , (1.25, 1.25)) in
  let b2 : centroid = (4.0 , (0.75, 0.75)) in
  Node (centroid_sum b1 b2, {
    ul = Empty;
    ur = Leaf (2.0 , (1.25, 1.25));
    ll = Leaf (4.0 , (0.75, 0.75));
    lr = Empty;
  })



(* Now that you've had a little practice seeing how quadtrees are
   built, let's start on some real functions that operate on this
   datatype! *)

(*----------------------------------------------------------------------------*)
(*-- Problem 6 (QuadTree Lookup) ---------------------------------------------*)
(*----------------------------------------------------------------------------*)


(* To get your feet wet working with the quadtree datatype, we will
   write a function that, given a quadtree, bounding box, and point,
   will return whether or not the quadtree contains a body at the
   given point.

   Three things to keep in mind as you write this function....

   - Due to the inexact nature of floating point numbers, one should
     use the 'close_enough' function given in nbody.ml when trying to
     determine when two floats are equal. We have opened the NBody
     module at the top of this file, so you can just use the function
     directly.

   - Because the children of a particular node are located in the
     'quads' record, you will have to directly reference the four
     fields of the quads record to access the children of a particular
     node. For example, if qds is the 'quads' record of a Node, you
     can access the upper left subtree by doing qds.ul.

     This might be useful as part of a match case like this:

       match q with
       | ...
       | Node (_, qds) ->  ... qds.ul ...

   - The functions defined above will be useful in this task. You'll
     have to figure out which ones, and how, though :-).  
**)

(* Given a quadtree, bounding box representing the area encompassed by
   that quadtree, and a point, determines whether or not the given
   point is in the given quadtree. 
*)


let rec qlookup (q: qtree) (p: point) (bb: bounding_box) : bool =
  match q with 
  | Empty -> false
  | Leaf (_, p') -> close_enough p p'
  | Node (_, qs) ->
      (
        match quadrant p bb with
        | UL -> qlookup qs.ul p (quadrant_of UL bb)
        | UR -> qlookup qs.ur p (quadrant_of UR bb)
        | LL -> qlookup qs.ll p (quadrant_of LL bb)
        | LR -> qlookup qs.lr p (quadrant_of LR bb)
      )

let test () : bool =
  let qt = quadtree_example_leaf () in
  let p = (1.0, 1.0) in
  qlookup qt p bb0to4
;; run_test "qlookup: leaf" test

let test () : bool =
  let qt = quadtree_example_node1 () in
  let p = (1.0, 1.0) in
  qlookup qt p bb0to4
;; run_test "qlookup: node 1" test

let test () : bool =
  let qt = quadtree_example_node2 () in
  let p = (1.0, 1.0) in
  qlookup qt p bb0to4
;; run_test "qlookup: node 2" test

(* You are responsible for creating tests for qlookup, based on the example quadtrees
 * above. The TAs will be grading your tests. *)


(*----------------------------------------------------------------------------*)
(*-- Problem 7 (QuadTree Acceleration) ---------------------------------------*)
(*----------------------------------------------------------------------------*)

(* Next, we can write a function that uses a quad tree to approximate
 * the forces acting on a body. Remember, the whole point of using
 * quad trees is to calculate accelerations more efficiently than the
 * brute-force method that you implemented earlier!

   The rules for calculating acceleration on a body from a quad tree
   are simple:

     - The Empty quad tree exerts no force, so its effect on the
       acceleration of the body is zero.

     - A Leaf quad tree just uses the acc_on function defined earlier.

     - A Node quad tree exerts a force as though it were a "virtual" body
       at its centroid, unless:
          (1) the point being accelerated is inside the bounding box of the
              quad tree
       or (2) the distance between the centroid and the body is below some
              threshold (i.e. mag (pos1 --> pos2) < thresh).

   In these latter two cases the point is "too close" to the masses in
   the quad tree to use their centroid as an approximation, so we
   recursively calculate accelerations on the point due to the four
   quadrant qtrees and sum them to find a better approximation. 
*)

(* Calculate the acceleration on the point in pos1 by the quadtree q *)
let acc_by_qtree (pos1: point) (q: qtree) (bb: bounding_box) (thresh: float) : vec =
  let rec aux (q: qtree) (bb:bounding_box) (acc: vec) : vec =
    match q with
  | Empty -> acc
  | Leaf (m, p) -> zero ++ acc_on pos1 m p 
  | Node (c, qs) ->(
    let cm, cp = c in
    let d = pos1 --> cp |> mag in
    if (d > thresh) && (in_bounding_box pos1 bb |> not) then
      acc_on pos1 cm cp
    else
      (* recurse on quadrants *)
      let acc_ur = aux qs.ur (quadrant_of UR bb) acc in
      let acc_ll = aux qs.ll (quadrant_of LL bb) acc in
      let acc_ul = aux qs.ul (quadrant_of UL bb) acc in
      let acc_lr = aux qs.lr (quadrant_of LR bb) acc in
      (* sum the four quadrants *)
      (acc_ur ++ acc_ll) ++ (acc_ul ++ acc_lr)
    )
    in
  aux q bb zero


(* The threshold we pick for these two tests doesn't matter, because our
   test points are all in the bounding box. *)
let test () : bool =
  let p = (1.0, 1.0) in
  zero = acc_by_qtree p Empty bb0to4 0.0
;; run_test "acc_by_qtree 1" test

let test () : bool =
  let qt = Leaf (10000000.0, (2.0, 3.0)) in
  let p = (2.0, 2.0) in
  (0.0, 0.000667428) = acc_by_qtree p qt bb0to4 0.0
;; run_test "acc_by_qtree 2" test

(* On the other hand, the threshold does matter for points outside the bounding
   box. *)

let qtree = (Node ((12345678.0, (3.0, 3.0)),
             {ul = Empty;
              ur = Empty;
              ll = (Leaf(12345678.0, (2.0, 2.0)));
              lr = Empty}))

let test () : bool = 
   let v1 = acc_by_qtree (5.0, 2.0) qtree bb0to4 2.236 in
   let v2 = (-0.00014739893883543214, 7.36994694177160698e-05) in
   close_enough v1 v2
;; run_test "acc_by_qtree (5.0, 2.0) qtree bb0to4 2.236" test

let test () : bool = 
   let v1 = acc_by_qtree (5.0, 2.0) qtree bb0to4 0.0 in
   let v2 = (-0.000823985117618399889, 0.) in
   close_enough v1 v2
;; run_test "acc_by_qtree (5.0, 2.0) qtree bb0to4 0.0" test


(* Be sure to add your own (ungraded) test cases, too! *)

(*----------------------------------------------------------------------------*)
(*-- Problem 8 (QuadTree Insertion) ------------------------------------------*)
(*----------------------------------------------------------------------------*)

(* The last problem, qinsert, defines how to create quadtrees by
 * inserting points into them. It is reminiscent of insertion into binary
 * search trees, but, because it may need to subdivide the space
 * repeatedly, it doesn't follow the same recursive structure. *)

(* One bit of syntax you'll find helpful for the next problem is the
 * 'with' keyword, which constructs a new record value from an
 * existing record value, but with a different value for a field *)
let test () : bool =
  let bb    = { minx = 0.0; maxx = 4.0; miny = 0.0; maxy = 4.0 } in
  let newbb = { minx = 999.0; maxx = 4.0; miny = 0.0; maxy = 4.0 } in
  newbb = { bb with minx = 999.0 }
;; run_test "'with' syntax example" test

(* Constructing a quadtree requires many calculations with floats, which 
 * - by virtue of the fact that computers must approximate values not
 * representable as a summation of powers of two - are inherently imprecise
 * and may introduce small errors in your calculations (e.g. 1.1 + 1.1 might
 * evalue to 2.200000001 instead of 2.2 exactly). 
 * 
 * Because of this, we may not get a correct answer if we try to compare 
 * quadtrees for strict equality. We define the function [remarkably_similar]
 * below, which checks if two qtrees are equal within some small error margin.
*)
let rec remarkably_similar (q1: qtree) (q2: qtree) : bool =
  begin match q1, q2 with
  | Empty, Empty -> true
  | Leaf (m1, p1), Leaf (m2,p2) -> m1 = m2 && p1 = p2
      (* Note: we say "p1 = p2" instead of "close_enough p1 p2" because the points are
               given not calculated, which means they will be exactly equal rather than
               approximately. *)
  | Node ((m1, p1), quads1), Node ((m2, p2), quads2) ->
      (* However, the position of the centroid is a weighted average of
         all of the points in the subtrees, so the order that the
         points are added may make subtle differences in the answer. *)
      m1 -. m2 < 0.000001 && close_enough p1 p2 &&
      remarkably_similar quads1.ul quads2.ul &&
      remarkably_similar quads1.ur quads2.ur &&
      remarkably_similar quads1.lr quads2.lr &&
      remarkably_similar quads1.ll quads2.ll
  | _, _ -> false
  end



(* [qinsert] is a very sophisticated function, so we are going to ask you to 
 * develop this function iteratively. You'll write several partial implementations
 * that will each implement a subset of the behavior of qinsert, culminating in a
 * full working implementation! 
 *
 * Here are some things to keep in mind during the process of developing [qinsert].
 *
 *  - If you're stuck, study the quadtree example given on the homework page; 
 *    thinking about structure of the quadtree returned in each part of the
 *    example will help you figure out how to structure the recursion
 *    for this problem. 
 *
 *  - If you're stuck, drawing a picture by hand can help you understand the problem
 *
 *  - Remember that we've given you a function [centroid_sum] to help update
 *    centroids!
 *
 *  - Be sure to look back at the bounding box and quadrant code at the
 *    beginning of the page; some of this will be useful here!  
 *)

(* First, let's write a version of [qinsert] called [qinsert_empty] that only
 * passes the test cases where [q] is Empty. For the time being, you can just 
 * have your function fail (using [failwith]) on other inputs.
 * 
 * Along with handling the Empty case, your code should also fail (via [failwith]) 
 * if the point [p] is not within the bounding box [bb]. 
 *)
let qinsert_empty (q: qtree) (m: float) (p: point) (bb: bounding_box) : qtree =
  if not (in_bounding_box p bb) then failwith "qinsert_empty: point not in bounding box"
  else
  match q with
  | Leaf _ -> failwith "qinsert_empty: called on Leaf"
  | Node _ -> failwith "qinsert_empty: called on Node"
  | Empty -> Leaf (m, p)


let test () : bool =
  let t1: qtree = qinsert_empty Empty 1.0 (1.5, 2.5) bb0to4 in
  remarkably_similar t1 q1
;; run_test "qinsert_empty: insert into an empty tree" test

let test () : bool =
  let t1: qtree = qinsert_empty Empty 1.0 (5.5, 2.5) bb0to4 in
  remarkably_similar t1 q1
;; run_failing_test "qinsert_empty: not in bounding box" test

let test () : bool =
  let t1: qtree = qinsert_empty q1 1.0 (1.5, 2.5) bb0to4 in
  remarkably_similar t1 q2
;; run_failing_test "qinsert_empty: one step at a time" test


(* Next, let's write a version of qinsert called [qinsert_node]. This version,
 * to start, must implement _all_ of the behavior of [qinsert_empty] above. In
 * addition, we would like you to implement the behavior of qinsert when inserting 
 * into a Node. This version of qinsert will only handle insertion into empty 
 * quadrants (but the four quadrants are not necessarily all empty).
 * 
 * Note that since you aren't implementing the Leaf behavior yet, your code should
 * fail if it encounters a Leaf while recursing down the tree (or if the initial 
 * tree is a Leaf).
 *
 * For example, if a Node contains a Leaf only in the UL quadrant, qinsert_node will
 * fail when you attempt to insert a point into the UL quadrant. However, since the 
 * UR, LL and LR quadrants are empty, qinsert_node should correctly insert a point 
 * into any of these. This behavior of qinsert is also demonstrated by the insertion 
 * of the green body into the tree q2 on the assignment webpage.
 * 
 * The empty quadrant that you are inserting into may be anywhere in the tree so 
 * this function should use recursion to find the Empty subtree where [p] should
 * be inserted.
 *)
let rec qinsert_node (q: qtree) (m: float) (p: point) (bb: bounding_box) : qtree =
  match q with
  | Empty -> Leaf (m, p)
  | Leaf _ -> failwith "qinsert_node: called on Leaf"
  | Node (c, qs) ->
      let qs =
      match quadrant p bb with
      | UL -> { qs with ul = qinsert_node qs.ul m p (quadrant_of UL bb) }
      | UR -> { qs with ur = qinsert_node qs.ur m p (quadrant_of UR bb) }
      | LL -> { qs with ll = qinsert_node qs.ll m p (quadrant_of LL bb) }
      | LR -> { qs with lr = qinsert_node qs.lr m p (quadrant_of LR bb) }
      in
    Node (centroid_sum c (m, p), qs)




let test () : bool =
  let t1: qtree = qinsert_node Empty 1.0 (1.5, 2.5) bb0to4 in
  remarkably_similar t1 q1
;; run_test "qinsert_node: insert into empty tree" test

let test () : bool = 
  let t3 : qtree = qinsert_node q2 2.0 (1.0, 1.0) bb0to4 in
  remarkably_similar t3 q3
;; run_test "qinsert_node: insert into an empty quadrant" test      

let test () : bool =
  let t2 : qtree = qinsert_node q1 1.0 (2.1, 2.1) bb0to4 in
  remarkably_similar t2 q2
;; run_failing_test "qinsert_node: insert into a leaf quadrant" test


(* Now that you have the Empty and Node cases finished, finish off [qinsert]
 * by writing in the behavior for the Leaf case. You should study the structure
 * of the quadtree before and after insertion into a Leaf in the assignment 
 * webpage examples to help you figure out what to 
 * do here.
 *
 * There is one edge case that arises in the Leaf case when the point you
 * are attempting to insert is on top of (as determined by [close_enough]) of
 * the existing point in the Leaf. In this case, you should return a Leaf
 * containing one point whose mass is the sum of the masses of the existing
 * and inserted bodies.
 *
 * When you have successfully implemented the Leaf behavior, you should pass
 * all four [qinsert] tests we provide below. Note that the tests we provide
 * below DO NOT test some edge cases in the [qinsert] specification. 
 *)
let rec qinsert (q: qtree) (m: float) (p: point) (bb: bounding_box) : qtree =
  match q with
  | Empty -> Leaf (m, p)
  | Leaf (lm, lp) when close_enough lp p -> Leaf (m +. lm, p)
  (* In this case a leaf needs to be split up into quadrants. And then the new Point needs to be inserted into the correct quadrant. *)
  | Leaf (lm, lp) -> 
      let qs = { ul = Empty; ur = Empty; ll = Empty; lr = Empty } in
      let qs =
      match quadrant lp bb with
        | UL -> { qs with ul = Leaf (lm, lp) }
        | UR -> { qs with ur = Leaf (lm, lp) }
        | LL -> { qs with ll = Leaf (lm, lp) }
        | LR -> { qs with lr = Leaf (lm, lp) }
      in
      let node = Node ((lm, lp), qs) in
      qinsert node m p bb

  | Node (c, qs) ->
      let qs =
        match quadrant p bb with
      | UL -> { qs with ul = qinsert qs.ul m p (quadrant_of UL bb) }
      | UR -> { qs with ur = qinsert qs.ur m p (quadrant_of UR bb) }
      | LL -> { qs with ll = qinsert qs.ll m p (quadrant_of LL bb) }
      | LR -> { qs with lr = qinsert qs.lr m p (quadrant_of LR bb) }
in
    Node (centroid_sum c (m, p), qs)


(* The following test cases for qinsert correspond to the homework
   webpage. See the pictures there to see exactly what these tests
   should look like. *)
let test () : bool =
  let t1: qtree = qinsert Empty 1.0 (1.5, 2.5) bb0to4 in
  remarkably_similar t1 q1
;; run_test "qinsert: insert into empty tree" test

let test () : bool =
  let t1: qtree = qinsert Empty 1.0 (1.5, 2.5) bb0to4 in
  let t2: qtree = qinsert t1 1.0 (2.1, 2.1) bb0to4 in
  remarkably_similar t2 q2
;; run_test "qinsert: insert into a leaf quadrant" test

let test () : bool =
  let t1: qtree = qinsert Empty 1.0 (1.5, 2.5) bb0to4 in
  let t2: qtree = qinsert t1 1.0 (2.1, 2.1) bb0to4 in
  let t3: qtree = qinsert t2 2.0 (1.0, 1.0) bb0to4 in
  remarkably_similar t3 q3
;; run_test "qinsert: insert into a leaf and empty quadrants" test

let test () : bool =
  let t1: qtree = qinsert Empty 1.0 (1.5, 2.5) bb0to4 in
  let t2: qtree = qinsert t1 1.0 (2.1, 2.1) bb0to4 in
  let t3: qtree = qinsert t2 2.0 (1.0, 1.0) bb0to4 in
  let t4: qtree = qinsert t3 1.0 (2.6, 2.8) bb0to4 in
  remarkably_similar t4 q4
;; run_test "qinsert: nested node quadrants" test


(* FINALLY, given qinsert, we can iteratively apply it to a list of bodies to
   build a quad tree. Hint: use a higher-order function! *)
let rec build_qtree_in (bodies: body list) (bb: bounding_box) : qtree =
  HigherOrderFunctions.fold (fun (b: body) (acc: qtree) -> qinsert acc b.mass b.pos bb) Empty bodies

(* Be sure to add your own tests for build_qtree_in. You should be
   able to use the trees defined above for qinsert in building your
   test cases *)



(* Now that we've finished a function to build quadtrees from lists of bodies, we also
   can much more succinctly build test cases for acc_by_qtree *)
let b0 : body = { mass = 100.0; pos = (1.0, 3.0); vel = zero }
let b1 : body = { mass = 101.0; pos = (3.0, 3.0); vel = zero }
let b2 : body = { mass = 102.0; pos = (3.0, 1.0); vel = zero }
let b3 : body = { mass = 103.0; pos = (1.0, 1.0); vel = zero }
let b4 : body = { mass = 104.0; pos = (2.0, 2.0); vel = zero }
let b5 : body = { mass = 105.0; pos = (1.1, 3.0); vel = zero }

let test () : bool =
  let bodies = [b0; b1; b2; b3; b4; b5] in
  let qt = build_qtree_in bodies bb0to4 in
  let p = (0.43, 3.83) in
  close_enough
    (9.70080811445949e-09, -1.27309104915603784e-08)
    (acc_by_qtree p qt bb0to4 0.0)
;; run_test "acc_by_qtree 3" test
