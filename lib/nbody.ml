(*----------------------------------------------------------------------------*)
(*-- n-body simulation -------------------------------------------------------*)
(*----------------------------------------------------------------------------*)

(* This module gives implementations of a 2D n-body simulation using
   a naive algorithm that simply does pair-wise calculation of gravitational
   attraction for all bodies in the system.

   This is a 2D gravity simulation, in which free-floating idealized
   "bodies" with specified masses interact by gravity in a
   frictionless environment.  With small numbers of bodies, we can
   simulate solar systems; with larger numbers of bodies, we can
   simulate galaxies or star clusters.

   For more information see:
     http://blogs.discovermagazine.com/cosmicvariance/2006/07/23/n-bodies/
 *)

;; open Assert

(*----------------------------------------------------------------------------*)
(*-- Problem 2 (Geometry and Physics) ----------------------------------------*)
(*----------------------------------------------------------------------------*)

(* Problem 2 builds a small library for manipulating points and vectors. Be
	sure to read through this entire section. The library is pretty
	straightforward, so we aren't going to ask you to write any code. However,
	you do need to complete the test cases to show that you understand how this
	library (and floating point arithmetic) works. *)

(* We model a point as a pair of floating-point coordinates. *)

type point = float * float

(* A vector is a displacement between two points defined by its length
   (or "magnitude") and direction.  We represent a vector as a pair of
   floating-point numbers, which can be read as the x and y
   displacements from the origin.  *)

type vec = float * float

(* Now we define a few basic operations on points and vectors.  Note
   that OCaml's primitive operations on floating-point numbers are
   written with dots: for example, addition is +. and *. is
   multiplication.  *)

(* We can displace a point by a vector by just adding the change 
* in x and y to that location. *)
let displace ((x, y): point) ((dx, dy): vec) : point =
  (x +. dx, y +. dy)

(* We can also calculate the magnitude of a vector by using the
 *  Pythagorean theorem to find the length of the displacement:
 *
 *    mag = sqrt(x^2 + y^2)
 *)

let mag_squared ((x, y): vec) : float = x *. x +. y *. y
let mag (v: vec) : float = sqrt (mag_squared v)

(* OCaml allows the definition of new infix operators. The binary operator 
 * below, written c *$ v, scales a vector by a constant. This operation
 * changes the maginitude of a displacement, but not its direction.  *)
let ( *$ ) (c: float) ((x1, y1): vec) : vec =
  (c *. x1, c *. y1)

(* The unit vector in a direction given by the vector v has the same
   direction as v but magnitude 1.0 *)
let unit_vec (v: vec) : vec = (1.0 /. mag v) *$ v

(* The operator below, written p1 --> p2, is an expression which evaluates to
   the vector from point p1 to point p2. The distance between p1 and p2 is 
   the magnitude of this vector. *)
let (-->) ((x1, y1): point) ((x2, y2): point) : vec =
  (x2 -. x1, y2 -. y1)


(* We define the symbol ++ to perform vector addition. This operation 
   combines the displacement of two different vectors into one. *)
let (++) ((x1, y1): vec) ((x2, y2): vec) : vec =
  (x1 +. x2, y1 +. y2)

(* The zero vector has zero displacement in both directions. *)
let zero : vec = (0.0, 0.0)


(*----------------------------------------------------------------------------*)

(* To make sure that you understand how to work with points and
   vectors, complete these these tests for the vector operations
   above. This part is just to check your understanding, we will not be 
   grading these tests. *)

let test () : bool =
   (2.0 *$ (2.0, 4.0)) = failwith "fill in the answer here"
;; run_test "scale vector by 2" test

let test () : bool =
  (1.0, 2.0) ++ (2.0, 3.0) = failwith "fill in the answer here"
;; run_test "vector addition" test

let test () : bool =
  let v = (2.0, 5.0) in
  mag_squared (5.0 *$ v) = failwith "fill in the answer here"
;; run_test "mag scaled vector" test

(* Working with floating point numbers is difficult in any programming
 * language. Because computers approximate their representation, some
 * equalities just don't hold. When we compare points for equality we
 * can get some strange answers for our tests if we use floating
 * point equality. *)
let exactly_equal ((x1, y1): point) ((x2, y2): point) : bool =
  (x1 = x2) && (y1 = y2)

(* To see why this test case computes false instead of true,
 * try evaluating 69.20 +. 0.62 in the OCaml toplevel. *)
let test () : bool =
  let p1 = (0.0, 69.82) in
  let p2 = (0.0, 69.20) ++ (0.0, 0.62) in
  not (exactly_equal p1 p2)
;; run_test "too_exact: almost collided, but not quite" test

(* To avoid spurious failures due to round-off error, we consider two
 * points or two vectors equal if they are close to each other,
 * even if they are not exactly equal.  We define close enough to be
 * that the square of the distance between them is less than 0.00001. *)
let close_enough (p1: float * float) (p2: float * float) : bool =
  mag_squared (p1 --> p2) < 0.00001

let test () : bool =
  let p1 = (0.0, 69.82) in
  let p2 = (0.0, 69.20) ++ (0.0, 0.62) in
  close_enough p1 p2
;; run_test "close_enough: almost equal, but not quite" test


(* To print floating point numbers with all of the digits use 
 * the following printing function. *)
let better_print_float (f: float) : unit = Printf.printf "%.64f" f


(*--------------------------*)
(* Physics                  *)
(*--------------------------*)

(* You do not need to do anything for the rest of this problem, but be sure that
 * you read through the functions and test cases below. You'll need them for
 * the rest of the homework. *)

(* Below, we will represent an astronomical body, such as a planet or star, by
 * its mass, position, and velocity.  (Throughout, we assume SI units: mass in
 * kg, length in meters, time in seconds.) 
 * 
 * We represent the velocity of a body using a vector. This vector represents 
 * how the position of the body changes over time.
 *)


(* A key part of the n-body simulation is calculating gravitational
 * acceleration --- i.e. the effect of the gravitational pull of other bodies
 * in the simulation. This gravitational acceleration changes how bodies 
 * move through space: a massive body can pull others towards it. 
 *
 * We also represent acceleration with vectors, but this time the vector 
 * represents a change in velocity, not a change in position. *)


(* The acc_on function computes the acceleration of one body due to the force
 * of gravity induced by another; this is independent of the velocity of both.
 * It only depends on the distance between the two bodies (r) and the mass
 * of the second body (m2). If the points have the same position then the
 * acceleration is zero. Otherwise, the magnitude of the acceleration is
 *
 *       g * m2 / r^2
 *
 * where g is the universal gravitational constant. The direction of the 
 * acceleration is the direction between the two points. *)

let g = 6.67428e-11 (* N (m/kg)^2 *)

let acc_on (pos1: point) (m2: float) (pos2: point) : vec =
  begin match (close_enough pos1 pos2) with
  | true -> zero
  | false ->
      let disp12 = pos1 --> pos2  in (* vector representing the displacement *)
      let r2 = mag_squared disp12 in (* magnitude of the vector, squared *)
      let aMag = g *. m2 /. r2    in (* magnitude of the acceleration *)
      let aDir = unit_vec disp12  in (* direction of the acceleration *)
        aMag *$ aDir
  end

(* For example, suppose we have two bodies, b1 and b2, where
      b1 has position (0.0, 0.0)   and mass 1.0e24 and
      b2 has position (0.0, 1.0e8) and mass 2.0e24
   We can calculate how b2's gravitational force affects b1 and
   how b1's gravitational force affects b2 using the following
   two tests. Because b2 is more massive, it has a larger effect.
   Furthermore, b1 will be pushed slightly up (i.e. b1's velocity will 
   increase in the y direction) and b2 will be pushed, even less 
   slightly, down (i.e. b2's velocity will decrease in the y 
   direction).
*)

let test () : bool =
  let b1_pos      = zero   in
  let b2_mass     = 2.0e24 in
  let b2_pos      = (0.0, 1.0e8) in
  let b1_acc = acc_on b1_pos b2_mass b2_pos in
  close_enough b1_acc (0., 0.01334856)
;; run_test "acc_on b1 by b2" test

let test () : bool =
  let b2_pos   = (0.0, 1.0e8) in
  let b1_mass  = 1.0e24 in
  let b1_pos   = zero in
  let b2_acc   = acc_on b2_pos b1_mass b1_pos in
  close_enough b2_acc (0., -0.00667428)
;; run_test "acc_on b2 by b1" test

(* The main idea of this simulation is that we'll have a collection of
   bodies, each with a position, mass, and velocity. At each timestep,
   the simulator will calculate a new position and velocity for each
   body, reflecting how the forces of gravity interact with each
   other.

   The new position of a body is calculated by the following equation,
   based on its old position (pos), old velocity (v), timestep (t), and
   acceleration (a):

     pos' = pos + vt + (1/2)at^2   -- the new position after time step t

   Likewise, given the old velocity, acceleration and timestep, we can
   calculate the new velocity of the body.

     v'   = v + at                 -- the new velocity after time step t

   Using the operations on points and vectors, we can implement these
   equations with the following functions. *)

type timestep = float

(* The new position position of point p, moving with velocity v
   and accelerated by vector a, after time step t. *)
let new_position (pos: point) (t: timestep) (v: vec) (a: vec) : point =
  displace pos ((t *$ v) ++ ((0.5 *. t *. t) *$ a))

(* The new velocity of the body after time step t *)
let new_velocity (v: vec) (a: vec) (t: timestep) : vec =
  v ++ (t *$ a)

(* For example, suppose we have two bodies, b1 and b2, where
      b1 has position (0.0, 0.0)   and mass 1.0e24
            and velocity (0.0, 0.0)
      b2 has position (0.0, 1.0e8) and mass 2.0e24
            and velocity (0.0, 9000.0)
   From the test cases for acc_on above, we know that
       b1's acceleration is (0., 0.01334856) from the pull of b2
   and b2's acceleration is (0., -0.00667428) from the pull of b1 *)

let test () : bool =
  let timestep    = 100.0  in
  let b1_pos      = zero   in
  let b1_velocity = zero   in
  let b1_acc      = (0., 0.01334856) in
  close_enough
    (new_position b1_pos timestep b1_velocity b1_acc)
    (0.00, 66.7428)
  &&
  close_enough
    (new_velocity b1_velocity b1_acc timestep)
    (0.00, 1.334856)

;; run_test "new position/velocity b1" test

let test () : bool =
  let timestep    = 100.0  in
  let b2_pos      = (0.0, 1.0e8) in
  let b2_velocity = (0.0, 9000.0)   in
  let b2_acc      = (0., -0.00667428) in
  close_enough
    (new_position b2_pos timestep b2_velocity b2_acc)
    (0.00, 100899966.6286000013)
  &&
  close_enough
    (new_velocity b2_velocity b2_acc timestep)
    (0.00, 8999.332572)

;; run_test "new position/velocity b2" test

(* To summarize: the important functions in this section are 
 *  [acc_on], [new_position] and [new_velocity].  
 * You should have an intuition about what these functions do before you proceed
 * to the next part. However, don't worry too much if you don't understand the 
 * precise connection between the mathematical formula and the implementations 
 * of these functions. We've done that part for you (this isn't physics class)
 * so that you can get to the fun part of the simulation.
 *)


(*----------------------------------------------------------------------------*)
(*-- Problem 3 (Mutating Bodies) ---------------------------------------------*)
(*----------------------------------------------------------------------------*)

 (* Unlike the pure mathematical values we've looked at so far in this
   class, OCaml allows record fields marked 'mutable' to be updated as
   computation proceeds. We exploit that feature here to simplify
   updating the state of each body of the simulation.  *)

(*--------------------------*)
(* Creating mutable state   *)
(*--------------------------*)


(* A body has mass, position, and velocity. *)

type body = {
   mass: float;
   mutable pos: point;
   mutable vel: vec;
}

(* A record field value is 'mutable' if the program can modify the value
   "in place" by using a command to update the contents of the field.

   Such fields are marked with the 'mutable' keyword.

   For example, the n-body simulator works by taking a collection of
   "bodies" (i.e. stars, planets, comets, etc.) and simulating their
   movement over time.  During the simulation, all of the bodies
   change their positions and and velocities. However, the mass of
   each body is constant. Therefore, we represent a body by a record
   where the mass is immutable, but the position and velocity can be
   updated.  *)


(* These "body constructors" are provided in case you want to play
   around with various solar-system-like simulations.  They are
   defined via no-argument functions that return a fresh body.  For
   example, every function call "sun ()", returns a new body standing
   still at the center of the universe. *)

let sun     () : body
  = { mass = 2.0e30; pos = (0.0, 0.0); vel = (0.0, 0.0) }
let mercury () : body
  = { mass = 3.30e23; pos = (57910000.0, 0.); vel = (9000.0, 9000.0) }
let venus   () : body
  = { mass = 4.87e24; pos = (0.0, 108200000.0); vel = (-9000.0, 0.0) }
let earth   () : body
  = { mass = 5.98e24; pos = (-149600000.0, 0.0); vel = (0.0, -9000.0) }
let mars    () : body
  = { mass = 6.42e23; pos = (0.0, -227940000.0); vel = (600000.0, 0.0) }
let jupiter () : body
  = { mass = 1.90e27; pos = (778330000.0, 0.0); vel = (0.0, 450000.0) }
let saturn  () : body
  = { mass = 5.69e26; pos = (0.0, 1426940000.0); vel = (-300000.0,0.0) }
let uranus  () : body
  = { mass = 8.69e25; pos = (-2870990000.0, 0.0); vel = (0.0, -200000.0) }

(*--------------------------*)
(* Updating bodies          *)
(*--------------------------*)

(* The command to update a field f of some record value r is written:

     r.f <- v

   Commands are *separated* with ';'.  For example, to run one command
   followed by another, write:

     r.field1 <- val1;
     r.field2 <- val2

   NOTE: The last command in the sequence does not end with ';'

   The following test illustrates these ideas by updating the position
   component of a body.
*)

let test () : bool =
  (* Allocate a body value with mutable pos and vel fields.
     To create a record, list the values for the fields in braces. *)
  let b: body = { mass = 1.0; pos = zero; vel = zero } in
  (* Next, update the value bound to the b.pos field. *)
  b.pos <- (1.0, 1.0);    (* Note we use ; instead of 'in' with <- *)
  b.pos <> zero           (* b.pos has been updated *)
    && zero = (0.0, 0.0)  (* but zero itself has not been changed. *)
;; run_test "mutable state 1" test


(* At every timestep, the simulator must update each body with its new
   position and velocity.

   To practice with the syntax of record update, use the new_position and
   new_velocity function above to complete the step_body function below. This
   function should first update the position and then the velocity of the
   given body according to the above functions.  *)
let step_body (b: body) (a: vec) (t: timestep) : unit =
  failwith "step_body not implemented"

let test () : bool =
  let b1       = { mass = 1.0e24; pos = zero; vel = zero } in
  let timestep = 100.0 in
  let b1_acc   = (0., 0.01334856) in
  step_body b1 b1_acc timestep;
  b1.mass = 1.0e24
    && close_enough b1.pos (0.00, 66.7428)
    && close_enough b1.vel (0.00, 1.334856)
;; run_test "step_body b1" test



(*---------------*)
(*-- Iteration --*)
(*---------------*)

(* Given the ability to mutate fields of a structure, a common task is
   to "iterate" over a list of mutable values and apply a command
   to each element.  The command returns a unit type so that,
   unlike fold or transform, there is no value accumulated or returned.  *)

(* Write a function 'iter' that applies the given processing command
   to each element of the list.  (Note: this problem is essentially
   asking you to implement the List.iter library function -- so you
   should not use that here.) *)
let rec iter (process: 'a -> unit) (l: 'a list) : unit =
  failwith "iter unimplemented"

let test () : bool =
  let l: body list = [ { mass = 1.0; pos = zero; vel = zero };
                       { mass = 1.0; pos = zero; vel = zero } ] in
  let _ = iter (fun b -> b.pos <- (1.0, 1.0)) l in
  begin match l with
  | [b1; b2] -> b1.pos = (1.0, 1.0) && b2.pos = (1.0, 1.0)
  | _ -> false
  end
;; run_test "iter" test

let iter_test_list : body list =
   let b = { mass = 1.0; pos = zero; vel = zero } in
   [b; b; b]

(* A useful application of iter is to print out a list of bodies. *)

let print_bodies (bs: body list) : unit =
  iter (fun b ->
    Printf.printf "(%.32f, %.32f)\n    moving (%.32f, %.32f)\n"
      (fst b.pos) (snd b.pos) (fst b.vel) (snd b.vel)
  ) bs

(* For example, uncomment this command to see some planets. *)
(* ;; print_bodies [sun (); mars (); jupiter (); saturn (); uranus ()] *)


(* Write a variant of iter that processes two lists in parallel.
   If the lists are of unequal length then the iteration should
   terminate when either list is []. *)
let rec iter2 (process: 'a -> 'b -> unit) (l1: 'a list) (l2: 'b list) : unit =
  failwith "iter2 unimplemented"

let test () : bool =
  let l1: body list = [ { mass = 1.0; pos = zero; vel = zero };
                        { mass = 1.0; pos = zero; vel = zero } ] in
  let l2: body list = [ { mass = 1.0; pos = (1.0, 1.0); vel = zero };
                        { mass = 1.0; pos = (2.0, 1.0); vel = zero } ] in
  let _ = iter2 (fun b c -> b.pos <- c.pos) l1 l2 in
  begin match l1, l2 with
  | [b1; b2], [c1; c2] -> b1.pos = c1.pos && b2.pos = c2.pos
  | _, _ -> false
  end
;; run_test "iter2" test

(*----------------------------------------------------------------------------*)
(*-- Problem 4 (Acceleration) ------------------------------------------------*)
(*----------------------------------------------------------------------------*)

(* Now that we can represent a solar system or galaxy as a list of bodies, the 
 * next step is to calculate how they interact with eachother via their 
 * gravitational forces. 
 *)


(* For example, using the [acc_on] function above, we can compute the 
 * acceleration on a body at some position [pos1] by two other bodies [b2] 
 * and [b3], by adding together the accelerations for each.
 *)

let rec accelerate_body_by_two (pos1:point) (b2: body) (b3:body): vec = 
  acc_on pos1 b2.mass b2.pos ++ acc_on pos1 b3.mass b3.pos

let test () : bool = 
   let e = earth () in
   let earth_acc = accelerate_body_by_two e.pos (sun ()) (jupiter ()) in
   close_enough earth_acc (5964.608655,0.000000)  
;; run_test "acc on earth by sun & jupiter together" test


(* Now generalize this acceleration to an arbitrary number of other bodies. 
 * We want to compute the acceleration on a particular body located 
 * at some position [pos1] by all other [bodies] in the simulation. Again, 
 * we compute this result by adding together the individual accelerations 
 * caused by each element of [bodies] on [pos1]. 
 *)

(* You should implement this function using HigherOrderFunctions.fold. 
 * HINT: define the combine function for fold as a helper 
 * functions *inside* accelerate_body.
 *)


let rec accelerate_body (bodies: body list) (pos1: point) : vec =
failwith "accelerate_body: unimplemented"

(* Again, we test whether the vectors are "close_enough", *not* whether
 * they are equal to each other. *)
let test () : bool =
  let planets = [sun (); mars (); jupiter (); saturn (); uranus ()] in
  let b1 = { mass = 1.0e24; pos = (0.0, 0.0); vel = zero } in
  let v = accelerate_body planets b1.pos in
  close_enough v (0.208625865754, 0.0178264304396)
;; run_test "accelerate_body b1 by planets" test

(* Now create two more simple test cases for [accelerate_body].  Don't forget 
 * to test with an empty list! You can also use the data in the test 
 * cases after [acc_on] *)



(* Now, given a list of [bodies] we want to compute *all* of the 
   accelerations of all of the bodies on each other. The result of 
   [accelerations] function should be a list where the first element 
   is the computed acceleration on the first body, the second is 
   the computed acceleration on the second body, and so on.

   Hint: use HigherOrderFunctions.transform. Implementing this function
   directly by recursion will probably be wrong.

   You do not need to write additional test cases for this function,
   unless you need them for debugging.
 *)
let accelerations (bodies: body list) : vec list =
  failwith "Missing implementation for accelerations"


let test () : bool =
  let b1 = { mass = 1.0e24; pos = (0.0, 0.0); vel = zero } in
  let b2 = { mass = 2.0e24; pos = (0.0, 1.0e8); vel = (0.0, 9000.0) } in
  let vecs = accelerations [b1; b2] in
  begin match vecs with
  | [v1; v2] ->
      close_enough v1 (0., 0.01334856) &&
      close_enough v2 (0., -0.00667428)
  | _ -> false
  end
;; run_test "accelerations [b1; b2]" test

let test () : bool =
  let planets = [sun (); mars (); jupiter (); saturn (); uranus ()] in
  let vecs = accelerations planets in
  begin match vecs with
  | [v1; v2; v3; v4; v5] ->
      close_enough v1 (0.2086258658, 0.0178264304) &&
        close_enough v2 (0.1843261992, 2569.2409941745) &&
        close_enough v3 (-220.3542470215, 0.0126009598) &&
        close_enough v4 (0.0224791256, -65.5999969867) &&
        close_enough v5 (16.2074668886, 0.0016440102)
  | _ -> false
  end
;; run_test "accelerations planets" test


(* Finally put it all together in the simulation by updating
   all of the bodies in the list using the step_body function
   with the accelerations calculated by the function above.

   Hint: use iter2 to implement this function. Again, implementing 
   this function with direct recursion will probably be wrong. Start
   by figuring out what two lists need to be passed into iter2.

   You do not need to write additional test cases for this function,
   unless you need them for debugging. After completing this function
   you can also run viewer.exe and compare your solution with the video
   on the assignment webpage.
 *)
let step_bodies (bodies: body list) (dt: timestep) : unit =
  failwith "Missing implementation of step_bodies"

let test () : bool =
  let b1 = { mass = 1.0e24; pos = (0.0, 0.0); vel = zero } in
  let b2 = { mass = 2.0e24; pos = (0.0, 1.0e8); vel = (0.0, 9000.0) } in
  step_bodies [b1; b2] 100.;
  close_enough b1.pos (0.00, 66.7428) &&
  close_enough b2.pos (0.00, 100899966.6286000013)
;; run_test "step_bodies [b1;b2]" test

let test () : bool =
  let planets = [sun (); mars (); jupiter (); saturn (); uranus ()] in
  step_bodies planets 100.;
  begin match planets with
  | [b1; b2; b3; b4; b5] ->
      close_enough b1.pos (1043.1293287710,89.1321521978) &&
      close_enough b2.pos (60000921.6309960112,-215093795.0291272998) &&
      close_enough b3.pos (777228228.7648922205,45000063.0047988296) &&
      close_enough b4.pos (-29999887.6043718122,1426612000.0150666237) &&
      close_enough b5.pos (-2870908962.6655569077,-19999991.7799491249)
  | _ -> false
  end
;; run_test "step_bodies planets" test




(* Here is some data to make your own test case for step_bodies:

After creating the planets as above and then stepping them with a
50.0 timestep, they display as:

(260.78233219275870169440167956054211, 22.28303804944323829317909257952124)
  moving (10.43129328771034813883034075843170, 0.89132152197772951396359530917834)
(30000230.40774900466203689575195312500000, -224728448.75728183984756469726562500000000)
  moving (600009.21630996011663228273391723632812, 128462.04970872697595041245222091674805)
(778054557.19122302532196044921875000000000, 22500015.75119970738887786865234375000000)
  moving (-11017.71235107738721126224845647811890, 450000.63004798826295882463455200195312)
(-14999971.90109295211732387542724609375000, 1426858000.00376677513122558593750000000000)
  moving (-299998.87604371813358739018440246582031, -3279.99984933284304133849218487739563)
(-2870969740.66638946533203125000000000000000, -9999997.94498728029429912567138671875000)
  moving (810.37334442976725767948664724826813, -199999.91779949123156256973743438720703)

After another 50 timesteps, they now print as:

(1042.87948138931710673205088824033737, 96.68864834990955614557606168091297)
  moving (20.85259268015198585999314673244953, 2.08490289004092321789585184887983)
(59571411.75189789384603500366210937500000, -215087728.35209900140762329101562500000000)
  moving (582838.03745599545072764158248901367188, 257166.76649858616292476654052734375000)
(777228378.54471373558044433593750000000000, 44992102.38900864124298095703125000000000)
  moving (-22029.43350929435837315395474433898926, 449682.83546436880715191364288330078125)
(-29999024.92463498935103416442871093750000, 1426612003.78234291076660156250000000000000)
  moving (-299963.24489796336274594068527221679688, -6559.84900762472534552216529846191406)
(-2870908962.73834562301635742187500000000000, -19999921.13195851445198059082031250000000)
  moving (1620.74377732667699092417024075984955, -199997.00967935816152021288871765136719)

Note that due to rounding error, you'll get a different answer if you try a single
timestep of 100. instead of two 50. timesteps.

*)




(*----------------------------------------------------------------------------*)
(*-- Sample n-body systems    ------------------------------------------------*)
(*----------------------------------------------------------------------------*)

(* The following code generates the actual simulation which you can view using
   viewer.ml; you don't have to write any code for this part! *)

(* Once you pass all of the tests above, you should be able to see the
   n-body simulation in action. In the code below, we define two
   sample n-body systems (i.e. lists of bodies) which you can display
   in the viewer. (Just run the viewer executable.)

   The two sample systems defined are:
       planets   - a collection of a few planets around a sun
       collision - a massive gravitational system in which two
         "star clusters" are on a collision course (defined below)

   By default, the viewer shows the "planets" simulation. If you would like to
   change the system that the viewer shows, edit the last line of viewer.ml.

   Take a look at the definitions of these systems below if you would
   like to create your own.
 *)

let sun_mass = 2.0e30
let star m = { mass = m; pos = (0.0, 0.0); vel = (0.0, 0.0) }

let planets = [star sun_mass; mars (); jupiter (); saturn (); uranus ()]

(* Distance to use in the 'collision' simulation *)
let dist = 4000000000.0

(* Generate a random number from a Gaussian distribution centered at d *)
let rec gen_dist (d: float) : float =
  let iter = 10 in
  let k = d /. (float_of_int iter) in
  let rec loop i acc =
    if i = 0 then acc else loop (i - 1) ((Random.float k) +. acc)
  in
    (loop iter 0.0) -. (d /. 2.)

(* calculate the velocity needed to put mass m2 into a circular orbit
   around m1 at radius r *)
let orbital_velocity m1 m2 r =
  sqrt ((m2 *. m2 *. g) /. ((m1 +. m2) *. r))

(* calculate the tangent vector to a circle centered at the origin
   at the point p *)
let tangent_vector (p:point) : vec =
  let (x, y) = p in
  let theta = atan2 x y in
  let vx = 0. -. (cos theta) in
  let vy = (sin theta) in
    (vx, vy)



(* Make a list of n bodies with random masses, distributed around
   a central star. Initialize their starting velocities to put
   them into circular orbits. *)
let mk_lots (star_mass: float) (n: int) : body list =
  let _ = Random.init 17 in
  let mk_one () : body =
    let d = dist *. 2. in
    let mass = (Random.float 1000.0) *. 1e13 in
    let x = gen_dist d in
    let y = gen_dist d in
    let r = mag (x, y) in
    let v = tangent_vector (x, y) in
    let velocity = orbital_velocity mass star_mass r in
      { mass = mass; pos = (x, y); vel = velocity *$ v }
  in
  let rec loop n acc =
    if n = 0 then acc else
      loop (n - 1) ((mk_one ()) :: acc)
  in
    loop n [star star_mass]

(* Displace all of the bodies in a list by a given vector *)
let displace_bodies (bodies:body list) (v:vec) : unit =
  List.iter (fun b -> b.pos <- displace b.pos v) bodies

(* Add a fixed velocity vector to each of the bodies' velocities.
   (This sets an entire collection of bodies moving at a constant
   velocity.) *)
let add_velocity (bodies:body list) (v:vec) : unit =
  List.iter (fun b -> b.vel <- b.vel ++ v) bodies

(* Create a massive gravitational system in which two "star clusters"
   are on a collision course *)
let collision : body list =
  let star_mass = sun_mass *. 0.1 in
  let solar1 = mk_lots sun_mass 1000 in
  let solar2 = mk_lots star_mass 300 in
  let disp1 = ((dist /. 4.), 0.0) in
  let disp2 = (0.0 -. (dist /. 4.), 0.0 -. (dist /. 4.)) in
  let diff = disp1 --> disp2 in
  let v2 = ((orbital_velocity star_mass sun_mass (mag diff)) *. 0.9)
                *$ (tangent_vector diff) in
    begin
      displace_bodies solar1 disp1;
      displace_bodies solar2 disp2;
      add_velocity solar2 v2;
      solar2@solar1
    end

(* --------------------------------------------------------------------- *)

(* After you finish the problems in this file, try experimenting with
   viewer.ml to see the simulation running.

   Then go back and read the next part of the HTML instructions before
   proceeding to the next set of problems. *)
