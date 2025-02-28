[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Graphics
open Lqtree
open Bbox
open Qtree
open Lqtree.Nbody

(* The bounding box for the quadtree simulation *)

let bb =
  { minx = dist *. -500.; maxx = dist *. 500.; miny = dist *. -500.; maxy = dist *. 500. }
;;

(* This function computes the forces acting on a body, and mutates
   the body to reflect the change in position over time span dt. *)
let step_body_with_acc (qt : Qtree.t) (dt : float) (b : body) : unit =
  step_body b (acc_by_qtree b.pos qt) dt
;;

let step_slow (qt : Qtree.t) : body -> unit =
  let dt = 50.0 in
  step_body_with_acc qt dt
;;

(* Define a few constants we use for the display. *)
let dim = 1000

let float_to_screen (f : float) : int =
  int_of_float (f /. dist *. float_of_int (dim / 2)) + (dim / 2)
;;

let draw_centroid (m : float) (x : float) (y : float) (c : color) : unit =
  let px = float_to_screen x in
  let py = float_to_screen y in
  let r = if m > 2.0e28 then 10 else max (min (int_of_float (m /. 2.0e18)) 4) 2 in
  set_color c;
  fill_circle px py r
;;

let rec draw_qtree (q : Qtree.t) (node_idx : int) (c : int) : unit =
  let node = Qtree.get_node q node_idx in
  match Node.node_type node with
  | Empty when node.next = 0 -> ()
  | Empty -> draw_qtree q node.next c
  | Leaf when node.next = 0 ->
    let m, (x, y) = node.centroid in
    draw_centroid m x y white
  | Leaf ->
    let m, (x, y) = node.centroid in
    draw_centroid m x y white;
    draw_qtree q node.next c
  | Node (*(m, (x, y)), quads*) ->
    let bb = node.bbox in
    let midx = float_to_screen (midx bb) in
    let midy = float_to_screen (midy bb) in
    set_color blue;
    moveto midx (float_to_screen bb.miny);
    lineto midx (float_to_screen bb.maxy);
    moveto (float_to_screen bb.minx) midy;
    lineto (float_to_screen bb.maxx) midy;
    draw_qtree q node.children (c - 5)
;;

(* You can configure the program to run for a given number
   of frames before pausing. *)
let step_counter = { contents = None }
(* let step_counter = {contents=(Some 20)} *)

let should_pause () : bool =
  match step_counter.contents with
  | None -> false
  | Some n ->
    if n > 0 then step_counter.contents <- Some (n - 1);
    n = 0
;;

let step_with (step : body -> unit) (bodies : body list) : unit = List.iter step bodies

type step_function = Qtree.t -> body -> unit

let run (bodies : body list) (step : step_function) : unit =
  open_graph "";
  resize_window dim dim;
  auto_synchronize false;
  (* don't draw immediately to the screen *)
  while not (key_pressed ()) do
    clear_graph ();
    set_color black;
    fill_rect 0 0 dim dim;
    let qt = build_qtree_in bodies bb in
    draw_qtree qt 0 155;
    synchronize ();
    (* show the freshly painted window *)
    (* We've done something interesting here: we take a function
             (step : qtree -> body -> unit) and pass it only one of the
             two arguments it expects. What we get is a function expecting
             the second argument: ((step qt) : body -> unit).
             This trick is called 'currying'. *)
    step_with (step qt) bodies;
    (* Unix.sleepf 0.05; *)
    (* When running continuously, a key press exits the program.
       * After exhuasting the frame budget, a quick press will single-step
       * and a held key will exit the program. *)
    if should_pause () then ignore (wait_next_event [ Key_pressed ]);
    ignore (wait_next_event [ Poll ])
  done

(* Try changing the parameter to step_fast;
 * does the accuracy of the simulation change?
 *)
(* ;; run planets step_slow  *)
(* ;; run collision step_slow  *)

(* You can run a solar system simulation by replacing the collision
 * command above with:
   
;; run planets step_slow
*)

(* ;; run planets step_slow *)
;;

run collision step_slow
