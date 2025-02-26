[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
(*----------------------------------------------------------------------------*)
(*-- Viewer for quadtree based simulation ------------------------------------*)
(*----------------------------------------------------------------------------*)

(* Like the other viewer, running this file will display the motion of the 
 * the interacting bodies. The viewer has the same interface as before, except
 * that this time it also shows the quadtree divisions. As before, you are 
 * encouraged to read and play with this file, but you do not need to edit it.
 * 
 * By default, the viewer runs with the massive collection called "collision"
 *)

;; open Graphics
;; open Nbody
;; open Qtree

(* The bounding box for the quadtree simulation *)    
 open Qtree.Bbox
let bb = {minx=dist *. -500.; maxx=dist *. 500.;
          miny=dist *. -500.; maxy=dist *. 500.}

(* This function computes the forces acting on a body, and mutates
   the body to reflect the change in position over time span dt. *)
let step_body_with_acc (qt:qtree) (thresh:float) (dt:float) (b:body): unit =
  step_body b (acc_by_qtree b.pos qt bb thresh) dt

(* Simulations are always a tradeoff between speed and accuracy.
   Rather than hard-code a particular tradeoff in the simulation, we
   use higher-order functions to encapsulate the parameters controlling
   the tradeoff. *)
let step_slow (qt:qtree) : body -> unit =
  let dt = 50.0 in
  let thresh = 1000000.0 in
  step_body_with_acc qt thresh dt

(* After running the viewer once, try changing the 'step_slow' at the bottom
 * of the file to 'step_fast' and re-running. *)
let step_fast (qt:qtree) : body -> unit =
  let dt = 200.0 in
  let thresh = 1000000.0 in
  step_body_with_acc qt thresh dt


(* Define a few constants we use for the display. *)
let dim = 1000

let float_to_screen (f:float) : int = 
  (int_of_float (f /. dist *. (float_of_int (dim / 2)))) + (dim / 2)

let draw_centroid (m:float) (x:float) (y:float) (c:color) : unit =
  let px = float_to_screen x in
  let py = float_to_screen y in
   let r = if m > 2.0e28 then 10 else max (min (int_of_float (m /. 2.0e18)) 4) 2 in 
     set_color c;
     fill_circle px py r

let rec draw_qtree (q:qtree) (bb:Bbox.t) (c:int) : unit =
  begin match q with
    | Empty -> ()
    | Leaf(m,(x,y)) -> draw_centroid m x y white
    | Node((m,(x,y)),quads) -> 
        let midx = (float_to_screen (midx bb)) in
        let midy = (float_to_screen (midy bb)) in begin
          set_color blue;
          moveto midx (float_to_screen (bb.miny));
          lineto midx (float_to_screen (bb.maxy));
          moveto (float_to_screen (bb.minx)) midy;
          lineto (float_to_screen (bb.maxx)) midy;
          draw_qtree quads.ll (Quadrant.to_bbox LL bb) (c-5) ;
          draw_qtree quads.lr (Quadrant.to_bbox LR bb) (c-5) ;
          draw_qtree quads.ul (Quadrant.to_bbox UL bb) (c-5) ;
          draw_qtree quads.ur (Quadrant.to_bbox UR bb) (c-5)
      end
  end

(* You can configure the program to run for a given number
   of frames before pausing. *)
let step_counter = {contents=None}
(*let step_counter = {contents=(Some 20)}*)
  
let should_pause () : bool =
  begin match step_counter.contents with
  | None -> false
  | Some n -> (if n > 0 then step_counter.contents <- Some (n - 1));
               n = 0
  end

let step_with (step:body -> unit) (bodies:body list): unit =
    List.iter step bodies

type step_function = qtree -> body -> unit


let run (bodies:body list) (step : step_function): unit =
    open_graph "";
    resize_window dim dim;
    auto_synchronize false;     (* don't draw immediately to the screen *)
    while not (key_pressed ()) do
      clear_graph ();
      set_color black;
      fill_rect 0 0 dim dim;
      let qt = build_qtree_in bodies bb in begin
          draw_qtree qt bb 155;
          synchronize ();           (* show the freshly painted window *)
          (* We've done something interesting here: we take a function
             (step : qtree -> body -> unit) and pass it only one of the
             two arguments it expects. What we get is a function expecting
             the second argument: ((step qt) : body -> unit).
             This trick is called 'currying'. *)
          step_with (step qt) bodies;
      end;
      (* When running continuously, a key press exits the program.
       * After exhuasting the frame budget, a quick press will single-step
       * and a held key will exit the program. *)
      if should_pause () then ignore (wait_next_event [Key_pressed]);
      ignore (wait_next_event [Poll]) 
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
;; run collision step_slow
