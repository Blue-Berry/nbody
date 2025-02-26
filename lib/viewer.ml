(*----------------------------------------------------------------------------*)
(*-- Viewer for simple n-body simulation -------------------------------------*)
(*----------------------------------------------------------------------------*)

(* Running this file will open a window that displays the n-body simulation. 
 * When running continuously, a key press exits the program.
 * You do not need to edit this file, but you are encouraged to play with
 * it.
 *
 * In particular, the last line starts the simulator with a particular 
 * collection of bodies. Try changing this collection to the one called 
 * "collision" to see what happens.
 *)

open Graphics
open Nbody
      
(* Simulations are always a tradeoff between speed and accuracy.
   Rather than hard-code a particular tradeoff in the simulation, we
   define a few different timesteps to play with. *)
let step_slow : timestep = 0.5
(* let step_fast : timestep = 200.00 *)
	
(* Define a few constants we use for the display. *)
let dim = 1000              
let dist = 4000000000.0
	
let float_to_screen (f:float) : int = 
  (int_of_float (f /. dist *. (float_of_int (dim / 2)))) + (dim / 2)

let draw_centroid (m:float) (x:float) (y:float) (c:color) : unit =
  let px = float_to_screen x in
  let py = float_to_screen y in
   let r = if m > 2.0e28 then 10 else max (min (int_of_float (m /. 2.0e18)) 4) 2 in 
     set_color c;
     fill_circle px py r

let draw_body (b:body) : unit =
  draw_centroid b.mass (fst b.pos) (snd b.pos) white

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

let run (bodies:body list) (timestep : timestep) : unit =
    open_graph "";
    resize_window dim dim;
    auto_synchronize false;     (* don't draw immediately to the screen *)
    while not (key_pressed ()) do
      clear_graph ();
      set_color black;
      fill_rect 0 0 dim dim;
      List.iter draw_body bodies;
      synchronize ();           (* show the freshly painted window *)
      step_bodies bodies timestep;
      (* When running continuously, a key press exits the program.
       * After exhuasting the frame budget, a quick press will single-step
       * and a held key will exit the program. *)
      if should_pause () then ignore (wait_next_event [Key_pressed]);
      ignore (wait_next_event [Poll]) 
    done			


(* You can run the colliding system simulation by replacing the 
 * command below with:  
 *    ;; run collision step_slow
 * But, it will be too slow.
 *)
;; run collision step_slow
 
