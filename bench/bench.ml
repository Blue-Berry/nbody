open Core_bench

let bench_linear () =
  let open Lqtree in
  let open Nbody in
  let lbb =
    Lqtree.Bbox.
      { minx = dist *. -500.
      ; maxx = dist *. 500.
      ; miny = dist *. -500.
      ; maxy = dist *. 500.
      }
  in
  let step_lq (qt : Lqtree.Qtree.t) : body -> unit =
    let open Lqtree in
    let open Qtree in
    let step_body_with_acc (qt : Qtree.t) (thresh : float) (dt : float) (b : body) : unit =
      step_body b (acc_by_qtree b.pos qt thresh) dt
    in
    let dt = 50.0 in
    let thresh = 1000000.0 in
    step_body_with_acc qt thresh dt
  in
  let lqtree_bodies = Nbody.collision in
  let lqt = Lqtree.Qtree.new_t lbb in
  Lqtree.Qtree.populate lqt lqtree_bodies;
  List.iter (step_lq lqt) lqtree_bodies
;;

let bench_quad () =
  let open Nbody in
  let open Qtree in
  let bb =
    Qtree.Bbox.
      { minx = dist *. -500.
      ; maxx = dist *. 500.
      ; miny = dist *. -500.
      ; maxy = dist *. 500.
      }
  in
  let step_body_with_acc (qt : qtree) (thresh : float) (dt : float) (b : body) : unit =
    step_body b (acc_by_qtree b.pos qt bb thresh) dt
  in
  let step_slow (qt : qtree) : body -> unit =
    let dt = 50.0 in
    let thresh = 1000000.0 in
    step_body_with_acc qt thresh dt
  in
  let qtree_bodies = Nbody.collision in
  let qt = Qtree.build_qtree_in qtree_bodies bb in
  List.iter (step_slow qt) qtree_bodies
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Linear" bench_linear
       ; Bench.Test.create ~name:"Quadtree" bench_quad
       ])
;;
