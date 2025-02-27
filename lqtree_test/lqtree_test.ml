module To_test = struct
  let subdivide = Lqtree.Qtree.subdivide_leaf
end

open Lqtree

let test_bbox = Bbox.{ minx = 0.0; miny = 0.0; maxx = 4.0; maxy = 4.0 }
let zero_c = 0.0, (0.0, 0.0)

(* Root is index 0, UL is index 1, UR is index 2, LL is index 3, LR is index 4 *)
let t_q0 =
  let open Lqtree.Quadrant in
  let open Lqtree.Node in
  let open Lqtree.Qtree in
  let ul = new_node zero_c (to_bbox UL test_bbox) in
  ul.next <- 2;
  let ur = new_node zero_c (to_bbox UR test_bbox) in
  ur.next <- 3;
  let ll = new_node (1.0, (1.5, 1.5)) (to_bbox LL test_bbox) in
  ll.next <- 4;
  let lr = new_node zero_c (to_bbox LR test_bbox) in
  lr.next <- 0;
  let t = { nodes = Dynarray.create () } in
  let root = new_node (1.0, (1.5, 1.5)) test_bbox in
  root.children <- 1;
  Dynarray.add_last t.nodes root;
  Dynarray.add_last t.nodes ul;
  Dynarray.add_last t.nodes ur;
  Dynarray.add_last t.nodes ll;
  Dynarray.add_last t.nodes lr;
  t
;;

(*
   Diagram:
               ┌──────┐              
    ┌──────────┤ Root ├──────────┐   
    │          ├──────┤          │   
    │          │      │          │   
  ┌─▼──┐    ┌──▼─┐   ┌▼───┐   ┌──▼─┐ 
  │ UL │    │ UR │   │ LL │   │ LR │ 
  └────┘    └────┘   └─┬──┘   └────┘ 
                       ▼             
                   1 (1.5;1.5)
*)

let test_divide1 () =
  let open Lqtree in
  let t = Qtree.{ nodes = Dynarray.create () } in
  let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
  Dynarray.add_last t.nodes node;
  let children = To_test.subdivide t 0 in
  Alcotest.(check int) "Children start at 1" 1 children
;;

let test_divide2 () =
  let open Lqtree in
  let t = Qtree.{ nodes = Dynarray.create () } in
  let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
  Dynarray.add_last t.nodes node;
  let _ = To_test.subdivide t 0 in
  for i = 0 to Dynarray.length t.nodes - 1 do
    let node = Dynarray.get t.nodes i in
    let t_node = Dynarray.get t_q0.nodes i in
    Alcotest.(check bool) "Nodes are the same" true (Node.equal node t_node)
  done
;;

(*Test insert:
    - Insert into empty tree
    - insert into leaf
    - insert into node*)

let () =
  let open Alcotest in
  run
    "Lqtree"
    [ ( "lqtree_test_divide_children"
      , [ test_case "Children start at 1" `Quick test_divide1 ] )
    ; "lqtree_test_divide", [ test_case "Nodes are the same" `Quick test_divide2 ]
    ]
;;
