module To_test = struct
  let subdivide = Lqtree.Qtree.subdivide_leaf
  let insert = Lqtree.Qtree.insert
  let acc_by_qtree = Lqtree.Qtree.acc_by_qtree
end

open Lqtree

let test_bbox = Bbox.{ minx = 0.0; miny = 0.0; maxx = 4.0; maxy = 4.0 }
let zero_c = 0.0, (0.0, 0.0)

(* Root is index 0, UL is index 1, UR is index 2, LL is index 3, LR is index 4 *)
let t_q0 =
  let open Quadrant in
  let open Node in
  let open Qtree in
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
  let qt = Qtree.{ nodes = Dynarray.create () } in
  let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
  Dynarray.add_last qt.nodes node;
  let children = To_test.subdivide qt 0 in
  Alcotest.(check int) "Children start at 1" 1 children
;;

let test_divide2 () =
  let open Lqtree in
  let qt = Qtree.{ nodes = Dynarray.create () } in
  let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
  Dynarray.add_last qt.nodes node;
  let _ = To_test.subdivide qt 0 in
  for i = 0 to Dynarray.length qt.nodes - 1 do
    let node = Dynarray.get qt.nodes i in
    let t_node = Dynarray.get t_q0.nodes i in
    Alcotest.(check bool) "Nodes are the same" true (Node.equal node t_node)
  done
;;

(*Test insert:
    - Insert into empty tree
    - insert into leaf
    - insert into node*)

(*
   Insert into empty tree:
    ┌──────┐   
    │ Root │   
    └──┬───┘   
       │       
       ▼       
   1 (1.5;1.5)
*)
let test_insert1 () =
  let open Lqtree in
  let qt_test =
    let qt = Qtree.{ nodes = Dynarray.create () } in
    let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
    Dynarray.add_last qt.nodes node;
    qt
  in
  let qt = Qtree.new_t test_bbox in
  To_test.insert qt (1.0, (1.5, 1.5));
  for i = 0 to Dynarray.length qt.nodes - 1 do
    let node = Dynarray.get qt.nodes i in
    let t_node = Dynarray.get qt_test.nodes i in
    Alcotest.(check bool) "Nodes are the same" true (Node.equal node t_node)
  done
;;

(*
   Insert into leaf:
    ┌──────┐                         ┌──────┐             
    │ Root │              ┌──────────┤ Root ├──────────┐  
    └──┬───┘ ─────────►   │          ├──────┤          │  
       │     1 (2.5;2.5)  │          │      │          │  
       ▼                ┌─▼──┐    ┌──▼─┐   ┌▼───┐   ┌──▼─┐
   1 (1.5;1.5)          │ UL │    │ UR │   │ LL │   │ LR │
                        └────┘    ├────┘   └─┬──┘   └────┘
                                  ▼          ▼            
                           1 (2.5;2.5)   1 (1.5;1.5)
*)

let test_insert2 () =
  let open Lqtree in
  let qt_test =
    let open Quadrant in
    let open Node in
    let open Qtree in
    let ul = new_node zero_c (to_bbox UL test_bbox) in
    ul.next <- 2;
    let ur = new_node (1.0, (2.5, 2.5)) (to_bbox UR test_bbox) in
    ur.next <- 3;
    let ll = new_node (1.0, (1.5, 1.5)) (to_bbox LL test_bbox) in
    ll.next <- 4;
    let lr = new_node zero_c (to_bbox LR test_bbox) in
    lr.next <- 0;
    let t = { nodes = Dynarray.create () } in
    let root = new_node (centroid_sum (1.0, (1.5, 1.5)) (1.0, (2.5, 2.5))) test_bbox in
    root.children <- 1;
    Dynarray.add_last t.nodes root;
    Dynarray.add_last t.nodes ul;
    Dynarray.add_last t.nodes ur;
    Dynarray.add_last t.nodes ll;
    Dynarray.add_last t.nodes lr;
    t
  in
  let leaf =
    let qt = Qtree.{ nodes = Dynarray.create () } in
    let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
    Dynarray.add_last qt.nodes node;
    qt
  in
  To_test.insert leaf (1.0, (2.5, 2.5));
  for i = 0 to Dynarray.length qt_test.nodes - 1 do
    let example_node = Dynarray.get qt_test.nodes i in
    let node = Dynarray.get leaf.nodes i in
    Alcotest.(check bool)
      ("Node " ^ string_of_int i ^ " are the same")
      true
      (Node.equal node example_node)
  done
;;

(*
   Insert into Node:
              ┌──────┐                      
              │ Root │                      
   ┌──────────┤ 2 2;2├──────────┐           
   │          ├──────┤          │           
   │          │      │          │           
 ┌─▼──┐    ┌──▼─┐   ┌▼───┐   ┌──▼─┐         
 │ UL │    │ UR │   │ LL │   │ LR │         
 └────┘    ├────┘   └─┬──┘   └────┘         
           ▼          ▼                     
    1 (2.5;2.5)   1 (1.5;1.5)               
                  │                         
                  │                         
                  │                         
                  │   1 (2.5;1.5)           
                  │                         
                  │                         
                  │                         
                  ▼                         
              ┌──────┐                      
              │ Root │                      
   ┌──────────┤ 3 2;2├──────────┐           
   │          ├──────┤          │           
   │          │      │          │           
 ┌─▼──┐    ┌──▼─┐   ┌▼───┐   ┌──▼─┐         
 │ UL │    │ UR │   │ LL │   │ LR │         
 └────┘    ├────┘   └─┬──┘   └────┘         
           ▼          ▼                     
    1 (2.5;2.5)   1 (1.5;1.5)   1 (2.5;1.5)
*)

let test_insert3 () =
  let open Lqtree in
  let qt_test =
    let open Quadrant in
    let open Node in
    let open Qtree in
    let ul = new_node zero_c (to_bbox UL test_bbox) in
    ul.next <- 2;
    let ur = new_node (1.0, (2.5, 2.5)) (to_bbox UR test_bbox) in
    ur.next <- 3;
    let ll = new_node (1.0, (1.5, 1.5)) (to_bbox LL test_bbox) in
    ll.next <- 4;
    let lr = new_node (1.0, (2.5, 1.5)) (to_bbox LR test_bbox) in
    lr.next <- 0;
    let t = { nodes = Dynarray.create () } in
    let root =
      new_node
        (centroid_sum
           (centroid_sum (1.0, (1.5, 1.5)) (1.0, (2.5, 2.5)))
           (1.0, (2.5, 1.5)))
        test_bbox
    in
    root.children <- 1;
    Dynarray.add_last t.nodes root;
    Dynarray.add_last t.nodes ul;
    Dynarray.add_last t.nodes ur;
    Dynarray.add_last t.nodes ll;
    Dynarray.add_last t.nodes lr;
    t
  in
  let qt_node =
    let qt = Qtree.{ nodes = Dynarray.create () } in
    let node = Node.new_node (1.0, (1.5, 1.5)) test_bbox in
    Dynarray.add_last qt.nodes node;
    qt
  in
  To_test.insert qt_node (1.0, (2.5, 2.5));
  To_test.insert qt_node (1.0, (2.5, 1.5));
  for i = 0 to Dynarray.length qt_test.nodes - 1 do
    let example_node = Dynarray.get qt_test.nodes i in
    let node = Dynarray.get qt_node.nodes i in
    Alcotest.(check bool)
      ("Node " ^ string_of_int i ^ " are the same")
      true
      (Node.equal node example_node)
  done
;;

(* Acceleration due to quadtrees *)

let test_accel_zero () =
  let qt_test =
    let qt = Qtree.new_t test_bbox in
    qt
  in
  let accel = To_test.acc_by_qtree (1.0, 1.0) qt_test 0.0 in
  Alcotest.(check bool) "Acceleration by empty tree" true (accel = (0.0, 0.0))
;;

let test_accel1 () =
  let qt_test = Qtree.new_t test_bbox in
  To_test.insert qt_test (10000000.0, (2.0, 3.0));
  Printf.printf "Tree: %s\n" (qt_test |> Qtree.sexp_of_t |> Sexplib.Sexp.to_string_hum);
  let accel = To_test.acc_by_qtree (2.0, 2.0) qt_test 0.0 in
  Alcotest.(check bool) "Acceleration by leaf" true (accel = (0.0, 0.000667428))
;;

let qtree =
  let open Quadrant in
  let open Node in
  let open Qtree in
  let ul = new_node zero_c (to_bbox UL test_bbox) in
  ul.next <- 2;
  let ur = new_node zero_c (to_bbox UR test_bbox) in
  ur.next <- 3;
  let ll = new_node (12345678.0, (2.0, 2.0)) (to_bbox LL test_bbox) in
  ll.next <- 4;
  let lr = new_node zero_c (to_bbox LR test_bbox) in
  lr.next <- 0;
  let t = { nodes = Dynarray.create () } in
  let root = new_node (12345678.0, (3.0, 3.0)) test_bbox in
  root.children <- 1;
  Dynarray.add_last t.nodes root;
  Dynarray.add_last t.nodes ul;
  Dynarray.add_last t.nodes ur;
  Dynarray.add_last t.nodes ll;
  Dynarray.add_last t.nodes lr;
  t
;;

let test_accel2 () =
  let acc = To_test.acc_by_qtree (5.0, 2.0) qtree 2.236 in
  let acc_test = -0.00014739893883543214, 7.36994694177160698e-05 in
  Alcotest.(check bool) "Acceleration by Node" true (close_enough acc acc_test)
;;

let test_accel3 () =
  let acc = To_test.acc_by_qtree (5.0, 2.0) qtree 0.0 in
  let acc_test = -0.000823985117618399889, 0. in
  Alcotest.(check bool) "Acceleration by Node" true (close_enough acc acc_test)
;;

let () =
  let open Alcotest in
  run
    "Lqtree"
    [ ( "subdivide"
      , [ test_case "Children start at 1" `Quick test_divide1
        ; test_case "Nodes are the same" `Quick test_divide2
        ] )
    ; ( "insert"
      , [ test_case "Insert into empty" `Quick test_insert1
        ; test_case "Insert into leaf" `Quick test_insert2
        ; test_case "Insert into node" `Quick test_insert3
        ] )
    ; ( "Acceleration by quadtree"
      , [ test_case "Acceleration by empty tree" `Quick test_accel_zero
        ; test_case "Acceleration by leaf" `Quick test_accel1
        ; test_case "Acceleration by node outside threshold" `Quick test_accel2
        ; test_case "Acceleration by node inside threshold" `Quick test_accel3
        ] )
    ]
;;
