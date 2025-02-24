(*----------------------------------------------------------------------------*)
(*-- Higher-Order Functions --------------------------------------------------*)
(*----------------------------------------------------------------------------*)

;; open Assert

(* As always, you are free to use all the functions we've seen in lectures and
   previous homeworks in your solution to this one. *)

(*----------------------------------------------------------------------------*)
(*-- Problem 1 (Fold) --------------------------------------------------------*)
(*----------------------------------------------------------------------------*)

(* In your last homework, you learned how to use the [filter] and 
   [transform] higher-order functions to operate on lists. [fold] is 
   an even more fundamental function on lists.  It takes three 
   arguments -- a "combining function", a base value, and a list -- and 
   it calls the combining function once for each element, iteratively 
   "folding" each of the values from the list into into the base value 
   to produce the final result.  That is, given a function f, a base 
   value v, and a list [a0; a1; .. ; an], the expression

       fold f v [a0; a1; .. ; an]

   evaluates to:

       f a0 (f a1 (... (f an v))).

   For example, if we define

       let plus (x:int) (y:int) = x+y

   then

         fold plus 0 [1;2;3;4]
       = plus 1 (plus 2 (plus 3 (plus 4 0)))
       = 10
*)

let rec fold (combine: 'a -> 'b -> 'b) (base: 'b) (l: 'a list) : 'b =
  begin match l with
  | [] -> base
  | hd :: tl -> combine hd (fold combine base tl)
  end

let test () : bool =
  fold (fun (x: int list) (y: int list) -> x @ y)
      []
      [[1]; [2; 3]; [5]; [4]]
    = [1; 2; 3; 5; 4]
;; run_test
    "fold (fun (x: int list) (y: int list) -> x @ y) [] [[1]; [2; 3]; [5]; [4]]"
    test

(* For any int list m, calling (fold product_combine 1 m) should
   produce the product of all the elements in m.  (Note that we've
   written the stub definition a little differently than we usually
   write function definitions, as a "let" whose right-hand side is an
   anonymous function.) *)
let product_combine : int -> int -> int =
  fun (x: int) (y: int) ->
     failwith "unimplemented product_combine"

let test () : bool =
  360 = (fold product_combine 1 [3; 4; 5; 6])
;; run_test "fold product_combine 1 [3; 4; 5; 6]" test

(* For any int list m, calling (fold all_evens_combine true m) should
   produce the value true if and only if all elements of m are
   even. *)
let all_evens_combine : int -> bool -> bool =
  fun (x: int) (y: bool) ->
     failwith "unimplemented all_evens_combine"

let test () : bool =
  fold all_evens_combine true [0; 2; 0; -2]
;; run_test "fold all_evens_combine true [0; 2; 0; -2]" test

let test () : bool =
  fold all_evens_combine true [0; 1; 0; -2] = false
;; run_test "fold all_evens_combine true [0; 1; 0; -2]" test



(*  Recall the list_member function from HW 03. *)
let rec list_member (x:'a) (l:'a list) : bool =
  begin match l with
  | [] -> false
  | y::l -> x = y || list_member x l
  end

(* We can implement list_member using fold too.  Complete the definition 
   of member_combine so that calling (fold (member_combine x) false l) is the 
   same as (list_member x l). *)
let member_combine (x:'a) : 'a -> bool -> bool =
  failwith "unimplemented member_combine"

let test () : bool =
 fold (member_combine 1) false [3;1;2] = true
;; run_test "fold (member_combine 1) false [3;1;2]" test

let test () : bool =
 fold (member_combine "a") false ["d";"e";"f"] = false
;; run_test "fold (member_combine a) false [d;e;f]" test

let test () : bool =
  fold (member_combine 3) false [1;2;3] = true


(* Finally, let's show that the filter function can also be expressed
   nicely using fold. First, recall the definition of filter... *)
let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  begin match l with
  | [] -> []
  | x :: xs ->
      let rest = filter pred xs in
      if pred x then x :: rest else rest
  end

(* Now, write a combining function for fold that will make it perform filtering.
   To write (filter pred l) using fold, we call
   (fold (filter_combine pred) [] l). *)
let filter_combine (pred: 'a -> bool) : 'a -> 'a list -> 'a list =
  fun (x: 'a) (y: 'a list) ->
     failwith "unimplemented filter_combine"

let test () : bool =
  fold (filter_combine (fun (x: int) -> (abs x) mod 2 <> 0))
       []
       [-2; -1; 0; 1; 2]
    = [-1; 1]
;; run_test "fold 1" test

let test () : bool =
  fold (filter_combine (fun (x: int) -> (abs x) > 1)) [] [-2; -1; 0; 1; 2]
    = [-2; 2]
;; run_test "fold 2" test

(* Similarly, we can represent [transform] using [fold] nicely. Recall the 
 * definition of [transform]... *)
let rec transform (f: 'a -> 'b) (l: 'a list) : 'b list =
  begin match l with
  | [] -> []
  | x :: xs -> f x :: transform f xs
  end

(* Now, write a function [transform_combine] that, given some function [f], will
 * return a combine function that simulates calling transform with [f] on a 
 * given list. If your implementation is correct, then the following should
 * be true for any list [l] and (correctly-typed) function [f]...
 *
 * transform f l = fold (transform_combine f) [] l
 *
 * Studying the test cases we give you should help you better understand how
 * [transform_combine] should work.
 *)

let transform_combine (f: 'a -> 'b) : 'a -> 'b list -> 'b list =
  failwith "unimplemented transform_combine"

let test () : bool =
  fold (transform_combine (fun (x: int) -> x * x)) [] [-2; 1; 0; 1; 2]
    = [4; 1; 0; 1; 4]
;; run_test "fold transform_combine 1" test

let test () : bool =
  fold (transform_combine string_of_int) [] [-2; -1; 0; 1; 2]
    = ["-2"; "-1"; "0"; "1"; "2"]
;; run_test "fold transform_combine 2" test


(* Finally, write the function [fold_unzip]. This is [unzip] from HW03, 
 * but this time you _must_ use [fold]
 *
 * Recall that in [unzip] we take a list of ('a * 'b) pairs and unpack them 
 * into a single pair of a 'a list and a 'b list, where the 'a list contains 
 * all of the first elements of the pairs and the 'b list contains all of 
 * the second elements of the pairs
 *
 * e.g. fold_unzip [("a", 1); ("b", 2)] = (["a"; "b"]; [1; 2])
 *
 * You may find the [fst] and [snd] functions - which each take a 
 * 2-tuple and return the first or second element, respectively - 
 * useful here.
 *)
let fold_unzip (l: ('a * 'b) list) : 'a list * 'b list =
  failwith "Missing implementation for unzip_combine"

let test () : bool =
  fold_unzip [("a", 1); ("b", 2)] = (["a"; "b"], [1; 2]) 
;; run_test "fold_unzip [(a, 1); (b, 2)]" test

(*----------------------------------------------------------------------------*)
(*-- Extra challenge problems (more fun with higher-order functions) ---------*)
(*----------------------------------------------------------------------------*)

(* Remember the different set implementations from last week?
   Here is one more! *)

type 'a set = 'a -> bool

let mem (x: 'a) (s: 'a set) : bool =
  s x

(* For instance, the set {3,4} could be represented by
     fun (x:int) -> if x=3 then true else (if x=4 then true else false)
   or even (hint!) by
     fun (x:int) -> if x=3
                      then true
                      else (fun (x:int) -> if x=4 then true else false) x
 *)

(* Implement empty, add, and remove using this representation: *)
let empty : 'a set =
  fun (x: 'a) -> failwith "unimplemented empty"

let add (x: 'a) (s: 'a set) : 'a set =
  failwith "unimplemented add"

let remove (x:'a) (s: 'a set) : 'a set =
  failwith "unimplemented remove"

let test () : bool =
  false = mem 3 empty
;; run_test "mem 3 empty" test

let test () : bool =
  true = mem 3 (add 3 empty)
;; run_test "mem 3 (add 3 empty)" test

let test () : bool =
  false = mem 3 (remove 3 (add 3 empty))
;; run_test "mem 3 (remove 3 (add 3 empty))" test

(* Unlike last week, we do not ask you to implement equals, is_empty,
   or elements. Why not?  *)

(* --------------------------------------------------------------------- *)
(* After you finish the problems in this file, go back and read the
   next part of the HTML instructions. *)
