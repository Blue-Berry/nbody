open Owl.Maths

let ( +. ) = add
let ( -. ) = sub
let ( *. ) = mul
let ( /. ) = div

open Sexplib.Std

type point = float * float [@@deriving sexp_of]
type vec = float * float [@@deriving sexp_of]

let displace ((x, y) : point) ((dx, dy) : vec) : point = x +. dx, y +. dy
[@@inline always]
;;

let mag_squared ((x, y) : vec) : float = sqr x +. sqr y [@@inline ]
(* let mag (v: vec) : float = sqrt (mag_squared v) *)
let mag ((x, y) : vec) : float = hypot x y [@@inline ]

let ( *$ ) (c : float) ((x1, y1) : vec) : vec = c *. x1, c *. y1 [@@inline ]
let unit_vec (v : vec) : vec = 1.0 /. mag v *$ v [@@inline ]

let ( --> ) ((x1, y1) : point) ((x2, y2) : point) : vec = x2 -. x1, y2 -. y1
[@@inline ]
;;

let ( ++ ) ((x1, y1) : vec) ((x2, y2) : vec) : vec = x1 +. x2, y1 +. y2 [@@inline ]
let zero : vec = 0.0, 0.0

let close_enough (p1 : float * float) (p2 : float * float) : bool =
  mag_squared (p1 --> p2) < 0.00001
[@@inline always]
;;

let print_float (f : float) : unit = Printf.printf "%.64f" f
let g = 6.67428e-11 (* N (m/kg)^2 *)

(* TODO: close_enough check is probably expensive so there is an argument for omitting it in some cases *)
let acc_on (pos1 : point) (m2 : float) (pos2 : point) : vec =
  match close_enough pos1 pos2 with
  | true -> zero
  | false ->
    let disp12 = pos1 --> pos2 in
    (* vector representing the displacement *)
    let r2 = mag_squared disp12 in
    (* magnitude of the vector, squared *)
    let aMag = g *. m2 /. r2 in
    (* magnitude of the acceleration *)
    let aDir = unit_vec disp12 in
    (* direction of the acceleration *)
    aMag *$ aDir
;;

type timestep = float

let new_position (pos : point) (t : timestep) (v : vec) (a : vec) : point =
  displace pos ((t *$ v) ++ (0.5 *. t *. t *$ a))
;;

let new_velocity (v : vec) (a : vec) (t : timestep) : vec = v ++ (t *$ a)

type body =
  { mass : float
  ; mutable pos : point
  ; mutable vel : vec
  }

let sun () : body = { mass = 2.0e30; pos = 0.0, 0.0; vel = 0.0, 0.0 }
let mercury () : body = { mass = 3.30e23; pos = 57910000.0, 0.; vel = 9000.0, 9000.0 }
let venus () : body = { mass = 4.87e24; pos = 0.0, 108200000.0; vel = -9000.0, 0.0 }
let earth () : body = { mass = 5.98e24; pos = -149600000.0, 0.0; vel = 0.0, -9000.0 }
let mars () : body = { mass = 6.42e23; pos = 0.0, -227940000.0; vel = 600000.0, 0.0 }
let jupiter () : body = { mass = 1.90e27; pos = 778330000.0, 0.0; vel = 0.0, 450000.0 }
let saturn () : body = { mass = 5.69e26; pos = 0.0, 1426940000.0; vel = -300000.0, 0.0 }
let uranus () : body = { mass = 8.69e25; pos = -2870990000.0, 0.0; vel = 0.0, -200000.0 }

let step_body (b : body) (a : vec) (t : timestep) : unit =
  let pos' = new_position b.pos t b.vel a in
  let vel' = new_velocity b.vel a t in
  b.pos <- pos';
  b.vel <- vel'
;;

let accelerate_body (bodies : body list) (pos1 : point) : vec =
  let combine_accelerations (acc : vec) (b : body) : vec =
    acc_on pos1 b.mass b.pos ++ acc
  in
  List.fold_left combine_accelerations zero bodies
;;

let accelerations (bodies : body list) : vec list =
  let calc_acc (b : body) : vec =
    let bodies = List.filter (fun b' -> b.pos <> b'.pos) bodies in
    accelerate_body bodies b.pos
  in
  List.map calc_acc bodies
;;

let step_bodies (bodies : body list) (dt : timestep) : unit =
  let accels = accelerations bodies in
  List.iter2 (fun a b -> step_body b a dt) accels bodies
;;

let sun_mass = 2.0e30
let star m = { mass = m; pos = 0.0, 0.0; vel = 0.0, 0.0 }
let planets = [ star sun_mass; mars (); jupiter (); saturn (); uranus () ]

(* Distance to use in the 'collision' simulation *)
let dist = 4000000000.0

(* Generate a random number from a Gaussian distribution centered at d *)
let gen_dist (d : float) : float =
  let iter = 10 in
  let k = d /. float_of_int iter in
  let rec loop i acc = if i = 0 then acc else loop (i - 1) (Random.float k +. acc) in
  loop iter 0.0 -. (d /. 2.)
;;

(* calculate the velocity needed to put mass m2 into a circular orbit
   around m1 at radius r *)
let orbital_velocity m1 m2 r = sqrt (m2 *. m2 *. g /. ((m1 +. m2) *. r))

(* calculate the tangent vector to a circle centered at the origin
   at the point p *)
let tangent_vector (p : point) : vec =
  let x, y = p in
  let theta = atan2 x y in
  let vx = 0. -. cos theta in
  let vy = sin theta in
  vx, vy
;;

(* Make a list of n bodies with random masses, distributed around
   a central star. Initialize their starting velocities to put
   them into circular orbits. *)
let mk_lots (star_mass : float) (n : int) : body list =
  let _ = Random.init 17 in
  let mk_one () : body =
    let d = dist *. 2. in
    let mass = Random.float 1000.0 *. 1e13 in
    let x = gen_dist d in
    let y = gen_dist d in
    let r = mag (x, y) in
    let v = tangent_vector (x, y) in
    let velocity = orbital_velocity mass star_mass r in
    { mass; pos = x, y; vel = velocity *$ v }
  in
  let rec loop n acc = if n = 0 then acc else loop (n - 1) (mk_one () :: acc) in
  loop n [ star star_mass ]
;;

(* Displace all of the bodies in a list by a given vector *)
let displace_bodies (bodies : body list) (v : vec) : unit =
  List.iter (fun b -> b.pos <- displace b.pos v) bodies
;;

(* Add a fixed velocity vector to each of the bodies' velocities.
   (This sets an entire collection of bodies moving at a constant
   velocity.) *)
let add_velocity (bodies : body list) (v : vec) : unit =
  List.iter (fun b -> b.vel <- b.vel ++ v) bodies
;;

let collision : body list =
  let star_mass = sun_mass *. 0.1 in
  let solar1 = mk_lots sun_mass 10000 in
  let solar2 = mk_lots star_mass 3000 in
  let disp1 = dist /. 4., 0.0 in
  let disp2 = 0.0 -. (dist /. 4.), 0.0 -. (dist /. 4.) in
  let diff = disp1 --> disp2 in
  let v2 = orbital_velocity star_mass sun_mass (mag diff) *. 0.9 *$ tangent_vector diff in
  displace_bodies solar1 disp1;
  displace_bodies solar2 disp2;
  add_velocity solar2 v2;
  solar2 @ solar1
;;
