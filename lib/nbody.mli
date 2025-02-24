type point = float * float
type vec = float * float
val ( --> ) : point -> point -> vec
val displace : point -> vec -> point
val ( ++ ) : vec -> vec -> vec
val ( *$ ) : float -> vec -> vec
val mag_squared : vec -> float
val mag : vec -> float
val unit_vec : vec -> vec
val zero : vec
val better_print_float : float -> unit
val exactly_equal : point -> point -> bool
val close_enough : point -> point -> bool
val g : float
val acc_on : point -> float -> point -> vec
type timestep = float
val new_position : point -> timestep -> vec -> vec -> point
val new_velocity : vec -> vec -> timestep -> vec
type body = { mass : float; mutable pos : point; mutable vel : vec; }
val sun : unit -> body
val mercury : unit -> body
val venus : unit -> body
val earth : unit -> body
val mars : unit -> body
val jupiter : unit -> body
val saturn : unit -> body
val uranus : unit -> body
val step_body : body -> vec -> timestep -> unit
val iter : ('a -> unit) -> 'a list -> unit
val iter_test_list : body list
val print_bodies : body list -> unit
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val accelerate_body : body list -> point -> vec
val accelerations : body list -> vec list
val step_bodies : body list -> timestep -> unit
val sun_mass : float
val star : float -> body
val planets : body list
val dist : float
val gen_dist : float -> float
val orbital_velocity : float -> float -> float -> float
val tangent_vector : point -> vec
val mk_lots : float -> int -> body list
val displace_bodies : body list -> vec -> unit
val add_velocity : body list -> vec -> unit
val collision : body list
