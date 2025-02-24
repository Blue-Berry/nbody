(* Given HOF Implementations *)
val fold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val transform : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list

(* Fold Problems *)
val product_combine : int -> int -> int
val all_evens_combine : int -> bool -> bool
val member_combine : 'a -> 'a -> bool -> bool
val filter_combine : ('a -> bool) -> 'a -> 'a list -> 'a list
val transform_combine : ('a -> 'b) -> 'a -> 'b list -> 'b list
val fold_unzip : ('a * 'b) list -> 'a list * 'b list

(* Kudos *)
type 'a set = 'a -> bool
val mem : 'a -> 'a set -> bool
val empty : 'a set
val add : 'a -> 'a set -> 'a set
val remove : 'a -> 'a set -> 'a set

val list_member : 'a -> 'a list -> bool

