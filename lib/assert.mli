val run_test: string -> (unit -> bool) -> unit
val run_failing_test: string -> (unit -> bool) -> unit
val stop_on_failure: unit -> unit

val stop_on_failure_flag : bool ref
val error_mesg : string -> unit

type result = Succ | Fail | Err of string

val assert_eqf : string -> (unit -> 'a) -> 'a -> unit
val assert_eq : string -> 'a -> 'a -> unit

