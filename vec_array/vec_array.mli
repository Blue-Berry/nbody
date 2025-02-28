val growth_factor : int

module Float : sig
  type t
  type elm = float

  val create : int -> t
  val get : t -> int -> elm
  val grow : t -> unit
  val add_last : t -> float -> unit
  val set : t -> int -> elm -> unit
  val len : t -> int
  val clear : t -> unit
end

module Float2 : sig
  type t

  type elm =
    { v1 : float
    ; v2 : float
    }

  val create : int -> t
  val get : t -> int -> elm
  val grow : t -> unit
  val add_last : t -> elm -> unit
  val set : t -> int -> elm -> unit
  val len : t -> int
  val clear : t -> unit
end

module Float3 : sig
  type t

  type elm =
    { v1 : float
    ; v2 : float
    ; v3 : float
    }

  val create : int -> t
  val get : t -> int -> elm
  val grow : t -> unit
  val add_last : t -> elm -> unit
  val set : t -> int -> elm -> unit
  val len : t -> int
  val clear : t -> unit
end

module Float4 : sig
  type t

  type elm =
    { v1 : float
    ; v2 : float
    ; v3 : float
    ; v4 : float
    }

  val create : int -> t
  val get : t -> int -> elm
  val grow : t -> unit
  val add_last : t -> elm -> unit
  val set : t -> int -> elm -> unit
  val len : t -> int
  val clear : t -> unit
end

module Int : sig
  type t
  type elm = int

  val create : int -> t
  val get : t -> int -> elm
  val add_last : t -> elm -> unit
  val set : t -> int -> elm -> unit
  val len : t -> int
  val clear : t -> unit
end
