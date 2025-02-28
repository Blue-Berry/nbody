open Bigarray

let growth_factor = 2

module Float = struct
  type t =
    { mutable arr : (float, float64_elt, c_layout) Array1.t
    ; mutable length : int
    }

  type elm = float

  let create (cap : int) : t =
    let arr = Array1.create Bigarray.float64 Bigarray.c_layout cap in
    { arr; length = 0 }
  ;;

  let get (v : t) (i : int) : elm = Array1.get v.arr i

  let grow (v : t) : unit =
    let new_len = v.length * growth_factor in
    let new_arr = Array1.create float64 c_layout new_len in
    Array1.blit v.arr new_arr;
    v.arr <- new_arr
  ;;

  let add_last (v : t) (x : float) : unit =
    if v.length >= Array1.dim v.arr then grow v;
    Array1.unsafe_set v.arr v.length x;
    v.length <- v.length + 1
  ;;

  let set (v : t) (i : int) (x : float) : unit = Array1.set v.arr i x
end

module Float2 = struct
  type t =
    { mutable arr : (float, float64_elt, c_layout) Array2.t
    ; mutable length : int
    }

  type elm =
    { v1 : float
    ; v2 : float
    }

  let dim2 = 2

  let create (cap : int) : t =
    let arr = Array2.create float64 c_layout dim2 cap in
    { arr; length = 0 }
  ;;

  let get v i = Array2.get v.arr 0 i, Array2.get v.arr 1 i

  let grow v =
    let new_len = v.length * growth_factor in
    let new_arr = Array2.create float64 c_layout dim2 new_len in
    Array2.blit v.arr new_arr;
    v.arr <- new_arr
  ;;

  let add_last v (x, y) =
    if v.length >= Array2.dim2 v.arr then grow v;
    Array2.unsafe_set v.arr 0 v.length x;
    Array2.unsafe_set v.arr 1 v.length y;
    v.length <- v.length + 1
  ;;

  let set v i { v1; v2 } =
    Array2.set v.arr 0 i v1;
    Array2.set v.arr 1 i v2
  ;;
end

module Float3 = struct
  type t =
    { mutable arr : (float, float64_elt, c_layout) Array2.t
    ; mutable length : int
    }

  let dim2 = 3

  type elm =
    { v1 : float
    ; v2 : float
    ; v3 : float
    }

  let create (cap : int) : t =
    let arr = Array2.create float64 c_layout dim2 cap in
    { arr; length = 0 }
  ;;

  let get v i = Array2.get v.arr 0 i, Array2.get v.arr 1 i, Array2.get v.arr 2 i

  let grow v =
    let new_len = v.length * growth_factor in
    let new_arr = Array2.create float64 c_layout dim2 new_len in
    Array2.blit v.arr new_arr;
    v.arr <- new_arr
  ;;

  let add_last v (x, y, z) =
    if v.length >= Array2.dim2 v.arr then grow v;
    Array2.unsafe_set v.arr 0 v.length x;
    Array2.unsafe_set v.arr 1 v.length y;
    Array2.unsafe_set v.arr 2 v.length z;
    v.length <- v.length + 1
  ;;

  let set v i { v1; v2; v3 } =
    Array2.set v.arr 0 i v1;
    Array2.set v.arr 1 i v2;
    Array2.set v.arr 2 i v3
  ;;
end

module Float4 = struct
  type t =
    { mutable arr : (float, float64_elt, c_layout) Array2.t
    ; mutable length : int
    }

  let dim2 = 4

  type elm =
    { v1 : float
    ; v2 : float
    ; v3 : float
    ; v4 : float
    }

  let create (cap : int) : t =
    let arr = Array2.create float64 c_layout dim2 cap in
    { arr; length = 0 }
  ;;

  let get v i =
    let v1 = Array2.get v.arr 0 i in
    let v2 = Array2.get v.arr 1 i in
    let v3 = Array2.get v.arr 2 i in
    let v4 = Array2.get v.arr 3 i in
    { v1; v2; v3; v4 }
  ;;

  let grow v =
    let new_len = v.length * growth_factor in
    let new_arr = Array2.create float64 c_layout dim2 new_len in
    Array2.blit v.arr new_arr;
    v.arr <- new_arr
  ;;

  let add_last v { v1; v2; v3; v4 } =
    if v.length >= Array2.dim2 v.arr then grow v;
    Array2.unsafe_set v.arr 0 v.length v1;
    Array2.unsafe_set v.arr 1 v.length v2;
    Array2.unsafe_set v.arr 2 v.length v3;
    Array2.unsafe_set v.arr 2 v.length v4;
    v.length <- v.length + 1
  ;;

  let set v i { v1; v2; v3; v4 } =
    Array2.set v.arr 0 i v1;
    Array2.set v.arr 1 i v2;
    Array2.set v.arr 2 i v3;
    Array2.set v.arr 2 i v4
  ;;
end
