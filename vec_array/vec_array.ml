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
    Array1.blit v.arr (Array1.sub new_arr 0 (Array1.dim v.arr));
    v.arr <- new_arr
  ;;

  let add_last (v : t) (x : float) : unit =
    if v.length >= Array1.dim v.arr then grow v;
    Array1.set v.arr v.length x;
    v.length <- v.length + 1
  ;;

  let set (v : t) (i : int) (x : float) : unit = Array1.set v.arr i x
  let len (v : t) : int = v.length
  let clear (v : t) : unit = v.length <- 0
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
    let arr = Array2.create float64 c_layout cap dim2 in
    { arr; length = 0 }
  ;;

  let get v i =
    let v1 = Array2.get v.arr i 0 in
    let v2 = Array2.get v.arr i 1 in
    { v1; v2 }
  ;;

  let grow v =
    let new_len = v.length * growth_factor in
    let new_arr = Array2.create float64 c_layout new_len dim2 in
    Array2.blit v.arr (Array2.sub_left new_arr 0 (Array2.dim1 v.arr));
    v.arr <- new_arr
  ;;

  let add_last v { v1; v2 } =
    if v.length >= Array2.dim2 v.arr then grow v;
    Array2.set v.arr v.length 0 v1;
    Array2.set v.arr v.length 1 v2;
    v.length <- v.length + 1
  ;;

  let set v i { v1; v2 } =
    Array2.set v.arr i 0 v1;
    Array2.set v.arr i 1 v2
  ;;

  let len (v : t) : int = v.length
  let clear (v : t) : unit = v.length <- 0
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
    let arr = Array2.create float64 c_layout cap dim2 in
    { arr; length = 0 }
  ;;

  let get v i : elm =
    let v1 = Array2.get v.arr i 0 in
    let v2 = Array2.get v.arr i 1 in
    let v3 = Array2.get v.arr i 2 in
    { v1; v2; v3 }
  ;;

  let grow v =
    let new_len = v.length * growth_factor in
    let new_arr = Array2.create float64 c_layout new_len dim2 in
    Array2.blit v.arr (Array2.sub_left new_arr 0 (Array2.dim1 v.arr));
    v.arr <- new_arr
  ;;

  let add_last v { v1; v2; v3 } =
    if v.length >= Array2.dim2 v.arr then grow v;
    Array2.set v.arr v.length 0 v1;
    Array2.set v.arr v.length 1 v2;
    Array2.set v.arr v.length 2 v3;
    v.length <- v.length + 1
  ;;

  let set v i { v1; v2; v3 } =
    Array2.set v.arr i 0 v1;
    Array2.set v.arr i 1 v2;
    Array2.set v.arr i 2 v3
  ;;

  let len (v : t) : int = v.length
  let clear (v : t) : unit = v.length <- 0
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
    let arr = Array2.create float64 c_layout cap dim2 in
    { arr; length = 0 }
  ;;

  let get v i =
    let v1 = Array2.get v.arr i 0 in
    let v2 = Array2.get v.arr i 1 in
    let v3 = Array2.get v.arr i 2 in
    let v4 = Array2.get v.arr i 3 in
    { v1; v2; v3; v4 }
  ;;

  (* Copy all elements of a Bigarray in another Bigarray. Genarray.blit src dst copies all elements of src into dst. Both arrays src and dst must have the same number of dimensions and equal dimensions. Copying a sub-array of src to a sub-array of dst can be achieved by applying Genarray.blit to sub-array or slices of src and dst. *)
  let grow v =
    let new_len = v.length * growth_factor in
    let new_arr = Array2.create float64 c_layout new_len dim2 in
    Array2.blit v.arr (Array2.sub_left new_arr 0 (Array2.dim1 v.arr));
    v.arr <- new_arr
  ;;

  let add_last v { v1; v2; v3; v4 } =
    if v.length >= Array2.dim2 v.arr then grow v;
    Array2.set v.arr v.length 0 v1;
    Array2.set v.arr v.length 1 v2;
    Array2.set v.arr v.length 2 v3;
    Array2.set v.arr v.length 2 v4;
    v.length <- v.length + 1
  ;;

  let set v i { v1; v2; v3; v4 } =
    Array2.set v.arr i 0 v1;
    Array2.set v.arr i 1 v2;
    Array2.set v.arr i 2 v3;
    Array2.set v.arr i 2 v4
  ;;

  let len (v : t) : int = v.length
  let clear (v : t) : unit = v.length <- 0
end

module Int = struct
  (* NOTE: something to be said for using int32 *)
  type t =
    { mutable arr : (int, int_elt, c_layout) Array1.t
    ; mutable length : int
    }

  type elm = int

  let create (cap : int) : t =
    let arr = Array1.create int Bigarray.c_layout cap in
    { arr; length = 0 }
  ;;

  let get (v : t) (i : int) : elm = Array1.get v.arr i

  let grow (v : t) : unit =
    let new_len = v.length * growth_factor in
    let new_arr = Array1.create int c_layout new_len in
    Array1.blit v.arr (Array1.sub new_arr 0 (Array1.dim v.arr));
    v.arr <- new_arr
  ;;

  let add_last (v : t) (x : elm) : unit =
    if v.length >= Array1.dim v.arr then grow v;
    Array1.set v.arr v.length x;
    v.length <- v.length + 1
  ;;

  let set (v : t) (i : int) (x : elm) : unit = Array1.set v.arr i x
  let len (v : t) : int = v.length
  let clear (v : t) : unit = v.length <- 0
end
