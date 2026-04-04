open Bigarray
open Math
open Logger
let zero : type a b. (a, b) kind -> a = function
  | Float32 -> 0.0 | Complex32 -> Complex.zero
  | Float64 -> 0.0 | Complex64 -> Complex.zero
  | Float16 -> 0.0
  | Int8_signed -> 0 | Int8_unsigned -> 0
  | Int16_signed -> 0 | Int16_unsigned -> 0
  | Int32 -> 0l | Int64 -> 0L
  | Int -> 0 | Nativeint -> 0n
  | Char -> '\000'

(*Bigarray helpers*)
let create_bigarray kind k n = Array1.create kind C_layout (k*n)

(*Returns the first int of the Bigarray returned by func*)
(*Typical use case: (Gl.genBuffers 1) returns a func that takes a Bigarray as param and stores the id of the buffer inside this bigarray*)
let get_first_int func =
  let a = create_bigarray Int32 1 1 in
  let () = func a in Int32.to_int a.{0}

(*Sets the first int in the Bigarray needed by func to the provided value and then runs func with this Bigarray*)
let set_first_int value =
  let a = create_bigarray Int32 1 1 in
  (a.{0} <- Int32.of_int value; a)

(*Set the i-th 4-dimensional vector in the bigarray a*)
let set_vector4 a i x y z w =
  let j = i * 4 in
  (a.{j} <- x; a.{j+1} <- y; a.{j+2} <- z; a.{j+3} <- w)

let bigarray_of_matrix4f ((a, b, c, d) : float matrix4) = 
  let bigarray = create_bigarray Float32 4 4 in
  set_vector4 bigarray 0 a.x a.y a.z a.w;
  set_vector4 bigarray 1 b.x b.y b.z b.w;
  set_vector4 bigarray 2 c.x c.y c.z c.w;
  set_vector4 bigarray 3 d.x d.y d.z d.w;
  bigarray

 let empty_bigarray = create_bigarray Float32 0 0

let string_of_float_bigarray ?(new_line=10) ba size =
  let rec aux i j =
    if j = new_line then "\n"^aux i 0 else
    if i = (size - 1) then string_of_float ba.{i}^" ]"
    else string_of_float ba.{i}^", "^aux (i+1) (j+1)
  in "[ "^aux 0 0

let string_of_int_bigarray ?(new_line=10) ba size =
  let rec aux i j =
    if j = new_line then "\n"^aux i 0 else
    if i = (size - 1) then string_of_int ba.{i}^" ]"
    else string_of_int ba.{i}^", "^aux (i+1) (j+1)
  in "[ "^aux 0 0
