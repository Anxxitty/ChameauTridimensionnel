open Bigarray
let zero : type a b. (a, b) kind -> a = function
  | Float32 -> 0.0 | Complex32 -> Complex.zero
  | Float64 -> 0.0 | Complex64 -> Complex.zero
  | Float16 -> 0.0
  | Int8_signed -> 0 | Int8_unsigned -> 0
  | Int16_signed -> 0 | Int16_unsigned -> 0
  | Int32 -> 0l | Int64 -> 0L
  | Int -> 0 | Nativeint -> 0n
  | Char -> '\000'

type 'a vector1 = { x : 'a }
type 'a vector2 = { x : 'a ; y : 'a }
type 'a vector3 = { x : 'a ; y : 'a ; z : 'a }
type 'a vector4 = { r : 'a ; g : 'a ; b : 'a ; a : 'a }

type 'a vector = Vector1 of 'a vector1
                     | Vector2 of 'a vector2
                     | Vector3 of 'a vector3
                     | Vector4 of 'a vector4

type ('a, 'b) vectorKind = Vector1Kind of ('a, 'b) kind
                         | Vector2Kind of ('a, 'b) kind
                         | Vector3Kind of ('a, 'b) kind
                         | Vector4Kind of ('a, 'b) kind

type 'a matrix4 = 'a vector4 * 'a vector4 * 'a vector4 * 'a vector4

type ('a, 'b) matrix = Matrix4 of (('a, 'b) kind * 'a matrix4)

type ('a, 'b) matrixKind = Matrix4Kind of ('a, 'b) kind

let (+:) (vector1 : int vector) (vector2 : int vector) = match vector1 with
  | Vector1 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector1 { x = vec1.x + vec2.x }
    | Vector2 vec2 -> Vector2 { x = vec1.x + vec2.x ; y = vec2.y }
    | Vector3 vec2 -> Vector3 { x = vec1.x + vec2.x ; y = vec2.y ; z = vec2.z }
    | Vector4 vec2 -> Vector4 { r = vec1.x + vec2.r ; g = vec2.g ; b = vec2.b ; a = vec2.a })
  | Vector2 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector2 { x = vec1.x + vec2.x ; y = vec1.y }
    | Vector2 vec2 -> Vector2 { x = vec1.x + vec2.x ; y = vec1.y + vec2.y }
    | Vector3 vec2 -> Vector3 { x = vec1.x + vec2.x ; y = vec1.y + vec2.y ; z = vec2.z }
    | Vector4 vec2 -> Vector4 { r = vec1.x + vec2.r ; g = vec1.y + vec2.g ; b = vec2.b ; a = vec2.a })
  | Vector3 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector3 { x = vec1.x + vec2.x ; y = vec1.y ; z = vec1.z }
    | Vector2 vec2 -> Vector3 { x = vec1.x + vec2.x ; y = vec1.y + vec2.y ; z = vec1.z }
    | Vector3 vec2 -> Vector3 { x = vec1.x + vec2.x ; y = vec1.y + vec2.y ; z = vec1.z + vec2.z }
    | Vector4 vec2 -> Vector4 { r = vec1.x + vec2.r ; g = vec1.y + vec2.g ; b = vec1.z + vec2.b ; a = vec2.a })
  | Vector4 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector4 { r = vec1.r + vec2.x ; g = vec1.g ; b = vec1.b ; a = vec1.a }
    | Vector2 vec2 -> Vector4 { r = vec1.r + vec2.x ; g = vec1.g + vec2.y ; b = vec1.b; a = vec1.a }
    | Vector3 vec2 -> Vector4 { r = vec1.r + vec2.x ; g = vec1.g + vec2.y ; b = vec1.b + vec2.z ; a = vec1.a }
    | Vector4 vec2 -> Vector4 { r = vec1.r + vec2.r ; g = vec1.g + vec2.g ; b = vec1.b + vec2.b ; a = vec1.a + vec2.a })

let (+:.) (vector1 : float vector) (vector2 : float vector) = match vector1 with
  | Vector1 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector1 { x = vec1.x +. vec2.x }
    | Vector2 vec2 -> Vector2 { x = vec1.x +. vec2.x ; y = vec2.y }
    | Vector3 vec2 -> Vector3 { x = vec1.x +. vec2.x ; y = vec2.y ; z = vec2.z }
    | Vector4 vec2 -> Vector4 { r = vec1.x +. vec2.r ; g = vec2.g ; b = vec2.b ; a = vec2.a })
  | Vector2 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector2 { x = vec1.x +. vec2.x ; y = vec1.y }
    | Vector2 vec2 -> Vector2 { x = vec1.x +. vec2.x ; y = vec1.y +. vec2.y }
    | Vector3 vec2 -> Vector3 { x = vec1.x +. vec2.x ; y = vec1.y +. vec2.y ; z = vec2.z }
    | Vector4 vec2 -> Vector4 { r = vec1.x +. vec2.r ; g = vec1.y +. vec2.g ; b = vec2.b ; a = vec2.a })
  | Vector3 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector3 { x = vec1.x +. vec2.x ; y = vec1.y ; z = vec1.z }
    | Vector2 vec2 -> Vector3 { x = vec1.x +. vec2.x ; y = vec1.y +. vec2.y ; z = vec1.z }
    | Vector3 vec2 -> Vector3 { x = vec1.x +. vec2.x ; y = vec1.y +. vec2.y ; z = vec1.z +. vec2.z }
    | Vector4 vec2 -> Vector4 { r = vec1.x +. vec2.r ; g = vec1.y +. vec2.g ; b = vec1.z +. vec2.b ; a = vec2.a })
  | Vector4 vec1 -> (match vector2 with
    | Vector1 vec2 -> Vector4 { r = vec1.r +. vec2.x ; g = vec1.g ; b = vec1.b ; a = vec1.a }
    | Vector2 vec2 -> Vector4 { r = vec1.r +. vec2.x ; g = vec1.g +. vec2.y ; b = vec1.b; a = vec1.a }
    | Vector3 vec2 -> Vector4 { r = vec1.r +. vec2.x ; g = vec1.g +. vec2.y ; b = vec1.b +. vec2.z ; a = vec1.a }
    | Vector4 vec2 -> Vector4 { r = vec1.r +. vec2.r ; g = vec1.g +. vec2.g ; b = vec1.b +. vec2.b ; a = vec1.a +. vec2.a })

let createBigarray kind k n = Array1.create kind C_layout (k*n)

(*Returns the first int of the Bigarray returned by func*)
(*Typical use case: (Gl.genBuffers 1) returns a func that takes a Bigarray as param and stores the id of the buffer inside this bigarray*)
let getFirstInt func =
  let a = createBigarray Int32 1 1 in
  let () = func a in Int32.to_int a.{0}

(*Sets the first int in the Bigarray needed by func to the provided value and then runs func with this Bigarray*)
let setFirstInt value =
  let a = createBigarray Int32 1 1 in
  (a.{0} <- Int32.of_int value; a)

(*Get the i-th 3-dimensional vector in the bigarray a*)
let getVector3 a i = let j = 3 * i in a.{j}, a.{j+1}, a.{j+2}

(*Get the i-th 4-dimensional vector in the bigarray a*)
let getVector4 a i = let j = 4 * i in a.{j}, a.{j+1}, a.{j+2}, a.{j+3}

(*Set the i-th 3-dimensional vector in the bigarray a*)
let setVector3 a i x y z =
  let j = i * 3 in
  (a.{j} <- x; a.{j+1} <- y; a.{j+2} <- z)

(*Set the i-th 4-dimensional vector in the bigarray a*)
let setVector4 a i r g b alpha =
  let j = i * 4 in
  (a.{j} <- r; a.{j+1} <- g; a.{j+2} <- b; a.{j+3} <- alpha)

let setVector7 a i x y z r g b alpha =
  let j = i * 7 in
  (a.{j} <- x; a.{j+1} <- y; a.{j+2} <- z; a.{j+3} <- r; a.{j+4} <- g; a.{j+5} <- b; a.{j+6} <- alpha)

let bigarrayOfMatrix4f ((a, b, c, d) : float matrix4) = 
  let bigarray = createBigarray Float32 4 4 in
  setVector4 bigarray 0 a.r a.g a.b a.a;
  setVector4 bigarray 1 b.r b.g b.b b.a;
  setVector4 bigarray 2 c.r c.g c.b c.a;
  setVector4 bigarray 3 d.r d.g d.b d.a;
  bigarray

