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

(*Vector datatype*)
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

(*Matrix datatype*)
type 'a matrix4 = 'a vector4 * 'a vector4 * 'a vector4 * 'a vector4

type 'a matrix = Matrix4 of 'a matrix4

type ('a, 'b) matrixKind = Matrix4Kind of ('a, 'b) kind

let pi = 3.141592653589793115997963468544185161590576171875

let radiansOfDeg angle =
  angle*.pi/.180.

let degOfRadians angle =
  angle*.180./.pi

(*dot product*)
let dot vec1 vec2 = vec1.x*.vec2.x +. vec1.y*.vec2.y +. vec1.z*.vec2.z

(*cross product*)
let cross vec1 vec2 = {x=vec1.y*.vec2.z-.vec1.z*.vec2.y;y=vec1.z*.vec2.x-.vec1.x*.vec2.z;z=vec1.x*.vec2.y-.vec1.y*.vec2.x}

(*normalizes the vector*)
let normalize3f vec =
  let length = sqrt (vec.x**2. +. vec.y**2. +. vec.z**2.) in
  {x=vec.x/.length; y=vec.y/.length; z=vec.z/.length}

(*transposes a matrix*)
let transpose (a, b, c, d) = (
  {r=a.r; g=b.r; b=c.r; a=d.r},
  {r=a.g; g=b.g; b=c.g; a=d.g},
  {r=a.b; g=b.b; b=c.b; a=d.b},
  {r=a.a; g=b.a; b=c.a; a=d.a}
)

let normalize4f vec =
  let length = sqrt (vec.r**2. +. vec.g**2. +. vec.b**2. +. vec.a**2.) in
  {r=vec.r/.length; g=vec.g/.length; b=vec.b/.length; a=vec.a/.length}

(*Add operator for vectors*)
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

(*Multiply matrix operator*)
let multiplyMatrices4f ((a1, b1, c1, d1) : float matrix4) ((a2, b2, c2, d2) : float matrix4) =
  let ar = (a1.r *. a2.r) +. (a1.g *. b2.r) +. (a1.b *. c2.r) +. (a1.a *. d2.r) in
  let ag = (a1.r *. a2.g) +. (a1.g *. b2.g) +. (a1.b *. c2.g) +. (a1.a *. d2.g) in
  let ab = (a1.r *. a2.b) +. (a1.g *. b2.b) +. (a1.b *. c2.b) +. (a1.a *. d2.b) in
  let aa = (a1.r *. a2.a) +. (a1.g *. b2.a) +. (a1.b *. c2.a) +. (a1.a *. d2.a) in
  let br = (b1.r *. a2.r) +. (b1.g *. b2.r) +. (b1.b *. c2.r) +. (b1.a *. d2.r) in
  let bg = (b1.r *. a2.g) +. (b1.g *. b2.g) +. (b1.b *. c2.g) +. (b1.a *. d2.g) in
  let bb = (b1.r *. a2.b) +. (b1.g *. b2.b) +. (b1.b *. c2.b) +. (b1.a *. d2.b) in
  let ba = (b1.r *. a2.a) +. (b1.g *. b2.a) +. (b1.b *. c2.a) +. (b1.a *. d2.a) in
  let cr = (c1.r *. a2.r) +. (c1.g *. b2.r) +. (c1.b *. c2.r) +. (c1.a *. d2.r) in
  let cg = (c1.r *. a2.g) +. (c1.g *. b2.g) +. (c1.b *. c2.g) +. (c1.a *. d2.g) in
  let cb = (c1.r *. a2.b) +. (c1.g *. b2.b) +. (c1.b *. c2.b) +. (c1.a *. d2.b) in
  let ca = (c1.r *. a2.a) +. (c1.g *. b2.a) +. (c1.b *. c2.a) +. (c1.a *. d2.a) in
  let dr = (d1.r *. a2.r) +. (d1.g *. b2.r) +. (d1.b *. c2.r) +. (d1.a *. d2.r) in
  let dg = (d1.r *. a2.g) +. (d1.g *. b2.g) +. (d1.b *. c2.g) +. (d1.a *. d2.g) in
  let db = (d1.r *. a2.b) +. (d1.g *. b2.b) +. (d1.b *. c2.b) +. (d1.a *. d2.b) in
  let da = (d1.r *. a2.a) +. (d1.g *. b2.a) +. (d1.b *. c2.a) +. (d1.a *. d2.a) in
  (({r=ar;g=ag;b=ab;a=aa},{r=br;g=bg;b=bb;a=ba},{r=cr;g=cg;b=cb;a=ca},{r=dr;g=dg;b=db;a=da}) : float matrix4)

let ( *::. ) mat1 mat2 = multiplyMatrices4f mat1 mat2

let multiplyMatVec4f ((a, b, c, d) : float matrix4) vec =
  {r=a.r*.vec.r+.a.g*.vec.g+.a.b*.vec.b+.a.a*.vec.a;
   g=b.r*.vec.r+.b.g*.vec.g+.b.b*.vec.b+.b.a*.vec.a;
   b=c.r*.vec.r+.c.g*.vec.g+.c.b*.vec.b+.c.a*.vec.a;
   a=d.r*.vec.r+.d.g*.vec.g+.d.b*.vec.b+.d.a*.vec.a}

let identityMatrix = ((
  {r=1.0; g=0.0; b=0.0; a=0.0},
  {r=0.0; g=1.0; b=0.0; a=0.0},
  {r=0.0; g=0.0; b=1.0; a=0.0},
  {r=0.0; g=0.0; b=0.0; a=1.0}
) : float matrix4)

let scaleMatrix (scaleVector : float vector3) = ((
  {r=scaleVector.x; g=0.0; b=0.0; a=0.0},
  {r=0.0; g=scaleVector.y; b=0.0; a=0.0},
  {r=0.0; g=0.0; b=scaleVector.z; a=0.0},
  {r=0.0; g=0.0; b=0.0; a=1.0}
) : float matrix4)

let translationMatrix (translationVector : float vector3) = ((
  {r=1.0; g=0.0; b=0.0; a=translationVector.x},
  {r=0.0; g=1.0; b=0.0; a=translationVector.y},
  {r=0.0; g=0.0; b=1.0; a=translationVector.z},
  {r=0.0; g=0.0; b=0.0; a=1.0}
) : float matrix4)

(*quaternion rotation*)
type quaternion = {r : float; i : float; j : float; k : float}

let quaternionMultiply q1 q2 =
  {r=(q1.r*.q2.r-.q1.i*.q2.i-.q1.j*.q2.j-.q1.k*.q2.k);
   i=(q1.i*.q2.r+.q1.r*.q2.i+.q1.j*.q2.k-.q1.k*.q2.j);
   j=(q1.j*.q2.r+.q1.r*.q2.j+.q1.k*.q2.i-.q1.i*.q2.k);
   k=(q1.k*.q2.r+.q1.r*.q2.k+.q1.i*.q2.j-.q1.j*.q2.i)}

(*conversion from q to a rotation matrix found on wikipedia*)
let rotationMatrix ~(axis: float vector3) ~(angle: float) =
let normalizedAxis = normalize3f axis in
let t = angle/.2. in let sint = sin t in 
let q = { r=(cos t); i=sint*.normalizedAxis.x; j=sint*.normalizedAxis.y; k=sint*.normalizedAxis.z } in
let s = 2.0/.(q.r**2.+.q.i**2.+.q.j**2.+.q.k**2.)
in ((
  { r=1.-.s*.(q.j**2.+.q.k**2.) ; g=s*.(q.i*.q.j-.q.k*.q.r) ; b=s*.(q.i*.q.k+.q.j*.q.r) ; a=0.0 },
  { r=s*.(q.i*.q.j+.q.k*.q.r) ; g=1.-.s*.(q.i**2.+.q.k**2.) ; b=s*.(q.j*.q.k-.q.i*.q.r) ; a=0.0 },
  { r=s*.(q.i*.q.k-.q.j*.q.r) ; g=s*.(q.j*.q.k+.q.i*.q.r) ; b=1.-.s*.(q.i**2.+.q.j**2.) ; a=0.0 },
  { r=0.0 ; g=0.0 ; b=0.0 ; a=1.0 }
) : float matrix4)

(*Translated directly from glm source code (for future reference: glm/ext/matrix_clip_space::perspectiveRH_NO)*)
(*Note: it is transposed compared to the glm version*)
let projectionMatrix ~fovy ~aspect ~zNear ~zFar =
  let tanHalfFovy = tan (fovy /. 2.0) in ((
    { r=1.0/.(aspect*.tanHalfFovy); g=0.0; b=0.0; a=0.0 },
    { r=0.0; g=1.0/.tanHalfFovy; b=0.0; a=0.0 },
    { r=0.0; g=0.0; b=(-.(zFar +. zNear))/.(zFar -. zNear); a=(-.2.*.zFar*.zNear)/.(zFar -. zNear) },
    { r=0.0; g=0.0; b=(-1.0); a=0.0 }
  ) : float matrix4)

(*generates a matrix that converts from world space to view space*)
let lookAtMatrix ~cameraPosition ~direction ~upVector =
  let cameraZ = {x=(-.direction.x);y=(-.direction.y);z=(-.direction.z)} in
  let cameraX = cross upVector cameraZ in
  let cameraY = cross cameraZ cameraX in
  let rot = ({r=cameraX.x; g=cameraX.y; b=cameraX.z; a=1.0},
             {r=cameraY.x; g=cameraY.y; b=cameraY.z; a=1.0},
             {r=cameraZ.x; g=cameraZ.y; b=cameraZ.z; a=1.0},
             {r=0.0; g=0.0; b=0.0; a=1.0}) in
  let trans = translationMatrix {x=(-.cameraPosition.x); y=(-.cameraPosition.y); z=(-.cameraPosition.z)} in
  rot *::. trans

(*Bigarray helpers*)
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

(*Set the i-th 4-dimensional vector in the bigarray a*)
let setVector4 a i r g b alpha =
  let j = i * 4 in
  (a.{j} <- r; a.{j+1} <- g; a.{j+2} <- b; a.{j+3} <- alpha)

let bigarrayOfMatrix4f ((a, b, c, d) : float matrix4) = 
  let bigarray = createBigarray Float32 4 4 in
  setVector4 bigarray 0 a.r a.g a.b a.a;
  setVector4 bigarray 1 b.r b.g b.b b.a;
  setVector4 bigarray 2 c.r c.g c.b c.a;
  setVector4 bigarray 3 d.r d.g d.b d.a;
  bigarray

