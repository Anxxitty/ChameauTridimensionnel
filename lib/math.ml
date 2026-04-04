open Bigarray

(*Math helpers*)
let pi = Float.pi

let rad_of_deg angle =
  angle*.pi/.180.

let deg_of_rad angle =
  angle*.180./.pi

(*Vectors*)
type 'a vector1 = { mutable x : 'a }
type 'a vector2 = { mutable x : 'a ; mutable y : 'a }
type 'a vector3 = { mutable x : 'a ; mutable y : 'a ; mutable z : 'a}
type 'a vector4 = { mutable x : 'a ; mutable y : 'a ; mutable z : 'a ; mutable w : 'a }

type 'a vector = Vector1 of 'a vector1
               | Vector2 of 'a vector2
               | Vector3 of 'a vector3
               | Vector4 of 'a vector4

type ('a, 'b) vector_kind = Vector1_kind of ('a, 'b) kind
                         | Vector2_kind of ('a, 'b) kind
                         | Vector3_kind of ('a, 'b) kind
                         | Vector4_kind of ('a, 'b) kind

let vec4_op f (vec1 : float vector4) (vec2 : float vector4) = { x = f vec1.x vec2.x ; y = f vec1.y vec2.y ; z = f vec1.z vec2.z ; w = f vec1.w vec2.w }
let vec3_op f (vec1 : float vector3) (vec2 : float vector3) = { x = f vec1.x vec2.x ; y = f vec1.y vec2.y ; z = f vec1.z vec2.z }
let vec2_op f (vec1 : float vector2) (vec2 : float vector2) = { x = f vec1.x vec2.x ; y = f vec1.y vec2.y }
let vec1_op f (vec1 : float vector1) (vec2 : float vector1) = { x = f vec1.x vec2.x }

let vec4_scalar_op f scalar (vec : float vector4) = { x = f scalar vec.x ; y = f scalar vec.y ; z = f scalar vec.z ; w = f scalar vec.w }
let vec3_scalar_op f scalar (vec : float vector3) = { x = f scalar vec.x ; y = f scalar vec.y ; z = f scalar vec.z }
let vec2_scalar_op f scalar (vec : float vector2) = { x = f scalar vec.x ; y = f scalar vec.y }
let vec1_scalar_op f scalar (vec : float vector1) = { x = f scalar vec.x }

let dot4f (vec1 : float vector4) (vec2 : float vector4) = vec1.x*.vec2.x +. vec1.y*.vec2.y +. vec1.z*.vec2.z +. vec1.w*.vec2.w
let dot3f (vec1 : float vector3) (vec2 : float vector3) = vec1.x*.vec2.x +. vec1.y*.vec2.y +. vec1.z*.vec2.z
let dot2f (vec1 : float vector2) (vec2 : float vector2) = vec1.x*.vec2.x +. vec1.y*.vec2.y

let cross3f (vec1 : float vector3) (vec2 : float vector3) = {x=vec1.y*.vec2.z-.vec1.z*.vec2.y;y=vec1.z*.vec2.x-.vec1.x*.vec2.z;z=vec1.x*.vec2.y-.vec1.y*.vec2.x}

let normalize4f (vec : float vector4) =
  let length = sqrt (vec.x**2. +. vec.y**2. +. vec.z**2. +. vec.w**2.) in
  { x=vec.x/.length; y=vec.y/.length; z=vec.z/.length; w=vec.w/.length }

let normalize3f (vec : float vector3) =
  let length = sqrt (vec.x**2. +. vec.y**2. +. vec.z**2. ) in
  { x=vec.x/.length; y=vec.y/.length; z=vec.z/.length }

let normalize2f (vec : float vector2) =
  let length = sqrt (vec.x**2. +. vec.y**2. ) in
  { x=vec.x/.length; y=vec.y/.length }


let vec1_of_vec4 (vec : float vector4) = { x=vec.x }
let vec1_of_vec3 (vec : float vector3) = { x=vec.x }
let vec1_of_vec2 (vec : float vector2) = { x=vec.x }
let vec2_of_vec4 (vec : float vector4) = { x=vec.x ; y=vec.y }
let vec2_of_vec3 (vec : float vector3) = { x=vec.x ; y=vec.y }
let vec3_of_vec4 (vec : float vector4) = { x=vec.x ; y=vec.y ; z=vec.z }
let vec2_of_vec1 (vec : 'a vector1) y = { x=vec.x ; y=y }
let vec3_of_vec1 (vec : 'a vector1) y z = { x=vec.x ; y=y ; z=z }
let vec4_of_vec1 (vec : 'a vector1) y z w = { x=vec.x ; y=y ; z=z ; w=w } 
let vec3_of_vec2 (vec : 'a vector2) z = { x=vec.x ; y=vec.y ; z=z }
let vec4_of_vec2 (vec : 'a vector2) z w = { x=vec.x ; y=vec.y ; z=z ; w=w } 
let vec4_of_vec3 (vec : 'a vector3) w = { x=vec.x ; y=vec.y ; z=vec.z ; w=w }

let vec1 x = { x=x }
let vec2 x y = { x=x ; y=y }
let vec3 x y z = { x=x ; y=y ; z=z }
let vec4 x y z w = { x=x ; y=y ; z=z ; w=w }

let x_axis = vec3 1.0 0.0 0.0
let y_axis = vec3 0.0 1.0 0.0
let z_axis = vec3 0.0 0.0 1.0

let vec3f_in_base ~(vector : float vector3) ~(base : float vector3 * float vector3 * float vector3) =
  let ex,ey,ez = base in
  let x = vec3_scalar_op ( *. ) vector.x ex in
  let y = vec3_scalar_op ( *. ) vector.y ey in
  let z = vec3_scalar_op ( *. ) vector.z ez in
  {
    x = dot3f x x_axis +. dot3f y x_axis +. dot3f z x_axis;
    y = dot3f x y_axis +. dot3f y y_axis +. dot3f z y_axis;
    z = dot3f x z_axis +. dot3f y z_axis +. dot3f z z_axis
  }

let string_of_vector1f (vec : float vector1) = "{ x = "^string_of_float vec.x^" }"
let string_of_vector2f (vec : float vector2) = "{ x = "^string_of_float vec.x^"; y = "^string_of_float vec.y^" }"
let string_of_vector3f (vec : float vector3) = "{ x = "^string_of_float vec.x^"; y = "^string_of_float vec.y^"; z = "^string_of_float vec.z^" }"
let string_of_vector4f (vec : float vector4) = "{ x = "^string_of_float vec.x^"; y = "^string_of_float vec.y^"; z = "^string_of_float vec.z^"; w = "^string_of_float vec.w^" }"



(*Quaternions*)
type quaternion = { mutable r : float; mutable i : float; mutable j : float; mutable k : float}


let multiply_quat q1 q2 =
  {r=(q1.r*.q2.r-.q1.i*.q2.i-.q1.j*.q2.j-.q1.k*.q2.k);
   i=(q1.i*.q2.r+.q1.r*.q2.i+.q1.j*.q2.k-.q1.k*.q2.j);
   j=(q1.j*.q2.r+.q1.r*.q2.j+.q1.k*.q2.i-.q1.i*.q2.k);
   k=(q1.k*.q2.r+.q1.r*.q2.k+.q1.i*.q2.j-.q1.j*.q2.i)}

let rotation_quat ~(axis : float vector3) ~angle =
  let sin_half_angle = sin (angle /. 2.) in
  let normalized_axis = normalize3f axis in
  { r = cos (angle/.2.) ; i = normalized_axis.x *. sin_half_angle ; j = normalized_axis.y *. sin_half_angle ; k = normalized_axis.z *. sin_half_angle }

let roll_of_rot_quat quat = atan2 (2.*.(quat.r*.quat.k +. quat.i*.quat.j)) (1.-.2.*.(quat.j**2. +. quat.k**2.))
let pitch_of_rot_quat quat = atan2 (2.*.(quat.r*.quat.i +. quat.j*.quat.k)) (1.-.2.*.(quat.i**2. +. quat.j**2.))
let yaw_of_rot_quat quat = asin (2. *. (quat.r*.quat.j -. quat.k*.quat.i))

let vec4_of_quat quat = { x=quat.r ; y=quat.i ; z=quat.j ; w=quat.k }
let quat_of_vec4 vec = { r=vec.x ; i=vec.y ; j=vec.z ; k=vec.w }

let vec3_of_quat quat = { x=quat.i ; y=quat.j ; z=quat.k }
let quat_of_vec3 (vec : float vector3) = { r=0.0 ; i=vec.x ; j=vec.y ; k=vec.z }

let conj_quat quat = { r=quat.r ; i=(-.quat.i) ; j=(-.quat.j) ; k=(-.quat.k) }

let rotate_vec_with_quat (vector : float vector3) (quat : quaternion) =
  let p = quat_of_vec3 vector in
  vec3_of_quat (multiply_quat (multiply_quat quat p) (conj_quat quat))


let identity_quat = {r=1.0;i=0.0;j=0.0;k=0.0}

(*Matrices*)
type 'a matrix4 = 'a vector4 * 'a vector4 * 'a vector4 * 'a vector4

let multiply_matrix4f ((a1, b1, c1, d1) : float matrix4) ((a2, b2, c2, d2) : float matrix4) =
  let ax = (a1.x *. a2.x) +. (a1.y *. b2.x) +. (a1.z *. c2.x) +. (a1.w *. d2.x) in
  let ay = (a1.x *. a2.y) +. (a1.y *. b2.y) +. (a1.z *. c2.y) +. (a1.w *. d2.y) in
  let az = (a1.x *. a2.z) +. (a1.y *. b2.z) +. (a1.z *. c2.z) +. (a1.w *. d2.z) in
  let aw = (a1.x *. a2.w) +. (a1.y *. b2.w) +. (a1.z *. c2.w) +. (a1.w *. d2.w) in
  let bx = (b1.x *. a2.x) +. (b1.y *. b2.x) +. (b1.z *. c2.x) +. (b1.w *. d2.x) in
  let by = (b1.x *. a2.y) +. (b1.y *. b2.y) +. (b1.z *. c2.y) +. (b1.w *. d2.y) in
  let bz = (b1.x *. a2.z) +. (b1.y *. b2.z) +. (b1.z *. c2.z) +. (b1.w *. d2.z) in
  let bw = (b1.x *. a2.w) +. (b1.y *. b2.w) +. (b1.z *. c2.w) +. (b1.w *. d2.w) in
  let cx = (c1.x *. a2.x) +. (c1.y *. b2.x) +. (c1.z *. c2.x) +. (c1.w *. d2.x) in
  let cy = (c1.x *. a2.y) +. (c1.y *. b2.y) +. (c1.z *. c2.y) +. (c1.w *. d2.y) in
  let cz = (c1.x *. a2.z) +. (c1.y *. b2.z) +. (c1.z *. c2.z) +. (c1.w *. d2.z) in
  let cw = (c1.x *. a2.w) +. (c1.y *. b2.w) +. (c1.z *. c2.w) +. (c1.w *. d2.w) in
  let dx = (d1.x *. a2.x) +. (d1.y *. b2.x) +. (d1.z *. c2.x) +. (d1.w *. d2.x) in
  let dy = (d1.x *. a2.y) +. (d1.y *. b2.y) +. (d1.z *. c2.y) +. (d1.w *. d2.y) in
  let dz = (d1.x *. a2.z) +. (d1.y *. b2.z) +. (d1.z *. c2.z) +. (d1.w *. d2.z) in
  let dw = (d1.x *. a2.w) +. (d1.y *. b2.w) +. (d1.z *. c2.w) +. (d1.w *. d2.w) in
  (({x=ax;y=ay;z=az;w=aw},{x=bx;y=by;z=bz;w=bw},{x=cx;y=cy;z=cz;w=cw},{x=dx;y=dy;z=dz;w=dw}) : float matrix4)

let ( *::. ) mat1 mat2 = multiply_matrix4f mat1 mat2

let multiply_mat4f_vec4f ((a, b, c, d) : float matrix4) vec =
  {x=a.x*.vec.x+.a.y*.vec.y+.a.z*.vec.z+.a.w*.vec.w;
   y=b.x*.vec.x+.b.y*.vec.y+.b.z*.vec.z+.b.w*.vec.w;
   z=c.x*.vec.x+.c.y*.vec.y+.c.z*.vec.z+.c.w*.vec.w;
   w=d.x*.vec.x+.d.y*.vec.y+.d.z*.vec.z+.d.w*.vec.w}

let transpose ((a, b, c, d) : 'a matrix4) =
  ({x=a.x; y=b.x; z=c.x; w=d.x},
   {x=a.y; y=b.y; z=c.y; w=d.y},
   {x=a.z; y=b.z; z=c.z; w=d.z},
   {x=a.w; y=b.w; z=c.w; w=d.w})

(*let inverse4f mat = ??*)

let identity_matrix4f = ((
  {x=1.0; y=0.0; z=0.0; w=0.0},
  {x=0.0; y=1.0; z=0.0; w=0.0},
  {x=0.0; y=0.0; z=1.0; w=0.0},
  {x=0.0; y=0.0; z=0.0; w=1.0}
) : float matrix4)

let scale_matrix4f (scale_vector : float vector3) = ((
  {x=scale_vector.x; y=0.0; z=0.0; w=0.0},
  {x=0.0; y=scale_vector.y; z=0.0; w=0.0},
  {x=0.0; y=0.0; z=scale_vector.z; w=0.0},
  {x=0.0; y=0.0; z=0.0; w=1.0}
) : float matrix4)

let translation_matrix4f (translation_vector : float vector3) = ((
  {x=1.0; y=0.0; z=0.0; w=translation_vector.x},
  {x=0.0; y=1.0; z=0.0; w=translation_vector.y},
  {x=0.0; y=0.0; z=1.0; w=translation_vector.z},
  {x=0.0; y=0.0; z=0.0; w=1.0}
) : float matrix4)

(*Translated directly from glm source code (for future reference: glm/ext/matrix_clip_space::perspectiveRH_NO)*)
(*Note: it is transposed compared to the glm version*)
let projection_matrix ~fov ~aspect_ratio ~near_plane ~far_plane =
  let tan_half_fov = tan (fov /. 2.0) in ((
    { x=1.0/.(aspect_ratio*.tan_half_fov); y=0.0; z=0.0; w=0.0 },
    { x=0.0; y=1.0/.tan_half_fov; z=0.0; w=0.0 },
    { x=0.0; y=0.0; z=(-.(far_plane +. near_plane))/.(far_plane -. near_plane); w=(-.2.*.far_plane*.near_plane)/.(far_plane -. near_plane) },
    { x=0.0; y=0.0; z=(-1.0); w=0.0 }
  ) : float matrix4)

(*generates a matrix that converts from world space to view space*)
(*almost the same as glm/ext/matrix_transform::lookAtRH*)
(*direction is a vector pointing from the camera to the object looked at*)
(*upVector is the vertical vector in world coordinates*)
let look_at_matrix ~camera_position ~direction ~up_vector =
  let camera_z = normalize3f direction in
  let camera_x = normalize3f (cross3f camera_z up_vector) in
  let camera_y = cross3f camera_x camera_z in
  ({x=camera_x.x;     y=camera_x.y;     z=camera_x.z;     w=(-.(dot3f camera_x camera_position))},
   {x=camera_y.x;     y=camera_y.y;     z=camera_y.z;     w=(-.(dot3f camera_y camera_position))},
   {x=(-.camera_z.x); y=(-.camera_z.y); z=(-.camera_z.z); w=(dot3f camera_z camera_position)},
   {x=0.0; y=0.0; z=0.0; w=1.0})

(*conversion from quaternion to a rotation matrix found on wikipedia*)
let rotation_matrix4f_from_quat (quat: quaternion) =
  let s = 2.0/.(quat.r**2.+.quat.i**2.+.quat.j**2.+.quat.k**2.)
  in ((
    { x=1.-.s*.(quat.j**2.+.quat.k**2.) ; y=s*.(quat.i*.quat.j-.quat.k*.quat.r) ; z=s*.(quat.i*.quat.k+.quat.j*.quat.r) ; w=0.0 },
    { x=s*.(quat.i*.quat.j+.quat.k*.quat.r) ; y=1.-.s*.(quat.i**2.+.quat.k**2.) ; z=s*.(quat.j*.quat.k-.quat.i*.quat.r) ; w=0.0 },
    { x=s*.(quat.i*.quat.k-.quat.j*.quat.r) ; y=s*.(quat.j*.quat.k+.quat.i*.quat.r) ; z=1.-.s*.(quat.i**2.+.quat.j**2.) ; w=0.0 },
    { x=0.0 ; y=0.0 ; z=0.0 ; w=1.0 }
  ) : float matrix4)

let rotation_matrix4f ~(axis: float vector3) ~(angle: float) =
  let q = rotation_quat ~axis ~angle in 
  rotation_matrix4f_from_quat q


(*let pitch_of_rotation_matrix4f = ?? *)
(*let yaw_of_rotation_matrix4f = ??*)
(*let roll_of_rot_mat = ??*)