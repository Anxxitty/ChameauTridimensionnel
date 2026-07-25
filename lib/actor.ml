open Math
open Logger
open Yojson
open Data_structures
open Shader

class movable ~(position : float vector3) ~(rotation : quaternion) ~(scale : float vector3) =
  object (self)
    val mutable _position = position
    val mutable _rotation = rotation
    val mutable _scale = scale
    val mutable _velocity = vec3 0.0 0.0 0.0
    val mutable _acceleration = vec3 0.0 0.0 0.0
    (*getters and setters*)
    method get_position =
      _position
    method get_rotation =
      _rotation
    method get_scale =
      _scale
    method get_velocity =
      _velocity
    method get_acceleration =
      _acceleration
    method set_position (position : float vector3) =
      _position <- position
    method set_rotation (rotation : quaternion) =
      _rotation <- rotation
    method set_scale (scale : float vector3) =
      _scale <- scale
    method set_velocity velocity =
      _velocity <- velocity
    method set_acceleration (acceleration : float vector3) =
      _acceleration <- acceleration
    (*other methods*)
    (*translates the object by the amount given*)
    (*sets position to 0,0,0 if result of addition is not a Vector3 (this cannot actually happen)*)
    method get_local_x_axis =
      rotate_vec_with_quat x_axis _rotation
    method get_local_y_axis =
      rotate_vec_with_quat y_axis _rotation
    method get_local_z_axis =
      rotate_vec_with_quat z_axis _rotation
    method translate (translation : float vector3) =
      _position <- vec3_op ( +. ) _position translation
    (*rotates the object along the given axis by the given angle*)
    method translate_local (translation : float vector3) =
      self#translate (vec3f_in_base ~vector:translation ~base:(base_of_rot ~quat:_rotation))
    method rotate ~(quat : quaternion) =
      _rotation <- multiply_quat quat _rotation
    method rotate_local ~(axis : float vector3) ~(angle : float) =
      _rotation <- multiply_quat (rotation_quat ~axis:(vec3f_in_base ~vector:axis ~base:(base_of_rot ~quat:_rotation)) ~angle) _rotation
    (*scales the object by the given vector*)
    method scale (scaling_vector : float vector3) =
      _scale <- vec3_op ( *. ) _scale scaling_vector
    method copy_movable (mov : movable) =
      _position <- mov#get_position;
      _rotation <- mov#get_rotation;
      _scale <- mov#get_scale;
      _velocity <- mov#get_velocity;
      _acceleration <- mov#get_acceleration
    (*cinematically updates the object*)
    method tick ~(elapsed_time : float) =
      self#translate (vec3_scalar_op ( *. ) elapsed_time _velocity);
      _velocity <- vec3_op ( +. ) _velocity (vec3_scalar_op ( *. ) elapsed_time _acceleration)
  end

let with_default_positionning f = f ~position:(vec3 0.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)

let to_movable a =
  List.map (fun x -> (x :> movable)) a

let json_of_movable (m : movable) =
  to_string (`Assoc [
    ("position", `String (json_of_vector3f m#get_position));
    ("rotation", `String (json_of_quat m#get_rotation));
    ("scale", `String (json_of_vector3f m#get_scale));
    ("velocity", `String (json_of_vector3f m#get_velocity));
    ("acceleration", `String (json_of_vector3f m#get_acceleration));
  ])

let movable_of_json str =
  try
    let json = Basic.from_string str in
    let mov = new movable
      ~position:(vector3f_of_json (get_in_json ~key:"position" ~json))
      ~rotation:(quat_of_json (get_in_json ~key:"rotation" ~json))
      ~scale:(vector3f_of_json (get_in_json ~key:"scale" ~json)) in
    mov#set_acceleration (vector3f_of_json (get_in_json ~key:"acceleration" ~json));
    mov#set_velocity (vector3f_of_json (get_in_json ~key:"velocity" ~json));
    mov
  with e -> logger Warning "actor: failed to parse string to actor. See error below: "; raise e

class virtual camera ~position ~rotation ~scale ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
  object (self)
    inherit movable ~position ~rotation ~scale
    val mutable _fov = fov
    val mutable _near_plane = near_plane
    val mutable _far_plane = far_plane
    (*getters and setters*)
    method get_fov =
      _fov
    method get_near_plane =
      _near_plane
    method get_far_plane =
      _far_plane
    method set_fov fov =
      _fov <- fov
    method set_near_plane near_plane =
      _near_plane <- near_plane
    method set_far_plane far_plane =
      _far_plane <- far_plane
    
    method gen_projection_matrix ~(aspect_ratio : float) =
      projection_matrix ~fov:_fov ~aspect_ratio ~near_plane:_near_plane ~far_plane:_far_plane
      
    method gen_view_matrix =
      let rot = rotation_matrix4f_from_quat (conj_quat _rotation) in
      let trans = translation_matrix4f (vec3_scalar_op ( *. ) (-1.0) _position) in
      rot *::. trans
  end


class fly_camera ~position ~rotation ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
  object (self)
    inherit camera ~position ~rotation ~scale:(vec3 0.0 0.0 0.0) ~fov ~near_plane ~far_plane

    val mutable _speed = 5.0

    method get_speed =
      _speed
    method set_speed s =
      _speed <- s

    method rotate_yaw ~angle =
      _rotation <- multiply_quat (rotation_quat ~axis:y_axis ~angle) _rotation
    method rotate_pitch ~angle =
      (*the exact pitch axis is recalculated before each pitch rotation to avoid slightly rolling because rotating along a slightly disoriented axis*)
      let pitch_axis = rotate_vec_with_quat x_axis _rotation in
      _rotation <- multiply_quat (rotation_quat ~axis:pitch_axis ~angle) _rotation
    method rotate_roll ~angle =
      let roll_axis = rotate_vec_with_quat z_axis _rotation in
      _rotation <- multiply_quat (rotation_quat ~axis:roll_axis ~angle) _rotation
    
    method forward () =
      self#get_velocity.z <- (-._speed)
    method backward () =
      self#get_velocity.z <- _speed
    method left () =
      self#get_velocity.x <- (-._speed)
    method right () =
      self#get_velocity.x <- _speed
    method up () =
      self#get_velocity.y <- _speed
    method down () =
      self#get_velocity.y <- (-._speed)
    method stop ?(x=false) ?(y=false) ?(z=false) () =
      if x then self#get_velocity.x <- 0.0;
      if y then self#get_velocity.y <- 0.0;
      if z then self#get_velocity.z <- 0.0;
      if (not x) && (not y) && (not z) then self#set_velocity (vec3 0.0 0.0 0.0) 
      

    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    (*more precisely, the x and z velocities will follow camera axes*)
    (*for the y velocity, it both follows world y_axis and camera axes: the strictly y velocity gets added to the resulting y velocities of the projected x and z velocities*)
    (*cs stands for camera space*)
    method! tick ~(elapsed_time : float) =
      let translation_cs = vec3_scalar_op ( *. ) elapsed_time _velocity in (*not exactly camera_space... its x and z components are in camera_space, but its y component is along the ws y_axis*)
      self#translate_local (vec3 translation_cs.x 0.0 translation_cs.z);
      self#translate (vec3 0.0 translation_cs.y 0.0);
      _velocity <- vec3_op ( +. ) _velocity (vec3_scalar_op ( *. ) elapsed_time _acceleration)
  end

class pov_camera ~(bound_actor : movable) ~(distance : float) ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
  object (self)
    inherit camera ~position:(bound_actor#get_position) ~rotation:(bound_actor#get_rotation) ~scale:(bound_actor#get_scale) ~fov ~near_plane ~far_plane
    
    val mutable _bound_actor = bound_actor
    val mutable _distance = distance

    val mutable _speed = 5.0

    method set_bound_actor a =
      _bound_actor <- a
    method get_bound_actor =
      _bound_actor
    
    method set_distance d =
      _distance <- d
    method get_distance =
      _distance

    method get_speed =
      _speed
    method set_speed s =
      _speed <- s

    method rotate_yaw ~angle =
      _bound_actor#rotate ~quat:(rotation_quat ~axis:(vec3 0.0 1.0 0.0) ~angle);
      self#rotate ~quat:(rotation_quat ~axis:(vec3 0.0 1.0 0.0) ~angle)
    method rotate_pitch ~angle =
      self#rotate_local ~axis:(vec3 1.0 0.0 0.0) ~angle
    method rotate_roll ~angle =
      _bound_actor#rotate_local ~axis:(vec3 0.0 0.0 1.0) ~angle;
      self#rotate_local ~axis:(vec3 0.0 0.0 1.0) ~angle
    
    method forward () =
      self#get_velocity.z <- _speed
    method backward () =
      self#get_velocity.z <- (-._speed)
    method left () =
      self#get_velocity.x <- _speed
    method right () =
      self#get_velocity.x <- (-._speed)
    method up () =
      self#get_velocity.y <- _speed
    method down () =
      self#get_velocity.y <- (-._speed)
    method stop ?(x=false) ?(y=false) ?(z=false) () =
      if x then self#get_velocity.x <- 0.0;
      if y then self#get_velocity.y <- 0.0;
      if z then self#get_velocity.z <- 0.0;
      if (not x) && (not y) && (not z) then self#set_velocity (vec3 0.0 0.0 0.0) 

    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    (*more precisely, the x and z velocities will follow camera axes*)
    (*for the y velocity, it both follows world y_axis and camera axes: the strictly y velocity gets added to the resulting y velocities of the projected x and z velocities*)
    method! tick ~(elapsed_time : float) =
      _bound_actor#translate_local (vec3_scalar_op ( *. ) elapsed_time _velocity);
      self#set_position _bound_actor#get_position;
      self#translate_local (vec3_scalar_op ( *. ) _distance z_axis)
  end

class light ~position ~(ambient : float vector3) ~(diffuse : float vector3) ~(specular : float vector3) ~intensity =
  object (self)
    inherit movable ~position ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0) as super
    val mutable _ambient = ambient
    val mutable _diffuse = diffuse
    val mutable _specular = specular
    val mutable _intensity = intensity
    val mutable _bound_movable : movable option = None
    (*getters and setters*)
    method get_ambient =
      _ambient
    method get_diffuse =
      _diffuse
    method get_specular =
      _specular
    method set_ambient c =
      _ambient <- c
    method set_diffuse c =
      _diffuse <- c
    method set_specular c =
      _specular <- c
    method get_bound_movable =
      _bound_movable
    method bind_movable m =
      _bound_movable <- m
    method! tick ~elapsed_time =
      match _bound_movable with
        | None -> ()
        | Some m -> self#copy_movable m;
      super#tick ~elapsed_time
    method get_uniforms ~(view_matrix : float matrix4) ~(uniform_prefix : string) = 
      [Vector_uniform3f (uniform_prefix^".ambient", self#get_ambient);
       Vector_uniform3f (uniform_prefix^".diffuse", self#get_diffuse);
       Vector_uniform3f (uniform_prefix^".specular", self#get_specular);
       Vector_uniform1f (uniform_prefix^".intensity", vec1 _intensity)]
  end

class directional_light ~(direction : float vector3) ~(ambient : float vector3) ~(diffuse : float vector3) ~(specular : float vector3) ~(intensity : float) =
  object (self)
    inherit light ~position:(vec3 0.0 0.0 0.0) ~ambient ~diffuse ~specular ~intensity as super
    initializer 
      let local_x = normalize3f (cross3f y_axis direction) in
      let local_y = normalize3f (cross3f direction local_x) in
      super#set_rotation (rot_quat_of_base ~base:(local_x, local_y, normalize3f(direction))) 
    method get_direction =
      self#get_local_z_axis
    method set_direction z =
      let local_x = normalize3f (cross3f y_axis z) in
      let local_y = normalize3f (cross3f z local_x) in
      _rotation <- rot_quat_of_base ~base:(local_x, local_y, normalize3f(z))
    method! get_uniforms ~view_matrix ~uniform_prefix =
      Vector_uniform3f (uniform_prefix^".direction", vec3_of_vec4 (multiply_mat4f_vec4f view_matrix (vec4_of_vec3 self#get_direction 0.0)))
      ::(super#get_uniforms ~view_matrix ~uniform_prefix)
  end

class point_light ~position ~ambient ~diffuse ~specular ~intensity ~(linear_attenuation : float) ~(quadratic_attenuation : float) =
  object (self)
    inherit light ~position ~ambient ~diffuse ~specular ~intensity as super
    val mutable _linear_attenuation = linear_attenuation
    val mutable _quadratic_attenuation = quadratic_attenuation
    method get_linear_attenuation =
      _linear_attenuation
    method get_quadratic_attenuation =
      _quadratic_attenuation
    method set_linear_attenuation x =
      _linear_attenuation <- x
    method set_quadratic_attenuation x =
      _quadratic_attenuation <- x
    method! get_uniforms ~view_matrix ~uniform_prefix =
      [Vector_uniform3f (uniform_prefix^".position", vec3_of_vec4 (multiply_mat4f_vec4f view_matrix (vec4_of_vec3 self#get_position 1.0)));
      Vector_uniform1f (uniform_prefix^".linear_attenuation", vec1 self#get_linear_attenuation);
      Vector_uniform1f (uniform_prefix^".quadratic_attenuation", vec1 self#get_quadratic_attenuation)]
      @(super#get_uniforms ~view_matrix ~uniform_prefix)
  end

class spot_light ~position ~(direction : float vector3) ~(inner_cut_off_angle : float) ~(outer_cut_off_angle : float) ~ambient ~diffuse ~specular ~intensity ~(linear_attenuation : float) ~(quadratic_attenuation : float) =
  object (self)
    inherit directional_light ~direction ~ambient ~diffuse ~specular ~intensity
    inherit! point_light ~position ~ambient ~diffuse ~specular ~linear_attenuation ~quadratic_attenuation ~intensity as super
    val mutable _direction = direction
    val mutable _inner_cut_off_angle = inner_cut_off_angle
    val mutable _outer_cut_off_angle = outer_cut_off_angle
    method get_inner_cut_off_angle =
      _inner_cut_off_angle
    method set_inner_cut_off_angle x =
      _inner_cut_off_angle <- x
    method get_outer_cut_off_angle =
      _outer_cut_off_angle
    method set_outer_cut_off_angle x =
      _outer_cut_off_angle <- x
    method! get_uniforms ~view_matrix ~uniform_prefix =
      [Vector_uniform3f (uniform_prefix^".direction", vec3_of_vec4 (multiply_mat4f_vec4f view_matrix (vec4_of_vec3 self#get_direction 0.0)));
      Vector_uniform1f (uniform_prefix^".inner_cut_off_angle", vec1 (cos self#get_inner_cut_off_angle));
      Vector_uniform1f (uniform_prefix^".outer_cut_off_angle", vec1 (cos self#get_outer_cut_off_angle))]
      @(super#get_uniforms ~view_matrix ~uniform_prefix)
  end

let to_light (l : #light list) =
  List.map (fun l -> (l :> light)) l