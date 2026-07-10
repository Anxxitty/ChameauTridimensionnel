open Math
open Logger
open Yojson
open Data_structures

class movable ~(scene_coordinates : float vector3) ~(local_rotation : quaternion) ~(scale : float vector3) =
  object (self)
    val mutable _coordinates = scene_coordinates
    val mutable _rotation = local_rotation
    val mutable _scale = scale
    val mutable _velocity_vector = vec3 0.0 0.0 0.0
    val mutable _acceleration_vector = vec3 0.0 0.0 0.0
    (*getters and setters*)
    method get_coordinates =
      _coordinates
    method get_rotation =
      _rotation
    method get_scale =
      _scale
    method get_velocity_vector =
      _velocity_vector
    method get_acceleration_vector =
      _acceleration_vector
    method set_coordinates (coordinates : float vector3) =
      _coordinates <- coordinates
    method set_rotation (rotation : quaternion) =
      _rotation <- rotation
    method set_scale (scale : float vector3) =
      _scale <- scale
    method set_velocity_vector velocity_vector =
      _velocity_vector <- velocity_vector
    method set_acceleration_vector (acceleration_vector : float vector3) =
      _acceleration_vector <- acceleration_vector
    (*other methods*)
    (*translates the object by the amount given*)
    (*sets position to 0,0,0 if result of addition is not a Vector3 (this cannot actually happen)*)
    method translate (translation : float vector3) =
      _coordinates <- vec3_op ( +. ) _coordinates translation
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
      _coordinates <- mov#get_coordinates;
      _rotation <- mov#get_rotation;
      _scale <- mov#get_scale;
      _velocity_vector <- mov#get_velocity_vector;
      _acceleration_vector <- mov#get_acceleration_vector
    (*cinematically updates the object*)
    method tick ~(elapsed_time : float) =
      self#translate (vec3_scalar_op ( *. ) elapsed_time _velocity_vector);
      _velocity_vector <- vec3_op ( +. ) _velocity_vector (vec3_scalar_op ( *. ) elapsed_time _acceleration_vector)
  end

let to_movable a =
  List.map (fun x -> (x :> movable)) a

let json_of_movable (m : movable) =
  to_string (`Assoc [
    ("coordinates", `String (json_of_vector3f m#get_coordinates));
    ("rotation", `String (json_of_quat m#get_rotation));
    ("scale", `String (json_of_vector3f m#get_scale));
    ("velocity_vector", `String (json_of_vector3f m#get_velocity_vector));
    ("acceleration_vector", `String (json_of_vector3f m#get_acceleration_vector));
  ])

let movable_of_json str =
  try
  let json = Basic.from_string str in
  let mov = new movable
    ~scene_coordinates:(vector3f_of_json (get_in_json ~key:"coordinates" ~json))
    ~local_rotation:(quat_of_json (get_in_json ~key:"rotation" ~json))
    ~scale:(vector3f_of_json (get_in_json ~key:"scale" ~json)) in
  mov#set_acceleration_vector (vector3f_of_json (get_in_json ~key:"acceleration_vector" ~json));
  mov#set_velocity_vector (vector3f_of_json (get_in_json ~key:"velocity_vector" ~json));
  mov
  with e -> logger Warning "actor: failed to parse string to actor. See error below: "; raise e

class virtual camera ~scene_coordinates ~scale ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
  object (self)
    inherit movable ~scene_coordinates ~local_rotation:identity_quat ~scale
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
      let trans = translation_matrix4f (vec3_scalar_op ( *. ) (-1.0) _coordinates) in
      rot *::. trans
  end


class fly_camera ~scene_coordinates ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
  object (self)
    inherit camera ~scene_coordinates ~scale:(vec3 0.0 0.0 0.0) ~fov ~near_plane ~far_plane

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
      self#get_velocity_vector.z <- (-._speed)
    method backward () =
      self#get_velocity_vector.z <- _speed
    method left () =
      self#get_velocity_vector.x <- (-._speed)
    method right () =
      self#get_velocity_vector.x <- _speed
    method up () =
      self#get_velocity_vector.y <- _speed
    method down () =
      self#get_velocity_vector.y <- (-._speed)
    method stop ?(x=false) ?(y=false) ?(z=false) () =
      if x then self#get_velocity_vector.x <- 0.0;
      if y then self#get_velocity_vector.y <- 0.0;
      if z then self#get_velocity_vector.z <- 0.0;
      if (not x) && (not y) && (not z) then self#set_velocity_vector (vec3 0.0 0.0 0.0) 
      

    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    (*more precisely, the x and z velocities will follow camera axes*)
    (*for the y velocity, it both follows world y_axis and camera axes: the strictly y velocity gets added to the resulting y velocities of the projected x and z velocities*)
    method! tick ~(elapsed_time : float) =
      let camera_z = rotate_vec_with_quat z_axis _rotation in
      let camera_x = rotate_vec_with_quat x_axis _rotation in
      let translation = vec3_scalar_op ( *. ) elapsed_time _velocity_vector in
      let translation_along_camera_z = vec3_scalar_op ( *. ) translation.z camera_z in
      let translation_along_camera_x = vec3_scalar_op ( *. ) translation.x camera_x in
      let translation_world_space = {x=(dot3f translation_along_camera_x x_axis)+.(dot3f translation_along_camera_z x_axis);
                                   y=translation.y+.(dot3f translation_along_camera_z y_axis+.(dot3f translation_along_camera_x y_axis));
                                   z=(dot3f translation_along_camera_x z_axis)+.(dot3f translation_along_camera_z z_axis)} in
      self#translate translation_world_space;
      _velocity_vector <- vec3_op ( +. ) _velocity_vector (vec3_scalar_op ( *. ) elapsed_time _acceleration_vector)
  end

class pov_camera ~(bound_actor : movable) ~(distance : float) ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
  object (self)
    inherit camera ~scene_coordinates:(bound_actor#get_coordinates) ~scale:(bound_actor#get_scale) ~fov ~near_plane ~far_plane
    
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
      self#get_velocity_vector.z <- _speed
    method backward () =
      self#get_velocity_vector.z <- (-._speed)
    method left () =
      self#get_velocity_vector.x <- _speed
    method right () =
      self#get_velocity_vector.x <- (-._speed)
    method up () =
      self#get_velocity_vector.y <- _speed
    method down () =
      self#get_velocity_vector.y <- (-._speed)
    method stop ?(x=false) ?(y=false) ?(z=false) () =
      if x then self#get_velocity_vector.x <- 0.0;
      if y then self#get_velocity_vector.y <- 0.0;
      if z then self#get_velocity_vector.z <- 0.0;
      if (not x) && (not y) && (not z) then self#set_velocity_vector (vec3 0.0 0.0 0.0) 

    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    (*more precisely, the x and z velocities will follow camera axes*)
    (*for the y velocity, it both follows world y_axis and camera axes: the strictly y velocity gets added to the resulting y velocities of the projected x and z velocities*)
    method! tick ~(elapsed_time : float) =
      _bound_actor#translate_local (vec3_scalar_op ( *. ) elapsed_time _velocity_vector);
      self#set_coordinates _bound_actor#get_coordinates;
      self#translate_local (vec3_scalar_op ( *. ) _distance z_axis)
  end