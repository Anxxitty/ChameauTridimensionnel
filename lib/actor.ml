open Math
open Logger

class movable ~(scene_coordinates : float vector3) ~(local_rotation : quaternion) ~(scale : float vector3) =
  object (self)
    val mutable _coordinates = scene_coordinates
    val mutable _rotation = local_rotation
    val mutable _scale = scale
    val mutable _velocity_vector = {x=0.0;y=0.0;z=0.0}
    val mutable _acceleration_vector = {x=0.0;y=0.0;z=0.0}
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
    method rotate ~(quat : quaternion) =
      _rotation <- quat_multiply quat _rotation
    (*scales the object by the given vector*)
    method scale (scaling_vector : float vector3) =
      _scale <- vec3_op ( *. ) _scale scaling_vector
    (*cinematically updates the object*)
    method tick ~(elapsed_time : float) =
      self#translate (vec3_scalar_op ( *. ) elapsed_time _velocity_vector);
      _velocity_vector <- vec3_op ( +. ) _velocity_vector (vec3_scalar_op ( *. ) elapsed_time _acceleration_vector)
  end

class camera ~scene_coordinates ~scale ~(fov : float) ~(near_plane : float) ~(far_plane : float) =
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
      let rot = rotation_matrix4f_from_quat {r=_rotation.r;i=(-._rotation.i);j=(-._rotation.j);k=(-._rotation.k)} in
      let trans = translation_matrix4f { x=(-._coordinates.x);y=(-._coordinates.y);z=(-._coordinates.z)} in
      rot *::. trans
    method rotate_yaw ~angle =
      _rotation <- quat_multiply (rotation_quat ~axis:{x=0.0;y=1.0;z=0.0} ~angle) _rotation
    method rotate_pitch ~angle =
      let pitch_axis = vec3_of_vec4 (multiply_mat4f_vec4f (rotation_matrix4f_from_quat _rotation) {x=1.0;y=0.0;z=0.0;w=1.0}) in
      _rotation <- quat_multiply (rotation_quat ~axis:pitch_axis ~angle) _rotation;

    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    method! tick ~(elapsed_time : float) =
      let camera_z = vec3_of_vec4 (multiply_mat4f_vec4f (rotation_matrix4f_from_quat _rotation) {x=0.0;y=0.0;z=(-1.0);w=1.0}) in
      logger Info ("camera_z: "^string_of_vector3f camera_z);
      let camera_x = vec3_of_vec4 (multiply_mat4f_vec4f (rotation_matrix4f_from_quat _rotation) {x=(-1.0);y=0.0;z=0.0;w=1.0}) in
      logger Info ("camera_x: "^string_of_vector3f camera_x);
      let translation = vec3_scalar_op ( *. ) elapsed_time _velocity_vector in
      let translation_along_camera_z = vec3_scalar_op ( *. ) translation.z camera_z in
      let translation_along_camera_x = vec3_scalar_op ( *. ) translation.x camera_x in
      let translation_world_space = {x=(dot3f translation_along_camera_x {x=1.0;y=0.0;z=0.0})+.(dot3f translation_along_camera_z {x=1.0;y=0.0;z=0.0});
                                   y=translation.y+.(dot3f translation_along_camera_z {x=0.0;y=1.0;z=0.0}+.(dot3f translation_along_camera_x {x=0.0;y=1.0;z=0.0}));
                                   z=(dot3f translation_along_camera_x {x=0.0;y=0.0;z=1.0})+.(dot3f translation_along_camera_z {x=0.0;y=0.0;z=1.0})} in
      self#translate translation_world_space;
      _velocity_vector <- vec3_op ( +. ) _velocity_vector (vec3_scalar_op ( *. ) elapsed_time _acceleration_vector)
  end