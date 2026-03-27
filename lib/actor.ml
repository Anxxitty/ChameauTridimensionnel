open Math
open Logger

class movable ~(scene_coordinates : float vector3) ~(local_rotation : float matrix4) ~(scale : float vector3) =
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
    method set_rotation (rotation : float matrix4) =
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
    method rotate ~(axis : float vector3) ~(angle : float) =
      _rotation <- (_rotation *::. (rotation_matrix4f ~axis ~angle))
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
    inherit movable ~scene_coordinates ~local_rotation:identity_matrix4f ~scale
    val mutable _fov = fov
    val mutable _near_plane = near_plane
    val mutable _far_plane = far_plane
    val mutable _pitch = 0.0
    val mutable _yaw = rad_of_deg 90.0
    val mutable _camera_direction = {x=0.0;y=0.0;z=(-1.0)} (*initially the camera looks in the negative z direction*)
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
      look_at_matrix ~camera_position:_coordinates ~direction:_camera_direction ~up_vector:{x=0.0;y=1.0;z=0.0}
    method rotate_yaw ~angle =
      _yaw <- _yaw +. angle
    method rotate_pitch ~angle =
      _pitch <- _pitch +. angle;
      if _pitch > rad_of_deg 89.0 then _pitch <- rad_of_deg 89.0;
      if _pitch < rad_of_deg (-89.0) then _pitch <- rad_of_deg (-89.0);
      logger Info ("pitch: "^string_of_float (deg_of_rad _pitch));
      logger Info ("yaw: "^string_of_float (deg_of_rad _yaw))

    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    method! tick ~(elapsed_time : float) =
      _camera_direction <- {x=cos _yaw;y=(-.sin _pitch);z=(-.cos _pitch)};
      let translation = vec3_scalar_op ( *. ) elapsed_time _velocity_vector in
      let translation_along_camera_z = vec3_scalar_op ( *. ) translation.z _camera_direction in
      let translation_along_camera_x = vec3_scalar_op ( *. ) translation.x (cross3f _camera_direction {x=0.0;y=1.0;z=0.0}) in
      let translation_world_space = {x=(dot3f translation_along_camera_x {x=1.0;y=0.0;z=0.0})+.(dot3f translation_along_camera_z {x=1.0;y=0.0;z=0.0});
                                   y=translation.y+.(dot3f translation_along_camera_z {x=0.0;y=1.0;z=0.0}+.(dot3f translation_along_camera_x {x=0.0;y=1.0;z=0.0}));
                                   z=(dot3f translation_along_camera_x {x=0.0;y=0.0;z=(-1.0)})+.(dot3f translation_along_camera_z {x=0.0;y=0.0;z=(-1.0)})} in
      self#translate translation_world_space;
      _velocity_vector <- vec3_op ( +. ) _velocity_vector (vec3_scalar_op ( *. ) elapsed_time _acceleration_vector)
  end