open Vector
open Logger

class movable ~(sceneCoordinates : float vector3) ~(localRotation : float matrix4) ~(scale : float vector3) =
  object (self)
    val mutable _coordinates = sceneCoordinates
    val mutable _rotation = localRotation
    val mutable _scale = scale
    val mutable _velocityVector = {x=0.0;y=0.0;z=0.0}
    val mutable _accelerationVector = {x=0.0;y=0.0;z=0.0}
    (*getters and setters*)
    method getCoordinates =
      _coordinates
    method getRotation =
      _rotation
    method getScale =
      _scale
    method getVelocityVector =
      _velocityVector
    method getAccelerationVector =
      _accelerationVector
    method setCoordinates (coordinates : float vector3) =
      _coordinates <- coordinates
    method setRotation (rotation : float matrix4) =
      _rotation <- rotation
    method setScale (scale : float vector3) =
      _scale <- scale
    method setVelocityX (vx : float) =
      _velocityVector <- {x=vx;y=_velocityVector.y;z=_velocityVector.z}
    method setVelocityY (vy : float) =
      _velocityVector <- {x=_velocityVector.x;y=vy;z=_velocityVector.z}
    method setVelocityZ (vz : float) =
      _velocityVector <- {x=_velocityVector.x;y=_velocityVector.y;z=vz}
    method setVelocityVector velocityVector =
      _velocityVector <- velocityVector
    method setAccelerationVector (accelerationVector : float vector3) =
      _accelerationVector <- accelerationVector
    (*other methods*)
    (*translates the object by the amount given*)
    (*sets position to 0,0,0 if result of addition is not a Vector3 (this cannot actually happen)*)
    method translate (translation : float vector3) =
      _coordinates <- match (Vector3 _coordinates) +:. (Vector3 translation) with | Vector3 vec -> vec | _ -> {x=0.0;y=0.0;z=0.0}
    (*rotates the object along the given axis by the given angle*)
    method rotate ~(axis : float vector3) ~(angle : float) =
      _rotation <- (_rotation *::. (rotationMatrix ~axis ~angle))
    (*scales the object by the given vector*)
    method scale (scalingVector : float vector3) =
      _scale <- { x = _scale.x *. scalingVector.x; y = _scale.y *. scalingVector.y; z = _scale.z *. scalingVector.z }
    (*cinematically updates the object*)
    method tick ~(elapsedTime : float) =
      self#translate {x=elapsedTime*._velocityVector.x;y=elapsedTime*._velocityVector.y;z=elapsedTime*._velocityVector.z};
      _velocityVector <- match (Vector3 _velocityVector+:.Vector3 {x=elapsedTime*._accelerationVector.x;y=elapsedTime*._accelerationVector.y;z=elapsedTime*._accelerationVector.z}) with
        | Vector3 vec -> vec
        | _ -> {x=0.0;y=0.0;z=0.0}
  end

class camera ~sceneCoordinates ~scale ~(fov : float) ~(nearPlane : float) ~(farPlane : float) =
  object (self)
    inherit movable ~sceneCoordinates ~localRotation:identityMatrix ~scale
    val mutable _fov = fov
    val mutable _nearPlane = nearPlane
    val mutable _farPlane = farPlane
    val mutable _viewRotMatrix = identityMatrix (*initially the camera looks in the negative z direction*)
    (*getters and setters*)
    method getFOV =
      _fov
    method getNearPlane =
      _nearPlane
    method getFarPlane =
      _farPlane
    method setFOV fov =
      _fov <- fov
    method setNearPlane nearPlane =
      _nearPlane <- nearPlane
    method setFarPlane farPlane =
      _farPlane <- farPlane
    method genProjectionMatrix ~(aspectRatio : float) =
      projectionMatrix ~fovy:_fov ~aspect:aspectRatio ~zNear:_nearPlane ~zFar:_farPlane
    method genViewMatrix =(*
      let direction = multiplyMatVec4f _rotation {r=0.0;g=0.0;b=(-1.0);a=1.0} in
      lookAtMatrix ~cameraPosition:_coordinates ~direction:({x=direction.r;y=direction.g;z=direction.b}) ~upVector:{x=0.0;y=1.0;z=0.0}*)
      _viewRotMatrix *::. translationMatrix {x=(-._coordinates.x);y=(-._coordinates.y);z=(-._coordinates.z)}
    (*tick is overriden so that the camera moves in the direction it is pointing and not along the world axes*)
    method! tick ~(elapsedTime : float) =
      let trans = multiplyMatVec4f _rotation {r=elapsedTime*._velocityVector.x;g=elapsedTime*._velocityVector.y;b=elapsedTime*._velocityVector.z;a=1.0} in
      self#translate {x=trans.r;y=trans.g;z=trans.b};
      _velocityVector <- match (Vector3 _velocityVector+:.Vector3 {x=elapsedTime*._accelerationVector.x;y=elapsedTime*._accelerationVector.y;z=elapsedTime*._accelerationVector.z}) with
        | Vector3 vec -> vec
        | _ -> {x=0.0;y=0.0;z=0.0}
    (*rotate is overriden to stop the player from looking directly vertically / beyond*)
    method! rotate ~(axis : float vector3) ~(angle : float) =
      let direction = multiplyMatVec4f _rotation {r=0.0;g=0.0;b=(-1.0);a=1.0} in
      let cameraAngleToVertical = acos (dot {x=direction.r;y=direction.g;z=direction.b} {x=0.0;y=1.0;z=0.0}) in
      logger DebugMainLoop (string_of_float (degOfRadians cameraAngleToVertical));
      (*let willRotateVertically = (axis.x <> 0.0) in
      let willGoUp = willRotateVertically && ((angle > 0.0 && axis.x > 0.0) || (angle < 0.0 && axis.x < 0.0)) in*)
      (*if not ((cameraAngleToVertical > radiansOfDeg 150. && (not willGoUp)) || (cameraAngleToVertical < radiansOfDeg 40. && willGoUp)) then ( *)
        _rotation <- (_rotation *::. (rotationMatrix ~axis ~angle));
        _viewRotMatrix <- (_viewRotMatrix *::. (rotationMatrix ~axis ~angle:(-.angle)))
  end