open Tgl3
open Logger

class inputHandler ~(key : GLFW.key) ~(callback : (unit -> unit)) =
  object (self)
    val _key = key
    val _callback = callback
    method process ~(window : GLFW.window) =
      if GLFW.getKey ~window ~key:_key then _callback ()
  end

class window ~width ~height ~name =

  let aspectRatio = ref (float_of_int width/.float_of_int height) in
  (*Callback function for window resizing event*)
  let onWindowResize aspectRatio (window : GLFW.window) newWidth newHeight =
    logger Debug "window: The window has been resized.";
    aspectRatio := (float_of_int newWidth/.float_of_int newHeight);
    Gl.viewport 0 0 newWidth newHeight in
  (*Creation of a window*)
  let glfwWindow =
    GLFW.init ();
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
    GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
    let win = GLFW.createWindow ~width ~height ~title:name () in
    GLFW.makeContextCurrent ~window:(Some win);
    logger Info "window: Successfully initialized GLFW and created a window.";
    Gl.viewport 0 0 width height;
    let _ = GLFW.setFramebufferSizeCallback ~window:win ~f:(Some (onWindowResize aspectRatio)) in
    Gl.clear_color 0.0 0.0 0.0 1.0;
    Gl.enable Gl.depth_test;
    win in

  object (self)
    val _glfwWindow = glfwWindow
    val _aspectRatio = aspectRatio
    val mutable _renderCallback = None
    val mutable _tickCallback = None
    val mutable _shouldClose = false
    val mutable _inputHandlers : (inputHandler list) = []
    val _targetTickTime = 0.015

    val mutable _lastTickTimestamp = 0.0
    val mutable _tickTime = 0.0
    val mutable _lastFrameTimestamp = 0.0
    val mutable _frameTime = 0.0
    
    method processInput () =
      let rec process l = match l with
        | [] -> ()
        | a::q -> a#process ~window:_glfwWindow; process q
      in process _inputHandlers 
    
    (*The float argument is for the aspect ratio of the window*)
    method registerRenderCallback (render : (float -> unit -> unit)) =
      _renderCallback <- Some render
    
    (*The float argument is for time*)
    method registerTickCallback (tick : (float -> unit -> unit)) =
      _tickCallback <- Some tick

    method registerInputHandler (inputHandler : inputHandler) =
      _inputHandlers <- (inputHandler::_inputHandlers)

    method getInputHandlers =
      _inputHandlers
    
    method setShouldClose b =
      _shouldClose <- b

    method shouldClose =
      _shouldClose
    
    method getFrameTime =
      _frameTime
    
    method getTickTime =
      _tickTime

    method render () =
      Gl.clear Gl.color_buffer_bit;
      Gl.clear Gl.depth_buffer_bit;
      match _renderCallback with
        | None -> ();
        | Some render -> render !_aspectRatio ();
      GLFW.swapBuffers ~window:_glfwWindow;
      let timestamp = GLFW.getTime () in
      _frameTime <- timestamp -. _lastFrameTimestamp;
      _lastFrameTimestamp <- timestamp

    method tick () =
      let timestamp = GLFW.getTime () in
      let lastLastTick = _lastTickTimestamp in
      if (timestamp -. _lastTickTimestamp >= _targetTickTime) then (
        _lastTickTimestamp <- timestamp;
        GLFW.pollEvents ();
        self#processInput ();
        match _tickCallback with
          | None -> ()
          | Some tick -> tick timestamp ();
        _tickTime <- GLFW.getTime () -. lastLastTick;
        if GLFW.windowShouldClose ~window:_glfwWindow then _shouldClose <- true)
    
    method delete () = 
        GLFW.destroyWindow ~window:_glfwWindow;
        GLFW.terminate ()
  end