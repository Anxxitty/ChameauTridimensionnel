open Tgl3
open Logger

class inputHandler ~(key : GLFW.key) ~(pressCallback : (unit -> unit)) ~(releaseCallback : (unit -> unit)) =
  object (self)
    val _key = key
    val _pressCallback = pressCallback
    val _releaseCallback = releaseCallback
    val mutable _isPressed = false
    method process ~(window : GLFW.window) =
      if GLFW.getKey ~window ~key:_key
        then (_pressCallback (); _isPressed <- true)
        else if _isPressed
          then (_releaseCallback (); _isPressed <- false)
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
    GLFW.swapInterval ~interval:0; (*Disables V-Sync*)
    GLFW.setInputMode ~window:win ~mode:GLFW.Cursor ~value:GLFW.Disabled; (*Captures the cursor*)
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
    val mutable _targetTickTime = 0.01 (*100 ticks per second*)
    val mutable _targetFrameTime = 0.01666667 (*60 FPS*)

    val mutable _lastTickTimestamp = 0.0
    val mutable _tickTime = 0.0
    val mutable _lastFrameTimestamp = 0.0
    val mutable _frameTime = 0.0
    
    (*The cursor is considered as initially centered*)
    val mutable _lastCursorX = float_of_int (width/2)
    val mutable _lastCursorY = float_of_int (height/2)
    val mutable _isCursorShown = false
    
    method processInput () =
      let rec process l = match l with
        | [] -> ()
        | a::q -> a#process ~window:_glfwWindow; process q
      in process _inputHandlers;
      let cursorMode = GLFW.getInputMode ~window:_glfwWindow ~mode:GLFW.Cursor in
      if (cursorMode = GLFW.Hidden || cursorMode = GLFW.Normal)  && GLFW.getMouseButton ~window:_glfwWindow ~button:GLFW.mouse_button_left then self#hideCursor ()
    
    (*The float argument is for the aspect ratio of the window*)
    method registerRenderCallback (render : (float -> unit -> unit)) =
      _renderCallback <- Some render
    
    (*The first float argument is for current time, the second for elapsed time since last tick*)
    method registerTickCallback (tick : (float -> float -> unit -> unit)) =
      _tickCallback <- Some tick

    method registerInputHandler (inputHandler : inputHandler) =
      _inputHandlers <- (inputHandler::_inputHandlers)
    
    (*The two float arguments correspond to the displacement of the cursor in x and y*)
    method registerMouseCallback (mouseCallback : (float -> float -> unit)) =
      let glfwCallback glfwWindow xPos yPos = 
        let xOffset = xPos -. _lastCursorX in
        let yOffset = yPos -. _lastCursorY in
        mouseCallback xOffset yOffset;
        _lastCursorX <- xPos;
        _lastCursorY <- yPos in
      let _ = GLFW.setCursorPosCallback ~window:_glfwWindow ~f:(Some glfwCallback) in ()

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
    
    method setFPS fps =
      _targetFrameTime <- (1./.(float_of_int fps))
    
    method setTPS tps =
      _targetTickTime <- (1./.(float_of_int tps))
    
    method hideCursor () =
      GLFW.setInputMode ~window:_glfwWindow ~mode:GLFW.Cursor ~value:GLFW.Disabled;
      _isCursorShown <- false
    
    method showCursor () =
      GLFW.setInputMode ~window:_glfwWindow ~mode:GLFW.Cursor ~value:GLFW.Normal;
      _isCursorShown <- true
    
    method isCursorShown =
      _isCursorShown

    method render () =
      let timestamp = GLFW.getTime () in
      if (timestamp -. _lastFrameTimestamp >= _targetFrameTime) then (
        _frameTime <- GLFW.getTime () -. _lastFrameTimestamp;
        _lastFrameTimestamp <- timestamp;
        Gl.clear Gl.color_buffer_bit;
        Gl.clear Gl.depth_buffer_bit;
        match _renderCallback with
          | None -> ();
          | Some render -> render !_aspectRatio ();
        GLFW.swapBuffers ~window:_glfwWindow;
      )

    method tick () =
      let timestamp = GLFW.getTime () in
      if (timestamp -. _lastTickTimestamp >= _targetTickTime) then (
        _tickTime <- timestamp -. _lastTickTimestamp;
        _lastTickTimestamp <- timestamp;
        GLFW.pollEvents ();
        self#processInput ();
        match _tickCallback with
          | None -> ()
          | Some tick -> tick timestamp _tickTime ();
        if GLFW.windowShouldClose ~window:_glfwWindow then _shouldClose <- true)
    
    method delete () = 
        GLFW.destroyWindow ~window:_glfwWindow;
        GLFW.terminate ()
  end