open Tgl3
open Logger

class input_handler ~(key : GLFW.key) ~(press_callback : (unit -> unit)) ~(release_callback : (unit -> unit)) =
  object (self)
    val _key = key
    val _press_callback = press_callback
    val _release_callback = release_callback
    val mutable _is_pressed = false
    method process ~(window : GLFW.window) =
      if GLFW.getKey ~window ~key:_key
        then (_press_callback (); _is_pressed <- true)
        else if _is_pressed
          then (_release_callback (); _is_pressed <- false)
  end

class window ~width ~height ~name =

  let aspect_ratio = ref (float_of_int width/.float_of_int height) in
  (*Callback function for window resizing event*)
  let on_window_resize aspect_ratio (window : GLFW.window) new_width new_height =
    logger Debug "window: The window has been resized.";
    aspect_ratio := (float_of_int new_width/.float_of_int new_height);
    Gl.viewport 0 0 new_width new_height in
  (*Creation of a window*)
  let glfw_window =
    GLFW.init ();
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
    GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
    let win = GLFW.createWindow ~width ~height ~title:name () in
    GLFW.makeContextCurrent ~window:(Some win);
    GLFW.swapInterval ~interval:0; (*Disables V-Sync*)
    GLFW.setInputMode ~window:win ~mode:GLFW.Cursor ~value:GLFW.Disabled; (*Captures the cursor*)
    logger Info "window: Successfully initialized GLFW and created a window.";
    let w,h = GLFW.getFramebufferSize ~window:win in
    Gl.viewport 0 0 w h;
    let _ = GLFW.setFramebufferSizeCallback ~window:win ~f:(Some (on_window_resize aspect_ratio)) in
    Gl.clear_color 0.0 0.0 0.0 1.0;
    Gl.enable Gl.depth_test;
    win in

  object (self)
    val _glfw_window = glfw_window
    val _aspect_ratio = aspect_ratio
    val mutable _render_callback = None
    val mutable _tick_callback = None
    val mutable _animation_tick_callback = None
    val mutable _should_close = false
    val mutable _input_handlers : (input_handler list) = []
    val mutable _target_tick_time = 0.05 (*20 ticks per second*) (*fixed-rate low-speed updates: for physical / game logic updates*)
    val mutable _target_animation_tick_time = 0.01666667 (*60 animation ticks per second*) (*animation tick: for fixed-rate high-speed updates, typically animations / camera movement etc*) 
    val mutable _target_frame_time = 0.01666667 (*60 FPS*)

    val mutable _last_tick_timestamp = 0.0
    val mutable _tick_time = 0.0
    val mutable _last_animation_tick_timestamp = 0.0
    val mutable _animation_tick_time = 0.0
    val mutable _last_frame_timestamp = 0.0
    val mutable _frame_time = 0.0
    
    (*The cursor is considered as initially centered*)
    val mutable _last_cursor_x = float_of_int (width/2)
    val mutable _last_cursor_y = float_of_int (height/2)
    val mutable _is_cursor_shown = false
    
    method process_input () =
      let rec process l = match l with
        | [] -> ()
        | a::q -> a#process ~window:_glfw_window; process q
      in process _input_handlers;
      let cursor_mode = GLFW.getInputMode ~window:_glfw_window ~mode:GLFW.Cursor in
      if (cursor_mode = GLFW.Hidden || cursor_mode = GLFW.Normal)  && GLFW.getMouseButton ~window:_glfw_window ~button:GLFW.mouse_button_left then self#hide_cursor ()
    
    (*The float argument is for the aspect ratio of the window*)
    method register_render_callback (render : (float -> unit -> unit)) =
      _render_callback <- Some render
    
    (*The first float argument is for current time, the second for elapsed time since last tick*)
    method register_tick_callback (tick : (float -> float -> unit -> unit)) =
      _tick_callback <- Some tick
    
    (*The first float argument is for current time, the second for elapsed time since last tick*)
    method register_animation_tick_callback (animation_tick : (float -> float -> unit -> unit)) =
      _animation_tick_callback <- Some animation_tick

    method register_input_handler (input_handler : input_handler) =
      _input_handlers <- (input_handler::_input_handlers)
    
    (*The two float arguments correspond to the displacement of the cursor in x and y*)
    method register_mouse_callback (mouse_callback : (float -> float -> unit)) =
      let glfw_callback glfw_window x_pos y_pos = 
        let x_offset = x_pos -. _last_cursor_x in
        let y_offset = y_pos -. _last_cursor_y in
        mouse_callback x_offset y_offset;
        _last_cursor_x <- x_pos;
        _last_cursor_y <- y_pos in
      let _ = GLFW.setCursorPosCallback ~window:_glfw_window ~f:(Some glfw_callback) in ()

    method get_input_handlers =
      _input_handlers
    
    method set_should_close b =
      _should_close <- b

    method should_close =
      _should_close
    
    method get_frame_time =
      _frame_time
    
    method get_tick_time =
      _tick_time
    
    method get_animation_tick_time =
      _animation_tick_time

    method set_fps fps =
      _target_frame_time <- (1./.(float_of_int fps))
    
    method set_tps tps =
      _target_tick_time <- (1./.(float_of_int tps))

    method set_animation_tps tps =
      _target_animation_tick_time <- (1./.(float_of_int tps))
    
    method hide_cursor () =
      GLFW.setInputMode ~window:_glfw_window ~mode:GLFW.Cursor ~value:GLFW.Disabled;
      _is_cursor_shown <- false
    
    method show_cursor () =
      GLFW.setInputMode ~window:_glfw_window ~mode:GLFW.Cursor ~value:GLFW.Normal;
      _is_cursor_shown <- true
    
    method is_cursor_shown =
      _is_cursor_shown

    method render () =
      let timestamp = GLFW.getTime () in
      if (timestamp -. _last_frame_timestamp >= _target_frame_time) then (
        _frame_time <- GLFW.getTime () -. _last_frame_timestamp;
        _last_frame_timestamp <- timestamp;
        Gl.clear Gl.color_buffer_bit;
        Gl.clear Gl.depth_buffer_bit;
        match _render_callback with
          | None -> ();
          | Some render -> render !_aspect_ratio ();
        GLFW.swapBuffers ~window:_glfw_window;
      )

    method tick () =
      let timestamp = GLFW.getTime () in
      if (timestamp -. _last_tick_timestamp >= _target_tick_time) then (
        _tick_time <- timestamp -. _last_tick_timestamp;
        _last_tick_timestamp <- timestamp;
        match _tick_callback with
          | None -> ()
          | Some tick -> tick timestamp _tick_time ())
    
    method animation_tick () =
      let timestamp = GLFW.getTime () in
      if (timestamp -. _last_animation_tick_timestamp >= _target_animation_tick_time) then (
        _animation_tick_time <- timestamp -. _last_animation_tick_timestamp;
        _last_animation_tick_timestamp <- timestamp;
        GLFW.pollEvents ();
        self#process_input ();
        match _animation_tick_callback with
          | None -> ()
          | Some animation_tick -> animation_tick timestamp _animation_tick_time ();
        if GLFW.windowShouldClose ~window:_glfw_window then _should_close <- true)
    
    method delete () = 
        GLFW.destroyWindow ~window:_glfw_window;
        GLFW.terminate ()
  end