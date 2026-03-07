open Tgl3
open Bigarray
open ChameauTridimensionnel
open Logger
open Vector
open Asset

(*Callback function for window resizing event*)
let onWindowResize resizeFlag (window : GLFW.window) newWidth newHeight =
  logger Debug "Main: The window has been resized.";
  resizeFlag := true;
  Gl.viewport 0 0 newWidth newHeight

(*Initialization of a window*)
let createWindow ~width ~height ~name ~resizeFlag =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
  let window = GLFW.createWindow ~width ~height ~title:name () in
  GLFW.makeContextCurrent ~window:(Some window);
  logger Info "Successfully initialized GLFW and created a window.";
  Gl.viewport 0 0 width height;
  let _ = GLFW.setFramebufferSizeCallback ~window ~f:(Some (onWindowResize resizeFlag)) in
  Gl.clear_color 0.0 0.0 0.0 1.0;
  Gl.enable Gl.depth_test;
  window

(*Processes keyboard and mouse input*)
let processInput ~window ~cameraPosition =
  if GLFW.getKey ~window ~key:GLFW.Escape then GLFW.setWindowShouldClose ~window ~b:true;
  if GLFW.getKey ~window ~key:GLFW.W then (cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y); z=(!cameraPosition.z +. 0.1)});
  if GLFW.getKey ~window ~key:GLFW.S then (cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y); z=(!cameraPosition.z -. 0.1)});
  if GLFW.getKey ~window ~key:GLFW.A then (cameraPosition := {x=(!cameraPosition.x +. 0.1); y=(!cameraPosition.y); z=(!cameraPosition.z)});
  if GLFW.getKey ~window ~key:GLFW.D then (cameraPosition := {x=(!cameraPosition.x -. 0.1); y=(!cameraPosition.y); z=(!cameraPosition.z)});
  if GLFW.getKey ~window ~key:GLFW.LeftShift then (cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y +. 0.1); z=(!cameraPosition.z)});
  if GLFW.getKey ~window ~key:GLFW.Space then (cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y -. 0.1); z=(!cameraPosition.z)})


(*Main function*)
let () =
  let date = Unix.localtime (Unix.time ()) in
  initLogger ~enableLogToFile:false ~enableDebug:true ~enableDebugMainLoop:false ~logFilePath:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");
  
  logger Debug "Main: Initializing a window.";
  let resizeFlag = ref false in
  let window = createWindow ~width:1280 ~height:720 ~name:"WombatCombat" ~resizeFlag in

  let positions = [
    Vector3 {x=0.0; y=0.0; z=0.0};
    Vector3 {x=1.0; y=0.0; z=0.0};
    Vector3 {x=1.0; y=0.0; z=1.0};
    Vector3 {x=0.0; y=0.0; z=1.0};
    Vector3 {x=0.0; y=1.0; z=0.0};
    Vector3 {x=1.0; y=1.0; z=0.0};
    Vector3 {x=1.0; y=1.0; z=1.0};
    Vector3 {x=0.0; y=1.0; z=1.0};
  ] in
  let colors = [
    Vector4 {r=1.0; g=0.0; b=0.0; a=1.0};
    Vector4 {r=0.0; g=1.0; b=0.0; a=1.0};
    Vector4 {r=0.0; g=0.0; b=1.0; a=1.0};
    Vector4 {r=1.0; g=1.0; b=0.0; a=1.0};
    Vector4 {r=1.0; g=0.0; b=1.0; a=1.0};
    Vector4 {r=0.0; g=1.0; b=1.0; a=1.0};
    Vector4 {r=0.0; g=1.0; b=0.0; a=1.0};
    Vector4 {r=1.0; g=0.0; b=1.0; a=1.0};
  ] in
  logger Debug "Main: Generating vertex attributes for cube.";
  let positionsVertexAttrib = new vertexAttribute ~attributeType:(Vector3Kind Float32) ~numberOfVertices:8 ~vectorList:positions () in
  let colorsVertexAttrib = new vertexAttribute ~attributeType:(Vector4Kind Float32) ~numberOfVertices:8 ~vectorList:colors () in

  logger Debug "Main: Generating EBO for cube.";
  let indices = [0;1;2;0;2;3;0;1;5;0;5;4;0;3;7;0;7;4;4;5;6;4;6;7;3;7;6;3;6;2;1;2;6;1;6;5] in
  let ebo = new buffer ~bufferType:Gl.element_array_buffer ~kind:Int8_unsigned in
  ebo#writeElementBuffer ~indices ~usage:Gl.static_draw;

  logger Debug "Main: Generating vertexArray for cube.";
  let vao = new vertexArray ~kind:Float32 ~vertexAttributes:[positionsVertexAttrib; colorsVertexAttrib] ~elementBuffer:ebo ~drawingType:Gl.static_draw in

  logger Debug "Main: Compiling and linking shader program for cube.";
  let vertexShader = new Shader.shader ~shaderType:Gl.vertex_shader ~shaderSourcePath:"assets/shaders/vertex.glsl" in
  let fragmentShader = new Shader.shader ~shaderType:Gl.fragment_shader ~shaderSourcePath:"assets/shaders/fragment.glsl" in
  let shaderProgram = new Shader.shaderProgram ~vertexShader ~fragmentShader in

  logger Debug "Main: Creating cube drawables.";
  let cube = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-.0.5);y=(-.0.5);z=(-.0.5)} ~localRotation:identityMatrix ~scale:{x=1.0;y=1.0;z=1.0} in
  
  let cube2 = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-.1.5);y=(-.1.5);z=(-.0.5)} ~localRotation:identityMatrix ~scale:{x=1.0;y=1.0;z=1.0} in

  let cameraPosition = ref {x=0.0; y=0.0; z=(-.15.0)} in

  let projMat = ref (projectionMatrix 45.0 (16.0/.9.0) 0.0 1000.0) in
  
  (*Main loop*)
  let rec loop () =
    Gl.clear Gl.color_buffer_bit;
    Gl.clear Gl.depth_buffer_bit;
    logger DebugMainLoop "Main: Processing input.";
    processInput ~window ~cameraPosition;
    if !resizeFlag then
      resizeFlag := false;
      let (width, height) = GLFW.getWindowSize ~window in
      projMat := projectionMatrix 45.0 (float_of_int width/.float_of_int height) 1.0 100.0;
    logger DebugMainLoop "Main: Moving drawables.";
    cube#setCoordinates { x=(sin ((GLFW.getTime ()))); y=(cos ((GLFW.getTime ()))); z=0.0 };
    cube#rotate ~axis:{x=0.5;y=0.5;z=0.2} ~angle:0.01;
    logger DebugMainLoop "Main: Rendering.";
    cube#render ~window ~uniforms:[MatrixUniform4f ("cameraTranslation", (translationMatrix !cameraPosition));MatrixUniform4f ("projectionMatrix", !projMat)];
    cube2#render ~window ~uniforms:[MatrixUniform4f ("cameraTranslation", (translationMatrix !cameraPosition));MatrixUniform4f ("projectionMatrix", !projMat)];
    GLFW.swapBuffers ~window;
    GLFW.pollEvents ();
    if GLFW.windowShouldClose ~window
      then (
        logger Debug "Exited the main loop, closing.";
        shaderProgram#delete ();
        vao#delete ();
        ebo#delete ();
        GLFW.destroyWindow ~window;
        GLFW.terminate ();
        logger Info "Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
