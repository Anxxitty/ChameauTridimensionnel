open Tgl3
open Bigarray
open ChameauTridimensionnel
open Logger
open Vector
open Asset
open Window

(*Main function*)
let () =
  (*Initialize logger*)
  let date = Unix.localtime (Unix.time ()) in
  initLogger ~enableLogToFile:false ~enableDebug:true ~enableDebugMainLoop:false ~logFilePath:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");
  
  (*Create a window*)
  logger Debug "Main: Initializing a window.";
  let window = new window ~width:1280 ~height:720 ~name:"WombatCombat" in

  (*Cube geometry*)
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

  (*Basic shaders*)
  logger Debug "Main: Compiling and linking shader program for cube.";
  let vertexShader = new Shader.shader ~shaderType:Gl.vertex_shader ~shaderSourcePath:"assets/shaders/vertex.glsl" in
  let fragmentShader = new Shader.shader ~shaderType:Gl.fragment_shader ~shaderSourcePath:"assets/shaders/fragment.glsl" in
  let shaderProgram = new Shader.shaderProgram ~vertexShader ~fragmentShader in

  logger Debug "Main: Creating cube drawables.";
  let cube = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-.0.5);y=(-.0.5);z=(-.0.5)} ~localRotation:identityMatrix ~scale:{x=1.0;y=1.0;z=1.0} in
  let cube2 = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-.1.5);y=(-.1.5);z=(-.0.5)} ~localRotation:identityMatrix ~scale:{x=1.0;y=1.0;z=1.0} in
  let cube3 = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-.7.);y=(-.5.);z=(-.2.)} ~localRotation:identityMatrix ~scale:{x=10.0;y=0.1;z=10.0} in

  let cameraPosition = ref {x=0.0; y=0.0; z=(-.15.0)} in

  (*Main render function*)
  let render aspectRatio () =
    logger DebugMainLoop "Main: Moving drawables.";
    cube#setCoordinates { x=(sin ((GLFW.getTime ()))); y=(cos ((GLFW.getTime ()))); z=0.0 };
    cube#rotate ~axis:{x=0.5;y=0.5;z=0.2} ~angle:0.01;
    let projMat = projectionMatrix 45.0 aspectRatio 1.0 100.0 in
    cube#render ~window ~uniforms:[MatrixUniform4f ("cameraTranslation", (translationMatrix !cameraPosition));MatrixUniform4f ("projectionMatrix", projMat)];
    cube2#render ~window ~uniforms:[MatrixUniform4f ("cameraTranslation", (translationMatrix !cameraPosition));MatrixUniform4f ("projectionMatrix", projMat)];
    cube3#render ~window ~uniforms:[MatrixUniform4f ("cameraTranslation", (translationMatrix !cameraPosition));MatrixUniform4f ("projectionMatrix", projMat)] in
  
  let tick time () =
    () in

  (*Setup window and inputs*)
  window#registerRenderCallback render;
  window#registerTickCallback tick;
  window#registerInputHandler (new inputHandler ~key:GLFW.Escape ~callback:(fun () -> window#setShouldClose true));
  window#registerInputHandler (new inputHandler ~key:GLFW.W ~callback:(fun () -> cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y); z=(!cameraPosition.z +. 0.1)}));
  window#registerInputHandler (new inputHandler ~key:GLFW.S ~callback:(fun () -> cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y); z=(!cameraPosition.z -. 0.1)}));
  window#registerInputHandler (new inputHandler ~key:GLFW.A ~callback:(fun () -> cameraPosition := {x=(!cameraPosition.x +. 0.1); y=(!cameraPosition.y); z=(!cameraPosition.z)}));
  window#registerInputHandler (new inputHandler ~key:GLFW.D ~callback:(fun () -> cameraPosition := {x=(!cameraPosition.x -. 0.1); y=(!cameraPosition.y); z=(!cameraPosition.z)}));
  window#registerInputHandler (new inputHandler ~key:GLFW.LeftShift ~callback:(fun () -> cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y +. 0.1); z=(!cameraPosition.z)}));
  window#registerInputHandler (new inputHandler ~key:GLFW.Space ~callback:(fun () -> cameraPosition := {x=(!cameraPosition.x); y=(!cameraPosition.y -. 0.1); z=(!cameraPosition.z)}));


  (*Main loop*)
  let rec loop () =
    logger DebugMainLoop "Main: Rendering.";
    window#render ();
    logger DebugMainLoop ("Main: FPS="^(string_of_float(1./.window#getFrameTime)));
    logger DebugMainLoop "Main: Tick.";
    window#tick ();
    logger DebugMainLoop ("Main: TPS="^(string_of_float(1./.window#getTickTime)));
    if window#shouldClose
      then (
        logger Debug "Exited the main loop, closing.";
        window#delete ();
        shaderProgram#delete ();
        vao#delete ();
        ebo#delete ();
        logger Info "Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
