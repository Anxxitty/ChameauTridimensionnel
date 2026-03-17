open Tgl3
open Bigarray
open ChameauTridimensionnel
open Logger
open Vector
open Asset
open Window
open Actor
open Model

(*Main function*)
let () =

  (*Initialize logger*)
  let date = Unix.localtime (Unix.time ()) in
  initLogger ~enableLogToFile:false ~enableDebug:true ~enableDebugMainLoop:false ~logFilePath:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");

  (*Create a window*)
  logger Debug "Main: Initializing a window.";
  let window = new window ~width:1280 ~height:720 ~name:"WombatCombat" in
  window#hideCursor ();

  (*Wireframe mode*)
  Gl.polygon_mode Gl.front_and_back Gl.line;

  (*Testing the basic .obj model loader*)
  let cubeNbVert, cubeMesh, cubeIndices = loadObjModel "assets/models/cube.obj" in
  let catNbVert, catMesh, catIndices = loadObjModel "assets/models/darkvadortie.obj" in

  logger Info (string_of_int_list_capped catIndices 24);
  logger Info ((string_of_float catMesh.{0})^" "^(string_of_float catMesh.{1})^" "^(string_of_float catMesh.{2}));
  logger Info ((string_of_float catMesh.{3})^" "^(string_of_float catMesh.{4})^" "^(string_of_float catMesh.{5}));

  let catNumberOfDrawnVertices = List.length catIndices in
  let cubeNumberOfDrawnVertices = List.length cubeIndices in
  let positionsVAofCat = new vertexAttribute ~attributeType:(Vector4Kind Float32) ~numberOfVertices:catNbVert () in
  positionsVAofCat#setRawData catMesh;
  let catEBO = new buffer ~bufferType:Gl.element_array_buffer ~kind:Int16_unsigned in
  catEBO#writeElementBuffer ~indices:catIndices ~usage:Gl.static_draw;

  logger Info (string_of_int catNumberOfDrawnVertices);
  logger Info (string_of_int catNbVert);

  let positionsVAofModel = new vertexAttribute ~attributeType:(Vector4Kind Float32) ~numberOfVertices:cubeNbVert () in
  positionsVAofModel#setRawData cubeMesh;
  let cubeEBO = new buffer ~bufferType:Gl.element_array_buffer ~kind:Int16_unsigned in
  cubeEBO#writeElementBuffer ~indices:cubeIndices ~usage:Gl.static_draw;


  let catVAO = new vertexArray ~kind:Float32 ~vertexAttributes:[positionsVAofCat] ~elementBuffer:catEBO ~drawingType:Gl.static_draw in
  let cubeVAO = new vertexArray ~kind:Float32 ~vertexAttributes:[positionsVAofModel] ~elementBuffer:cubeEBO ~drawingType:Gl.static_draw in

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

  (*Shaders for model loader test*)
  logger Debug "Main: Compiling and linking shader program for cube created by modelLoader.";
  let vertexShader2 = new Shader.shader ~shaderType:Gl.vertex_shader ~shaderSourcePath:"assets/shaders/vertex2.glsl" in
  let shaderProgram2 = new Shader.shaderProgram ~vertexShader:vertexShader2 ~fragmentShader in

  logger Debug "Main: Creating cube drawables.";
  let cube = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-0.5);y=(-0.5);z=(-0.5)} ~localRotation:identityMatrix ~scale:{x=1.0;y=1.0;z=1.0} in
  let cube2 = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-0.5);y=(-0.5);z=(-0.5)} ~localRotation:identityMatrix ~scale:{x=1.0;y=1.0;z=1.0} in
  let cube3 = new drawable ~vertexArray:vao ~shaderProgram ~numberOfDrawnVertices:36 ~sceneCoordinates:{x=(-5.);y=(-2.5);z=(-5.)} ~localRotation:identityMatrix ~scale:{x=10.0;y=0.1;z=10.0} in

  let cubeModel = new drawable ~vertexArray:cubeVAO ~shaderProgram:shaderProgram2 ~numberOfDrawnVertices:cubeNumberOfDrawnVertices ~sceneCoordinates:{x=0.0;y=4.0;z=0.0} ~localRotation:identityMatrix ~scale:{x=1.5;y=1.5;z=1.5} in
  let catInitialRot = rotationMatrix ~axis:{x=1.0;y=0.0;z=0.0} ~angle:(radiansOfDeg (0.0)) in
  let cat = new drawable ~vertexArray:catVAO ~shaderProgram:shaderProgram2 ~numberOfDrawnVertices:catNumberOfDrawnVertices ~sceneCoordinates:{x=(-8.0);y=(8.0);z=0.0} ~localRotation:catInitialRot ~scale:{x=0.05;y=0.05;z=0.05} in
  let cat2 = new drawable ~vertexArray:catVAO ~shaderProgram:shaderProgram2 ~numberOfDrawnVertices:catNumberOfDrawnVertices ~sceneCoordinates:{x=8.0;y=(8.0);z=0.0} ~localRotation:catInitialRot ~scale:{x=0.05;y=0.05;z=0.05} in

  let camera = new camera ~sceneCoordinates:{x=0.0;y=0.0;z=(15.0)} ~scale:{x=0.0;y=0.0;z=0.0} ~fov:(radiansOfDeg 45.0) ~nearPlane:1.0 ~farPlane:100.0 in

  (*Main render function*)
  let render aspectRatio () =
    logger DebugMainLoop "Main: Moving drawables.";
    cube#setCoordinates { x=(-0.5)+.(1.5*.sin ((GLFW.getTime ()))); y=(-0.5)+.(1.5*.cos ((GLFW.getTime ()))); z=(-0.5) };
    cube#setRotation (rotationMatrix ~axis:{x=0.5;y=0.5;z=0.2} ~angle:(sin(GLFW.getTime ())));
    cubeModel#setRotation (rotationMatrix ~axis:{x=0.0;y=1.0;z=0.0} ~angle:(sin(GLFW.getTime ())));
    cat#rotate ~axis:{x=0.0;y=0.0;z=1.0} ~angle:0.01;
    let viewMatrix = camera#genViewMatrix in
    let projMatrix = camera#genProjectionMatrix ~aspectRatio in
    cube#render ~window ~uniforms:[MatrixUniform4f ("viewMatrix", viewMatrix);MatrixUniform4f ("projectionMatrix", projMatrix)];
    cube2#render ~window ~uniforms:[MatrixUniform4f ("viewMatrix", viewMatrix);MatrixUniform4f ("projectionMatrix", projMatrix)];
    cube3#render ~window ~uniforms:[MatrixUniform4f ("viewMatrix", viewMatrix);MatrixUniform4f ("projectionMatrix", projMatrix)];
    cubeModel#render ~window ~uniforms:[MatrixUniform4f ("viewMatrix", viewMatrix);MatrixUniform4f ("projectionMatrix", projMatrix)];
    cat#render ~window ~uniforms:[MatrixUniform4f ("viewMatrix", viewMatrix);MatrixUniform4f ("projectionMatrix", projMatrix)];
    cat2#render ~window ~uniforms:[MatrixUniform4f ("viewMatrix", viewMatrix);MatrixUniform4f ("projectionMatrix", projMatrix)] in
  
  let tick time elapsedTime () =
    camera#tick ~elapsedTime in

  (*Setup window and inputs*)
  window#registerRenderCallback render;
  window#registerTickCallback tick;
  window#setFPS 120;
  window#setTPS 100;
  window#registerInputHandler (new inputHandler ~key:GLFW.Escape ~pressCallback:(window#showCursor) ~releaseCallback:(fun () -> ()));
  window#registerInputHandler (new inputHandler ~key:GLFW.W ~pressCallback:(fun () -> camera#setVelocityZ (-5.0)) ~releaseCallback:(fun () -> camera#setVelocityZ 0.0));
  window#registerInputHandler (new inputHandler ~key:GLFW.S ~pressCallback:(fun () -> camera#setVelocityZ   5.0 ) ~releaseCallback:(fun () -> camera#setVelocityZ 0.0));
  window#registerInputHandler (new inputHandler ~key:GLFW.A ~pressCallback:(fun () -> camera#setVelocityX (-5.0)) ~releaseCallback:(fun () -> camera#setVelocityX 0.0));
  window#registerInputHandler (new inputHandler ~key:GLFW.D ~pressCallback:(fun () -> camera#setVelocityX   5.0 ) ~releaseCallback:(fun () -> camera#setVelocityX 0.0));
  window#registerInputHandler (new inputHandler ~key:GLFW.LeftShift ~pressCallback:(fun () -> camera#setVelocityY (-5.0)) ~releaseCallback:(fun () -> camera#setVelocityY 0.0));
  window#registerInputHandler (new inputHandler ~key:GLFW.Space     ~pressCallback:(fun () -> camera#setVelocityY   5.0 ) ~releaseCallback:(fun () -> camera#setVelocityY 0.0));
 
  let mouseSensitivity = 0.001 in

  let mousePositionCallback xOffset yOffset =
    if window#isCursorShown = false then (
      camera#rotate ~axis:{x=1.0;y=0.0;z=0.0} ~angle:(-.mouseSensitivity*.yOffset);
      camera#rotate ~axis:{x=0.0;y=1.0;z=0.0} ~angle:(-.mouseSensitivity*.xOffset)) in
  window#registerMouseCallback mousePositionCallback;

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
        shaderProgram2#delete ();
        vao#delete ();
        ebo#delete ();
        cubeVAO#delete ();
        cubeEBO#delete ();
        catVAO#delete ();
        catEBO#delete ();
        logger Info "Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
