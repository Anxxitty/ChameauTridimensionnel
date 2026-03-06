open Tgl3
open Bigarray
open ChameauTridimensionnel
open Logger
open Vector
open Asset

(*Callback function for window resizing event*)
let onWindowResize (window : GLFW.window) newWidth newHeight =
  Gl.viewport 0 0 newWidth newHeight

(*Initialization of a window*)
let createWindow ~width ~height ~name =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
  let window = GLFW.createWindow ~width ~height ~title:name () in
  GLFW.makeContextCurrent ~window:(Some window);
  logger Info "Successfully initialized GLFW and created a window.";
  Gl.viewport 0 0 width height;
  let _ = GLFW.setFramebufferSizeCallback ~window ~f:(Some onWindowResize) in
  Gl.clear_color 0.0 0.0 0.0 1.0;
  Gl.enable Gl.depth_test;
  window

(*Processes keyboard and mouse input*)
let processInput ~window =
  if GLFW.getKey ~window ~key:GLFW.Escape then GLFW.setWindowShouldClose ~window ~b:true

(*Main render function*)
let render ~window ~vao ~numberOfVertices ~(shaderProgram : Shader.shaderProgram) =
  let t = GLFW.getTime () in
  let rotationMatrix1 = (
    {r=(sin t); g=(cos t);     b=0.0; a=0.0},
    {r=(cos t); g=(-.(sin t)); b=0.0; a=0.0},
    {r=0.0;     g=0.0;         b=1.0; a=0.0},
    {r=0.0;     g=0.0;         b=0.0; a=1.0}) in
  let theta = 0.5 in
  let rotationMatrix2 = (
    {r=(sin theta); g=0.0; b=(cos theta);     a=0.0},
    {r=0.0;         g=1.0; b=0.0;             a=0.0},
    {r=(cos theta); g=0.0; b=(-.(sin theta)); a=0.0},
    {r=0.0;         g=0.0; b=0.0;             a=1.0}) in
  Gl.bind_vertex_array vao;
  shaderProgram#setUniformMatrix4f ~name:"rotationMatrix1" ~value:rotationMatrix1;
  shaderProgram#setUniformMatrix4f ~name:"rotationMatrix2" ~value:rotationMatrix2;
  shaderProgram#use ();
  Gl.draw_arrays Gl.triangles 0 numberOfVertices
  

(*Main function*)
let () =
  let date = Unix.localtime (Unix.time ()) in
  initLogger ~enableLogToFile:false ~logFilePath:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");
  let window = createWindow ~width:1280 ~height:720 ~name:"WombatCombat" in

  let positions = [
    Vector3 {x=(-0.5); y=(-0.5); z=0.0};
    Vector3 {x=0.0; y=0.5; z=0.0};
    Vector3 {x=0.5; y=(-0.5); z=0.0};
  ] in
  let colors = [
    Vector4 {r=1.0; g=1.0; b=0.0; a=1.0};
    Vector4 {r=1.0; g=1.0; b=0.0; a=1.0};
    Vector4 {r=1.0; g=1.0; b=0.0; a=1.0};
  ] in
  let positionsVertexAttrib = new vertexAttribute ~attributeType:(Vector3Kind Float32) ~numberOfVertices:3 ~vectorList:positions () in
  let colorsVertexAttrib = new vertexAttribute ~attributeType:(Vector4Kind Float32) ~numberOfVertices:3 ~vectorList:colors () in

  let vbo2 = new buffer ~bufferType:Gl.array_buffer in
  vbo2#writeVertexAttributes ~vertexAttributes:[positionsVertexAttrib; colorsVertexAttrib] ~usage:Gl.static_draw;

  let vao2 = getFirstInt (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao2;
  Gl.vertex_attrib_pointer 0 positionsVertexAttrib#getDimension Gl.float false (7*kind_size_in_bytes Float32) Gl.(`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 colorsVertexAttrib#getDimension Gl.float false (7*kind_size_in_bytes Float32) Gl.(`Offset (3*kind_size_in_bytes Float32));
  Gl.enable_vertex_attrib_array 1;

  let vertexData = createBigarray Float32 7 12 in
  setVector7 vertexData 0 (-0.5)  (0.0)   0.5    1.0   0.0   0.0    1.0;
  setVector7 vertexData 1   0.0    0.0  (-0.5)   0.0   1.0   0.0    1.0;
  setVector7 vertexData 2   0.0    1.0    0.0    0.0   0.0   1.0    1.0;
  setVector7 vertexData 3 (-0.5)   0.0    0.5    1.0   0.0   0.0    1.0;
  setVector7 vertexData 4   0.0    1.0    0.0    0.0   0.0   1.0    1.0;
  setVector7 vertexData 5   0.5    0.0    0.5    0.0   1.0   1.0    1.0;
  setVector7 vertexData 6   0.5    0.0    0.5    0.0   1.0   1.0    1.0;
  setVector7 vertexData 7   0.0    1.0    0.0    0.0   0.0   1.0    1.0;
  setVector7 vertexData 8   0.0    0.0  (-0.5)   0.0   1.0   0.0    1.0;
  setVector7 vertexData 9 (-0.5)   0.0    0.5    1.0   0.0   0.0    1.0;
  setVector7 vertexData 10  0.0    0.0  (-0.5)   0.0   1.0   0.0    1.0;
  setVector7 vertexData 11  0.5    0.0    0.5    0.0   1.0   1.0    1.0;
  let vbo = getFirstInt (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertexData) (Some vertexData) Gl.static_draw;
  let vao = getFirstInt (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vao;
  Gl.vertex_attrib_pointer 0 3 Gl.float false (7*kind_size_in_bytes Float32) Gl.(`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 4 Gl.float false (7*kind_size_in_bytes Float32) Gl.(`Offset (3*kind_size_in_bytes Float32));
  Gl.enable_vertex_attrib_array 1;

  let vertexShader = new Shader.shader ~shaderType:Gl.vertex_shader ~shaderSourcePath:"assets/shaders/vertex.glsl" in
  let fragmentShader = new Shader.shader ~shaderType:Gl.fragment_shader ~shaderSourcePath:"assets/shaders/fragment.glsl" in
  let shaderProgram = new Shader.shaderProgram ~vertexShader ~fragmentShader in

  let translationMatrix = (
    { r=1.0; g=0.0; b=0.0;     a=0.0 },
    { r=0.0; g=1.0; b=0.0;     a=0.0 },
    { r=0.0; g=0.0; b=1.0;     a=0.0 },
    { r=0.0; g=0.0; b=(-15.0); a=1.0 }) in

  let scaleMatrix = (
    { r=5.0; g=0.0; b=0.0; a=0.0 },
    { r=0.0; g=5.0; b=0.0; a=0.0 },
    { r=0.0; g=0.0; b=5.0; a=0.0 },
    { r=0.0; g=0.0; b=0.0; a=1.0 }) in

  let projectionMatrix = (
    { r=1.540976 ; g=0.0 ;      b=0.0          ; a = 0.0   },
    { r=0.0      ; g=2.739512 ; b=0.0          ; a = 0.0   },
    { r=0.0      ; g=0.0 ;      b=(-1.020202)  ; a = (-1.0)},
    { r=0.0      ; g=0.0 ;      b=(-20.202021) ; a = 0.0   }) in

  shaderProgram#setUniformMatrix4f ~name:"translationMatrix" ~value:translationMatrix;
  shaderProgram#setUniformMatrix4f ~name:"scaleMatrix" ~value:scaleMatrix;
  shaderProgram#setUniformMatrix4f ~name:"projectionMatrix" ~value:projectionMatrix;

  (*Main loop*)
  let rec loop () =
    Gl.clear Gl.color_buffer_bit;
    Gl.clear Gl.depth_buffer_bit;
    processInput ~window;
    render ~window ~vao ~numberOfVertices:12 ~shaderProgram;
    render ~window ~vao:vao2 ~numberOfVertices:3 ~shaderProgram;
    GLFW.swapBuffers ~window;
    GLFW.pollEvents ();
    if GLFW.windowShouldClose ~window
    then (
      shaderProgram#deleteProgram ();
      Gl.delete_buffers 1 (setFirstInt vbo);
      Gl.delete_vertex_arrays 1 (setFirstInt vao);
      GLFW.destroyWindow ~window;
      GLFW.terminate ();
      logger Info "Successfully terminated GLFW. Exiting.")
    else
      loop ();
  in loop ()
