open Tgl3
open Bigarray
open ChameauTridimensionnel
open Logger
open Math
open Asset
open Window
open Actor
open Shader
open Model

(*Main function*)
let () =

  (*Initialize logger*)
  let date = Unix.localtime (Unix.time ()) in
  init_logger ~enable_log_to_file:false ~enable_debug:true ~enable_debug_main_loop:false ~log_file_path:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");

  (*Create a window*)
  logger Debug "Main: Initializing a window.";
  let window = new window ~width:1280 ~height:720 ~name:"WombatCombat" in
  window#hide_cursor ();

  (*Wireframe mode*)
  Gl.polygon_mode Gl.front_and_back Gl.line;

  (*Testing the basic .obj model loader*)
  let cube_model = new model ~path:"assets/models/cube.obj" in
  let cat_model = new model ~path:"assets/models/cat.obj" in
  let tie_model = new model ~path:"assets/models/tie.obj" in
  let dark_vador_tie_model = new model ~path:"assets/models/darkvadortie.obj" in
  let hexagon_model = new model ~path:"assets/models/hexagon.obj" in
  let table_model = new model ~path:"assets/models/table.obj" in
  let torus_model = new model ~path:"assets/models/torus.obj" in

  (*Compiling shaders*)
  logger Debug "Main: Compiling and linking shader program for cube created by model_loader.";
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex2.glsl" in
  let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment.glsl" in
  let shader_program = new shader_program ~vertex_shader ~fragment_shader in

  let cube_drawable = new drawable ~vertex_array:cube_model#get_vao ~shader_program ~number_of_drawn_vertices:cube_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=1.5;y=1.5;z=1.5} in
  let cat_initial_rot = rotation_quat ~axis:{x=1.0;y=0.0;z=0.0} ~angle:(rad_of_deg (-90.0)) in
  let cat_drawable = new drawable ~vertex_array:cat_model#get_vao ~shader_program ~number_of_drawn_vertices:cat_model#get_number_of_drawn_vertices ~scene_coordinates:{x=(-12.0);y=3.0;z=0.0} ~local_rotation:cat_initial_rot ~scale:{x=0.25;y=0.25;z=0.25} in
  let tie_drawable = new drawable ~vertex_array:tie_model#get_vao ~shader_program ~number_of_drawn_vertices:tie_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=0.1;y=0.1;z=0.1} in
  let dark_vador_tie_drawable = new drawable ~vertex_array:dark_vador_tie_model#get_vao ~shader_program ~number_of_drawn_vertices:dark_vador_tie_model#get_number_of_drawn_vertices ~scene_coordinates:{x=12.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=0.05;y=0.05;z=0.05} in
  let hexagon_drawable = new drawable ~vertex_array:hexagon_model#get_vao ~shader_program ~number_of_drawn_vertices:hexagon_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=(-20.0)} ~local_rotation:identity_quat ~scale:{x=0.5;y=0.5;z=0.5} in
  let table_drawable = new drawable ~vertex_array:table_model#get_vao ~shader_program ~number_of_drawn_vertices:table_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=(-16.0);z=0.0} ~local_rotation:identity_quat ~scale:{x=0.25;y=0.25;z=0.25} in
  let torus_drawable = new drawable ~vertex_array:torus_model#get_vao ~shader_program ~number_of_drawn_vertices:torus_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=12.0} ~local_rotation:identity_quat ~scale:{x=3.0;y=3.0;z=3.0} in


  let camera = new camera ~scene_coordinates:{x=0.0;y=10.0;z=(15.0)} ~scale:{x=0.0;y=0.0;z=0.0} ~fov:(rad_of_deg 45.0) ~near_plane:1.0 ~far_plane:1000.0 in

  (*Main render function*)
  let render aspect_ratio () =
    let view_matrix = camera#gen_view_matrix in
    let proj_matrix = camera#gen_projection_matrix ~aspect_ratio in
    cube_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)];
    cat_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)];
    tie_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)];
    dark_vador_tie_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)];
    hexagon_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)];
    table_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)];
    torus_drawable#render ~window ~uniforms:[Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)] in
    
  let animation_tick time elapsed_time () =
    logger Debug_main_loop "Main: Moving drawables.";
    cube_drawable#set_rotation (rotation_quat ~axis:{x=0.0;y=1.0;z=0.0} ~angle:(sin(GLFW.getTime ())));
    cat_drawable#rotate ~quat:(rotation_quat ~axis:{x=1.0;y=1.0;z=1.0} ~angle:0.01);
    camera#tick ~elapsed_time in

  (*Setup window and inputs*)
  window#register_render_callback render;
  window#register_animation_tick_callback animation_tick;
  window#set_fps 40000;
  window#set_tps 20;
  window#set_animation_tps 60;
  window#register_input_handler (new input_handler ~key:GLFW.Escape ~press_callback:(window#show_cursor) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.W ~press_callback:(fun () -> camera#get_velocity_vector.z <-   5.0 ) ~release_callback:(fun () -> camera#get_velocity_vector.z <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.S ~press_callback:(fun () -> camera#get_velocity_vector.z <- (-5.0)) ~release_callback:(fun () -> camera#get_velocity_vector.z <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.A ~press_callback:(fun () -> camera#get_velocity_vector.x <-   5.0 ) ~release_callback:(fun () -> camera#get_velocity_vector.x <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.D ~press_callback:(fun () -> camera#get_velocity_vector.x <- (-5.0)) ~release_callback:(fun () -> camera#get_velocity_vector.x <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.LeftShift ~press_callback:(fun () -> camera#get_velocity_vector.y <- (-5.0)) ~release_callback:(fun () -> camera#get_velocity_vector.y <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.Space     ~press_callback:(fun () -> camera#get_velocity_vector.y <-   5.0 ) ~release_callback:(fun () -> camera#get_velocity_vector.y <- 0.0));

  let mouse_sensitivity = 0.001 in

  let mouse_position_callback x_offset y_offset =
    if window#is_cursor_shown = false then (
      camera#rotate_pitch ~angle:(-.mouse_sensitivity*.y_offset);
      camera#rotate_yaw ~angle:(-.mouse_sensitivity*.x_offset)) in
  window#register_mouse_callback mouse_position_callback;

  logger Info ("Main: "^string_of_vector3f (cross3f camera#get_coordinates {x=0.0;y=1.0;z=0.0}));

  (*Main loop*)
  let rec loop () =
    logger Debug_main_loop "Main: Rendering.";
    window#render ();
    logger Debug_main_loop ("Main: FPS="^(string_of_float(1./.window#get_frame_time)));
    logger Debug_main_loop "Main: Tick.";
    window#tick ();
    logger Debug_main_loop ("Main: TPS="^(string_of_float(1./.window#get_tick_time)));
    logger Debug_main_loop "Main: Animation tick.";
    window#animation_tick ();
    logger Debug_main_loop ("Main: Animation TPS="^(string_of_float(1./.window#get_animation_tick_time)));
    if window#should_close
      then (
        logger Debug "Main: Exited the main loop, closing.";
        window#delete ();
        shader_program#delete ();
        cat_model#delete ();
        cube_model#delete ();
        logger Info "Main: Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
