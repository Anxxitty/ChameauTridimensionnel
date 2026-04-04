open Tgl3
open Bigarray
open ChameauTridimensionnel
open Bigarray_helper
open Logger
open Math
open Graphics
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

  (*Wireframe mode*)(*
  Gl.polygon_mode Gl.front_and_back Gl.line;*)

  (*Testing the basic .obj model loader*)
  let cube_model = new model ~path:"assets/models/cube.obj" ~with_tex_and_normals:true in
  let cat_model = new model ~path:"assets/models/cat.obj" ~with_tex_and_normals:true in
  let tie_model = new model ~path:"assets/models/tie.obj" ~with_tex_and_normals:false in
  let dark_vador_tie_model = new model ~path:"assets/models/darkvadortie.obj" ~with_tex_and_normals:false in
  let hexagon_model = new model ~path:"assets/models/hexagon.obj" ~with_tex_and_normals:true in
  let table_model = new model ~path:"assets/models/table.obj" ~with_tex_and_normals:false in
  let torus_model = new model ~path:"assets/models/torus.obj" ~with_tex_and_normals:true in
  let car_model = new model ~path:"assets/models/car.obj" ~with_tex_and_normals:true in
  let dirac_model = new model ~path:"assets/models/dirac.obj" ~with_tex_and_normals:false in

  let car_texture = new texture ~path:"assets/textures/car_exterior.jpeg" in
  let brick_texture = new texture ~path:"assets/textures/brick.png" in

  logger Debug "Main: Compiling and linking shader program for textured models.";
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex_tex.glsl" in
  let fragment_shader1 = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment_tex.glsl" in
  let fragment_shader2 = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment_tex2.glsl" in
  let tex_shader = new shader_program ~vertex_shader ~fragment_shader:fragment_shader1 in
  let tex_shader2 = new shader_program ~vertex_shader ~fragment_shader:fragment_shader2 in

  let tex_cube_drawable = new textured_drawable ~vertex_array:cube_model#get_vao ~shader_program:tex_shader2 ~textures:[brick_texture;car_texture] ~number_of_drawn_vertices:cube_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=10.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=5.0;y=5.0;z=5.0} in
  let car_drawable = new textured_drawable ~vertex_array:car_model#get_vao ~shader_program:tex_shader ~textures:[car_texture] ~number_of_drawn_vertices:car_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=10.0;z=(-15.0)} ~local_rotation:identity_quat ~scale:{x=(0.1);y=(0.1);z=(0.1)} in

  (*Compiling shaders*)
  logger Debug "Main: Compiling and linking shader program for untextured models.";
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex2.glsl" in
  let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment.glsl" in
  let shader_program = new shader_program ~vertex_shader ~fragment_shader in

  let cube_drawable = new drawable ~vertex_array:cube_model#get_vao ~shader_program ~number_of_drawn_vertices:cube_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=(-5.0)} ~local_rotation:identity_quat ~scale:{x=1.5;y=1.5;z=1.5} in
  let cat_initial_rot = rotation_quat ~axis:{x=1.0;y=0.0;z=0.0} ~angle:(rad_of_deg (-90.0)) in
  let cat_drawable = new drawable ~vertex_array:cat_model#get_vao ~shader_program:tex_shader ~number_of_drawn_vertices:cat_model#get_number_of_drawn_vertices ~scene_coordinates:{x=(-16.0);y=5.0;z=0.0} ~local_rotation:cat_initial_rot ~scale:{x=0.25;y=0.25;z=0.25} in
  let tie_drawable = new drawable ~vertex_array:tie_model#get_vao ~shader_program ~number_of_drawn_vertices:tie_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=0.1;y=0.1;z=0.1} in
  let dark_vador_tie_drawable = new drawable ~vertex_array:dark_vador_tie_model#get_vao ~shader_program ~number_of_drawn_vertices:dark_vador_tie_model#get_number_of_drawn_vertices ~scene_coordinates:{x=12.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=0.05;y=0.05;z=0.05} in
  let hexagon_drawable = new drawable ~vertex_array:hexagon_model#get_vao ~shader_program:tex_shader ~number_of_drawn_vertices:hexagon_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=(-20.0)} ~local_rotation:identity_quat ~scale:{x=0.5;y=0.5;z=0.5} in
  let table_drawable = new drawable ~vertex_array:table_model#get_vao ~shader_program ~number_of_drawn_vertices:table_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=(-16.0);z=0.0} ~local_rotation:identity_quat ~scale:{x=0.25;y=0.25;z=0.25} in
  let torus_drawable = new drawable ~vertex_array:torus_model#get_vao ~shader_program:tex_shader ~number_of_drawn_vertices:torus_model#get_number_of_drawn_vertices ~scene_coordinates:{x=0.0;y=3.0;z=12.0} ~local_rotation:identity_quat ~scale:{x=3.0;y=3.0;z=3.0} in
  let dirac_drawable = new drawable ~vertex_array:dirac_model#get_vao ~shader_program ~number_of_drawn_vertices:dirac_model#get_number_of_drawn_vertices ~local_rotation:identity_quat ~scene_coordinates:{x=30.0;y=5.0;z=(-50.0)} ~scale:{x=1.0;y=1.0;z=1.0} in

  let camera = new fly_camera ~scene_coordinates:{x=0.0;y=10.0;z=(15.0)} ~fov:(rad_of_deg 45.0) ~near_plane:1.0 ~far_plane:1000.0 in

  let in_tie = ref false in
  let speed = ref 5.0 in

  (*Main render function*)
  let render aspect_ratio () =
    shader_program#set_uniform1f ~name:"time" ~value:{x=GLFW.getTime()};
    tex_shader#set_uniform1f ~name:"time" ~value:{x=GLFW.getTime()};
    let view_matrix = camera#gen_view_matrix in
    let proj_matrix = camera#gen_projection_matrix ~aspect_ratio in
    let uniforms = [Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)] in
    cube_drawable#render ~window ~uniforms;
    cat_drawable#render ~window ~uniforms;
    tie_drawable#render ~window ~uniforms;
    dark_vador_tie_drawable#render ~window ~uniforms;
    hexagon_drawable#render ~window ~uniforms;
    table_drawable#render ~window ~uniforms;
    torus_drawable#render ~window ~uniforms;
    tex_cube_drawable#render ~window ~uniforms;
    car_drawable#render ~window ~uniforms;
    dirac_drawable#render ~window ~uniforms
    in
  
  let animation_tick time elapsed_time () =
    logger Debug_main_loop "Main: Moving drawables.";
    let time = 2.*.time in 
    tie_drawable#get_coordinates.x <- 25.*.cos(time);
    tie_drawable#get_coordinates.z <- 25.*.sin(time);
    tie_drawable#set_rotation (rotation_quat ~axis:{x=0.0;y=1.0;z=0.0} ~angle:(-.time));
    tie_drawable#rotate ~quat:(rotation_quat ~axis:{x=sin(time);y=0.0;z=(-.cos(time))} ~angle:(time));
    cube_drawable#set_rotation (rotation_quat ~axis:{x=0.0;y=1.0;z=0.0} ~angle:(sin(time)));
    cat_drawable#rotate ~quat:(rotation_quat ~axis:{x=1.0;y=1.0;z=1.0} ~angle:0.01);
    if (!in_tie) then (
      let c = tie_drawable#get_coordinates in
      camera#set_coordinates {x=(c.x-.25.0*.sin(time)) ; y=c.y ; z=(c.z+.25.0*.cos(time))};
      camera#set_rotation (quat_of_vec4 (vec4_scalar_op ( *. ) (-1.0) (vec4_of_quat (tie_drawable#get_rotation))));
      camera#rotate ~quat:(rotation_quat ~axis:{x=0.0;y=1.0;z=0.0} ~angle:0.)
    ) else camera#tick ~elapsed_time in
  
  let tick time elapsed_time () = ()(*
    logger Info ("roll "^string_of_float (roll_of_rot_quat camera#get_rotation));
    logger Info ("yaw "^string_of_float (yaw_of_rot_quat camera#get_rotation));
    logger Info ("pitch "^string_of_float (pitch_of_rot_quat camera#get_rotation))*) in

  (*Setup window and inputs*)
  window#register_render_callback render;
  window#register_animation_tick_callback animation_tick;
  window#register_tick_callback tick;
  window#set_fps 60;
  window#set_tps 20;
  window#set_animation_tps 60;
  window#register_input_handler (new input_handler ~key:GLFW.Escape ~press_callback:(window#show_cursor) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.W ~press_callback:(fun () -> camera#get_velocity_vector.z <- (-.(!speed))) ~release_callback:(fun () -> camera#get_velocity_vector.z <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.S ~press_callback:(fun () -> camera#get_velocity_vector.z <-   !speed ) ~release_callback:(fun () -> camera#get_velocity_vector.z <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.A ~press_callback:(fun () -> camera#get_velocity_vector.x <- (-.(!speed))) ~release_callback:(fun () -> camera#get_velocity_vector.x <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.D ~press_callback:(fun () -> camera#get_velocity_vector.x <-   !speed ) ~release_callback:(fun () -> camera#get_velocity_vector.x <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.LeftShift ~press_callback:(fun () -> camera#get_velocity_vector.y <- (-.(!speed))) ~release_callback:(fun () -> camera#get_velocity_vector.y <- 0.0));
  window#register_input_handler (new input_handler ~key:GLFW.Space     ~press_callback:(fun () -> camera#get_velocity_vector.y <-   !speed ) ~release_callback:(fun () -> camera#get_velocity_vector.y <- 0.0));
  window#register_input_handler (new toggle_input_handler ~key:GLFW.F11 ~toggle_on_callback:(fun () -> window#enable_fullscreen ()) ~toggle_off_callback:(fun () -> window#disable_fullscreen ()));
  window#register_input_handler (new toggle_input_handler ~key:GLFW.T ~toggle_on_callback:(fun () -> in_tie := true) ~toggle_off_callback:(fun () -> in_tie := false));
  window#register_input_handler (new input_handler ~key:GLFW.LeftControl ~press_callback:(fun () -> speed := 10.0) ~release_callback:(fun () -> speed := 5.0));

  let mouse_sensitivity = 0.001 in

  let mouse_position_callback x_offset y_offset =
    if window#is_cursor_shown = false then (
      camera#rotate_pitch ~angle:(-.mouse_sensitivity*.y_offset);
      camera#rotate_yaw ~angle:(-.mouse_sensitivity*.x_offset)) in
  window#register_mouse_callback mouse_position_callback;

  let scroll_callback x_offset y_offset = camera#set_fov (camera#get_fov +. (0.01*.y_offset)) in
  window#register_scroll_callback scroll_callback;

  window#register_input_handler (new input_handler ~key:GLFW.Left ~press_callback:(fun () -> camera#rotate_roll ~angle:0.01) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.Right ~press_callback:(fun () -> camera#rotate_roll ~angle:(-0.01)) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.R ~press_callback:(fun () -> (
    let roll = roll_of_rot_quat camera#get_rotation in
    let camera_z = rotate_vec_with_quat z_axis camera#get_rotation in
    let roll_sign = dot3f camera_z z_axis in
    if roll_sign > 0. then camera#rotate_roll ~angle:(-.roll)
    else camera#rotate_roll ~angle:(pi+.roll)
  )) ~release_callback:(fun () -> ()));

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
        tex_shader#delete ();
        cat_model#delete ();
        cube_model#delete ();
        tie_model#delete ();
        dark_vador_tie_model#delete ();
        hexagon_model#delete ();
        table_model#delete ();
        torus_model#delete ();
        car_model#delete ();
        dirac_model#delete ();
        brick_texture#delete ();
        car_texture#delete ();
        logger Info "Main: Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
