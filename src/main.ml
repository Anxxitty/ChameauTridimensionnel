open Tgl3
open Bigarray
open ChameauTridimensionnel
open Data_structures
open Logger
open Math
open Graphics
open Window
open Actor
open Shader
open Model
open Material
open Materials
open Defaults
open Scene_handler
open Scenes
open Initializer

(*Main function*)
let () =
  (*Initialize logger*)
  let date = Unix.localtime (Unix.time ()) in
  init_logger ~enable_log_to_file:false ~enable_debug:false ~enable_debug_main_loop:false ~log_file_path:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");

  (*Create a window*)
  logger Debug "Main: Initializing a window.";
  let window = new window ~width:1280 ~height:720 ~name:"WombatCombat" in
  initialize_defaults ();
  initialize_materials ();
  initialize_scenes ();
  window#hide_cursor ();

  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;

  (*Wireframe mode*)(*
  Gl.polygon_mode Gl.front_and_back Gl.line;*)

  let scene1 = Main_scene.scene#get in
  let scene2 = Test_scene.scene#get in

  let scener = new scene_handler ~initial_scene:scene1 in

  (*Main render function*)
  let render aspect_ratio () =
    scener#render ~window ~uniforms:[] ~aspect_ratio
    in
  
  let animation_tick time elapsed_time () =
    logger Debug_main_loop "Main: Moving drawables.";
    scener#tick ~window ~elapsed_time in
  
  let tick time elapsed_time () = ()(*
    logger Info ("roll "^string_of_float (roll_of_rot_quat camera#get_rotation));
    logger Info ("yaw "^string_of_float (yaw_of_rot_quat camera#get_rotation));
    logger Info ("pitch "^string_of_float (pitch_of_rot_quat camera#get_rotation))*) in

  let camera = Main_scene.camera in

  (*Setup window and inputs*)
  window#register_render_callback render;
  window#register_animation_tick_callback animation_tick;
  window#register_tick_callback tick;
  window#set_fps 60;
  window#set_tps 20;
  window#set_animation_tps 60;
  window#register_input_handler (new input_handler ~key:GLFW.Escape ~press_callback:(window#show_cursor) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.W ~press_callback:(fun () -> camera#forward ()) ~release_callback:(fun () -> camera#stop ~z:true ()));
  window#register_input_handler (new input_handler ~key:GLFW.S ~press_callback:(fun () -> camera#backward ()) ~release_callback:(fun () -> camera#stop ~z:true ()));
  window#register_input_handler (new input_handler ~key:GLFW.A ~press_callback:(fun () -> camera#left ()) ~release_callback:(fun () -> camera#stop ~x:true ()));
  window#register_input_handler (new input_handler ~key:GLFW.D ~press_callback:(fun () -> camera#right ()) ~release_callback:(fun () -> camera#stop ~x:true ()));
  window#register_input_handler (new input_handler ~key:GLFW.LeftShift ~press_callback:(fun () -> camera#down ()) ~release_callback:(fun () -> camera#stop ~y:true ()));
  window#register_input_handler (new input_handler ~key:GLFW.Space     ~press_callback:(fun () -> camera#up ()) ~release_callback:(fun () -> camera#stop ~y:true ()));
  window#register_input_handler (new toggle_input_handler ~key:GLFW.F11 ~toggle_on_callback:(fun () -> window#enable_fullscreen ()) ~toggle_off_callback:(fun () -> window#disable_fullscreen ()));
  window#register_input_handler (new input_handler ~key:GLFW.LeftControl ~press_callback:(fun () -> camera#set_speed 10.0) ~release_callback:(fun () -> camera#set_speed 5.0));
  window#register_input_handler (new toggle_input_handler ~key:GLFW.P ~toggle_on_callback:(fun () -> scener#switch_scene scene2) ~toggle_off_callback:(fun () -> scener#switch_scene scene1));

  let mouse_sensitivity = 0.001 in

  let mouse_position_callback x_offset y_offset =
    if window#is_cursor_shown = false then (
      camera#rotate_pitch ~angle:(-.mouse_sensitivity*.y_offset);
      camera#rotate_yaw ~angle:(-.mouse_sensitivity*.x_offset)) in
  window#register_mouse_callback mouse_position_callback;

  let scroll_callback x_offset y_offset = () (*camera#set_fov (camera#get_fov +. (0.01*.y_offset))*) in
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
        delete_defaults ();
        logger Info "Main: Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
