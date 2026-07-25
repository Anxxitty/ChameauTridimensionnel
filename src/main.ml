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
open Settings

(*Main function*)
let () =
  (*Initialize logger*)
  let date = Unix.localtime (Unix.time ()) in
  init_logger ~enable_log_to_file:false ~enable_debug:false ~enable_debug_main_loop:false ~log_file_path:("log/log_"^(string_of_int date.tm_mday)^"-"^(string_of_int (date.tm_mon+1))^"-"^(string_of_int (date.tm_year+1900))^"_"^(string_of_int date.tm_hour)^"h"^(string_of_int date.tm_min)^"min"^(string_of_int date.tm_sec)^"sec.txt");
  load_settings ();

  (*Create a window*)
  logger Debug "Main: Initializing a window.";
  let window = new window ~width:1280 ~height:720 ~name:"WombatCombat" in
  initialize_defaults ();
  initialize_materials ();
  initialize_scenes ();
  window#hide_cursor ();

  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;

  (*Wireframe mode*)

  let scene1 = Main_scene.scene#get in
  let scene2 = Test_scene.scene#get in

  let scener = new scene_handler ~window ~initial_scene:scene1 in

  (*Main render function*)
  let render aspect_ratio () =
    scener#render ~uniforms:[] ~aspect_ratio ~render_flags:[]
    in
  
  let animation_tick time elapsed_time () =
    logger Debug_main_loop "Main: Moving drawables.";
    scener#tick ~elapsed_time in
  
  let tick time elapsed_time () = ()(*
    logger Info ("roll "^string_of_float (roll_of_rot_quat camera#get_rotation));
    logger Info ("yaw "^string_of_float (yaw_of_rot_quat camera#get_rotation));
    logger Info ("pitch "^string_of_float (pitch_of_rot_quat camera#get_rotation))*) in

  let camera = Main_scene.camera in

  (*Setup window and inputs*)
  window#register_render_callback render;
  window#register_animation_tick_callback animation_tick;
  window#register_tick_callback tick;
  window#set_fps 600000000;
  window#set_tps 20;
  window#set_animation_tps 60;
  window#register_input_handler (new input_handler ~key:GLFW.Escape ~press_callback:(window#show_cursor) ~release_callback:(fun () -> ()));
  window#register_input_handler (new toggle_input_handler ~key:GLFW.F11 ~toggle_on_callback:(fun () -> window#enable_fullscreen ()) ~toggle_off_callback:(fun () -> window#disable_fullscreen ()));
  window#register_input_handler (new toggle_input_handler ~key:GLFW.P ~toggle_on_callback:(fun () -> scener#switch_scene scene2) ~toggle_off_callback:(fun () -> scener#switch_scene scene1));
  
  window#register_input_handler (new input_handler ~key:GLFW.Left ~press_callback:(fun () -> camera#rotate_roll ~angle:0.01) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.Right ~press_callback:(fun () -> camera#rotate_roll ~angle:(-0.01)) ~release_callback:(fun () -> ()));
  window#register_input_handler (new input_handler ~key:GLFW.R ~press_callback:(fun () -> (
    let z = normalize3f (camera#get_local_z_axis) in
    let x = normalize3f (cross3f y_axis z) in
    let y = normalize3f (cross3f z x) in
    camera#set_rotation (rot_quat_of_base ~base:(x,y,z))
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
        delete_materials ();
        delete_scenes ();
        logger Info "Main: Successfully terminated GLFW. Exiting.")
      else
        loop ();
  in loop ()
