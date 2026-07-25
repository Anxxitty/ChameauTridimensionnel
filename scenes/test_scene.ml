open ChameauTridimensionnel
open Scene_handler
open Window
open Model
open Shader
open Logger
open Graphics
open Material
open Materials
open Bigarray
open Math
open Tgl3
open Actor
open Data_structures
open Save

(*
let camera = new fly_camera ~position:(vec3 0.0 0.0 0.0) ~fov:(rad_of_deg 45.0) ~near_plane:0.1 ~far_plane:1000.0
*)
let camera = new pov_camera ~bound_actor:(new movable ~position:(vec3 0.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)) ~distance:10.0 ~fov:(rad_of_deg 45.0) ~near_plane:0.1 ~far_plane:1000.0

let light = let light_col = vec3 1.0 0.7 0.8 in new point_light 
  ~position:(vec3 10.0 5.0 10.0) 
  ~ambient:(vec3_scalar_op ( *. ) 0.1 light_col) ~diffuse:light_col ~specular:light_col 
  ~intensity:1.0 ~linear_attenuation:0.1 ~quadratic_attenuation:0.05

let scene = make_scene ~scene_init:(fun () -> (module struct
  (*cubes*)
  let cube_model = new model ~path:"assets/models/cube.obj" ~with_tex_and_normals:true

  let cube_mat = Translucent_color.Green.material#get
  let cube_solid_mat = Phong_cube.material#get
  let cube_mat_arr = make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:cube_mat
  let cube_mat_arr2 = make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:(cube_solid_mat :> material)

  let drawable0 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable1 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 3.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable2 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 6.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable3 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 9.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable4 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 3.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable5 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 6.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable6 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 9.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable7 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 0.0 3.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable8 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 0.0 6.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable9 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 0.0 9.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable10 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr2 ~position:(vec3 12.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable11 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr2 ~position:(vec3 0.0 12.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  let drawable12 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr2 ~position:(vec3 0.0 0.0 12.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)

  (*testing normal matrix*)

  let normal_test_mat = Normal_matrix_test.material#get
  let normal_test_mat_arr = make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:(normal_test_mat :> material)

  let undeformed_cube = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:normal_test_mat_arr ~position:(vec3 10.0 10.0 10.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)
  
  let deformed_cube = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:normal_test_mat_arr ~position:(vec3 10.0 10.0 20.0) ~rotation:identity_quat ~scale:(vec3 1.0 10.0 30.0) 

  (*light*)
  let light_drawable = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr2 ~position:(vec3 10.0 5.0 10.0) ~rotation:(rotation_quat ~axis:y_axis ~angle:pi) ~scale:(vec3 1.0 1.0 1.0)
  let () = light#bind_movable (Some (light_drawable :> movable)); camera#set_bound_actor (light_drawable :> movable)

  let movables = [(camera :> movable);(light :> movable);(light_drawable :> movable)]
  let opaque_drawables = to_drawable [drawable10;drawable11;drawable12;light_drawable;undeformed_cube;deformed_cube]
  let translucent_drawables = to_drawable [drawable0;drawable1;drawable2;drawable3;drawable4;drawable5;drawable6;drawable7;drawable8;drawable9]
  let to_delete = [(cube_model :> deletable)]
  let lights = [
    "point_lights", to_light [light]
  ]
  let active_camera = (camera :> camera)

  let keybindings = [
    new input_handler ~key:GLFW.W ~press_callback:(fun () -> camera#forward ()) ~release_callback:(fun () -> camera#stop ~z:true ());
    new input_handler ~key:GLFW.A ~press_callback:(fun () -> camera#left ()) ~release_callback:(fun () -> camera#stop ~x:true ());
    new input_handler ~key:GLFW.S ~press_callback:(fun () -> camera#backward ()) ~release_callback:(fun () -> camera#stop ~z:true ());
    new input_handler ~key:GLFW.D ~press_callback:(fun () -> camera#right ()) ~release_callback:(fun () -> camera#stop ~x:true ());
    new input_handler ~key:GLFW.LeftShift ~press_callback:(fun () -> camera#down ()) ~release_callback:(fun () -> camera#stop ~y:true ());
    new input_handler ~key:GLFW.Space     ~press_callback:(fun () -> camera#up ()) ~release_callback:(fun () -> camera#stop ~y:true ());
    new input_handler ~key:GLFW.LeftControl ~press_callback:(fun () -> camera#set_speed 10.0) ~release_callback:(fun () -> camera#set_speed 5.0);
  ]

  let mouse_sensitivity = 0.001

  let captures_cursor = true

  let mouse_callback x_offset y_offset =
    camera#rotate_pitch ~angle:(-.mouse_sensitivity*.y_offset);
    camera#rotate_yaw ~angle:(-.mouse_sensitivity*.x_offset)

  let scroll_callback x_offset y_offset = camera#set_fov (camera#get_fov +. (0.01*.y_offset))

  let save = new json_save ~path:"save/test_scene.json"
  let to_save = []

  let on_load ~window = ()

  let on_unload ~window = ()

  let tick ~(window : window) ~(elapsed_time : float) =
    ()

end : Scene))
