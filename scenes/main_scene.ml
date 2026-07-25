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
let camera = new pov_camera ~bound_actor:(new movable ~position:(vec3 0.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)) ~distance:10.0 ~fov:(rad_of_deg 45.0) ~near_plane:0.1 ~far_plane:1000.0
*)
let camera = new fly_camera ~position:(vec3 0.0 0.0 0.0) ~rotation:identity_quat ~fov:(rad_of_deg 45.0) ~near_plane:0.1 ~far_plane:1000.0

let light = let light_col = vec3 1.0 0.7 0.8 in new point_light 
  ~position:(vec3 10.0 5.0 10.0) 
  ~ambient:(vec3_scalar_op ( *. ) 0.1 light_col) ~diffuse:light_col ~specular:light_col 
  ~intensity:0.7 ~linear_attenuation:0.01 ~quadratic_attenuation:0.002
let mini_lamp = let light_col = vec3 0.7 0.8 1.0 in new point_light 
  ~position:(vec3 0.0 0.0 0.0) 
  ~ambient:(vec3_scalar_op ( *. ) 0.1 light_col) ~diffuse:light_col ~specular:light_col 
  ~intensity:0.1 ~linear_attenuation:0.1 ~quadratic_attenuation:0.2

let scene = make_scene ~scene_init:(fun () -> (module struct
  (*cubes*)
  let cube_model = new model ~path:"assets/models/cube.obj" ~with_tex_and_normals:true

  let green_mat = Solid_color.Green.material#get
  let blue_mat = Solid_color.Blue.material#get
  let red_mat = Solid_color.Red.material#get

  let cube_mat_arr = make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:green_mat

  let cube_drawable0 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 20.0 20.0 20.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable1 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 0.0 10.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable2 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 10.0 0.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable3 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 0.0 10.0 10.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable4 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 10.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable5 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 10.0 0.0 10.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable6 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 10.0 10.0 0.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable7 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~position:(vec3 10.0 10.0 10.0) ~rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)

  (*orientation arrows*)
  let arrow_model = new model ~path:"assets/models/arrow.obj" ~with_tex_and_normals:true

  let green_arrow_mat_arr = make_single_material_array ~slot_names:arrow_model#get_material_slot_names ~material:green_mat
  let red_arrow_mat_arr = make_single_material_array ~slot_names:arrow_model#get_material_slot_names ~material:red_mat
  let blue_arrow_mat_arr = make_single_material_array ~slot_names:arrow_model#get_material_slot_names ~material:blue_mat

  let x_arrow = new multi_mesh_drawable ~vertex_arrays:arrow_model#get_vaos ~materials:red_arrow_mat_arr ~position:(vec3 0.0 0.0 0.0) ~rotation:(rotation_quat ~axis:(vec3 0.0 0.0 (-1.0)) ~angle:(pi/.2.)) ~scale:(vec3 0.05 0.05 0.05)
  let y_arrow = new multi_mesh_drawable ~vertex_arrays:arrow_model#get_vaos ~materials:blue_arrow_mat_arr ~position:(vec3 0.0 0.0 0.0) ~rotation:identity_quat ~scale:(vec3 0.05 0.05 0.05)
  let z_arrow = new multi_mesh_drawable ~vertex_arrays:arrow_model#get_vaos ~materials:green_arrow_mat_arr ~position:(vec3 0.0 0.0 0.0) ~rotation:(rotation_quat ~axis:(vec3 1.0 0.0 0.0) ~angle:(pi/.2.)) ~scale:(vec3 0.05 0.05 0.05)

  (*light sources*)
  let light_model = new model ~path:"assets/models/sphere.obj" ~with_tex_and_normals:true
  let light_mat = Solid_color.Light.material#get
  let light_mat_arr = make_single_material_array ~slot_names:light_model#get_material_slot_names ~material:light_mat
  let light_drawable = new multi_mesh_drawable ~vertex_arrays:light_model#get_vaos ~materials:light_mat_arr ~position:light#get_position ~rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)

  let () = light#bind_movable (Some (light_drawable :> movable))

  let sun = let light_col = vec3 1.0 1.0 0.9 in new directional_light 
    ~direction:(vec3 (-1.0) (-3.0) (-1.0)) 
    ~ambient:(vec3_scalar_op ( *. ) 0.3 light_col) ~diffuse:light_col ~specular:light_col
    ~intensity:1.5

  let torch = let light_col = vec3 1.0 1.0 0.7 in new spot_light 
    ~position:(vec3 0.0 0.0 0.0) 
    ~direction:(rotate_vec_with_quat z_axis camera#get_rotation) 
    ~inner_cut_off_angle:(rad_of_deg 10.0) ~outer_cut_off_angle:(rad_of_deg 20.0) 
    ~ambient:(vec3_scalar_op ( *. ) 0.1 light_col) ~diffuse:light_col ~specular:light_col 
    ~intensity:1.5 ~linear_attenuation:0.1 ~quadratic_attenuation:0.05
  
  (*car*)
  let car_model = new model ~path:"assets/models/car.obj" ~with_tex_and_normals:true

  let car_interior_material = Car_interior.material#get
  let car_exterior_material = Car_exterior.material#get
  let car_windows_material = Car_windows.material#get

  let car_mat_arr = new material_array ~nb_of_slots:car_model#get_nb_material_slots ~slot_names:car_model#get_material_slot_names ~materials:([car_windows_material;car_exterior_material;car_interior_material] :> material list) ()

  let car_drawable = new multi_mesh_drawable ~vertex_arrays:car_model#get_vaos ~materials:car_mat_arr ~position:(vec3 5.0 5.0 5.0) ~rotation:(rotation_quat ~axis:y_axis ~angle:pi) ~scale:(vec3 0.1 0.1 0.1)

  let () = (*camera#set_bound_actor (car_drawable :> movable);*)
    mini_lamp#bind_movable (Some (car_drawable :> movable))

  let movables = (camera :> movable) :: (torch :> movable) :: (light :> movable) :: to_movable [cube_drawable0;cube_drawable1;cube_drawable2;cube_drawable3;cube_drawable4;cube_drawable5;cube_drawable6;cube_drawable7;car_drawable;light_drawable]
  let opaque_drawables = to_drawable [cube_drawable0;cube_drawable1;cube_drawable2;cube_drawable3;cube_drawable4;cube_drawable5;cube_drawable6;cube_drawable7;light_drawable;x_arrow;y_arrow;z_arrow]
  let translucent_drawables = to_drawable [car_drawable]
  let lights = [
    "point_lights", to_light [light; mini_lamp];
    "directional_lights", to_light [sun];
    "spot_lights", to_light [torch];
  ]
  let to_delete = [(cube_model :> deletable); (car_model :> deletable); (light_model :> deletable)]
  let active_camera = (camera :> camera)

  let keybindings = [
    new input_handler ~key:GLFW.W ~press_callback:(fun () -> camera#forward ()) ~release_callback:(fun () -> camera#stop ~z:true ());
    new input_handler ~key:GLFW.A ~press_callback:(fun () -> camera#left ()) ~release_callback:(fun () -> camera#stop ~x:true ());
    new input_handler ~key:GLFW.S ~press_callback:(fun () -> camera#backward ()) ~release_callback:(fun () -> camera#stop ~z:true ());
    new input_handler ~key:GLFW.D ~press_callback:(fun () -> camera#right ()) ~release_callback:(fun () -> camera#stop ~x:true ());
    new input_handler ~key:GLFW.LeftShift ~press_callback:(fun () -> camera#down ()) ~release_callback:(fun () -> camera#stop ~y:true ());
    new input_handler ~key:GLFW.Space     ~press_callback:(fun () -> camera#up ()) ~release_callback:(fun () -> camera#stop ~y:true ());
    new input_handler ~key:GLFW.LeftControl ~press_callback:(fun () -> camera#set_speed 10.0) ~release_callback:(fun () -> camera#set_speed 5.0);
    new input_handler ~key:GLFW.O ~press_callback:(fun () -> light#set_diffuse (vec3 1.0 1.0 1.0)) ~release_callback:(fun () -> ());
    new input_handler ~key:GLFW.L ~press_callback:(fun () -> light#set_diffuse (vec3 1.0 0.8 0.7)) ~release_callback:(fun () -> ());
    new input_handler ~key:GLFW.N ~press_callback:(fun () -> light#set_diffuse (vec3 0.7 1.0 0.8)) ~release_callback:(fun () -> ());
    new toggle_input_handler ~key:GLFW.T ~toggle_on_callback:(fun () -> torch#set_direction y_axis) ~toggle_off_callback:(fun () -> torch#set_direction (vec3_scalar_op ( *. ) (-1.0) z_axis));
  ]

  let mouse_sensitivity = 0.001

  let captures_cursor = true

  let mouse_callback x_offset y_offset =
    camera#rotate_pitch ~angle:(-.mouse_sensitivity*.y_offset);
    camera#rotate_yaw ~angle:(-.mouse_sensitivity*.x_offset)

  let scroll_callback x_offset y_offset = camera#set_fov (camera#get_fov +. (0.01*.y_offset))

  let movables_persistents = List.mapi (fun i m ->
    new persistent ~handle:("movable"^string_of_int i) ~to_string:json_of_movable ~from_string:movable_of_json ~read_target:(fun () -> m) ~write_target:(fun m' -> m#copy_movable m')
  ) movables
  let save = new json_save ~path:"save/main_scene.json"
  let to_save = 
    (List.map (fun p -> (module struct type persistent_type = float vector3;; let persistent = p end : Persistent)) movables_persistents)

  let on_load ~window = ()

  let on_unload ~window = ()

  let tick ~(window : window) ~(elapsed_time : float) = (
    let time = GLFW.getTime () in
    light_drawable#set_position (vec3 (20.0*.cos(time)) 5.0 (20.0*.sin(time)));
    torch#set_position camera#get_position;
    torch#set_direction (vec3_scalar_op ( *. ) (-1.0) camera#get_local_z_axis);
    (*/!\ This changes the color of the material stored in Solid_color.Light !!!*)
    light_mat#get_shader_program#set_uniform3f ~name:"color" ~value:(light#get_diffuse))
end : Scene))
