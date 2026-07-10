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

let camera = new pov_camera ~bound_actor:(new movable ~scene_coordinates:(vec3 0.0 0.0 0.0) ~local_rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)) ~distance:10.0 ~fov:(rad_of_deg 45.0) ~near_plane:0.1 ~far_plane:1000.0

let scene = make_scene ~scene_init:(fun () -> (module struct
  (*cubes*)
  let cube_model = new model ~path:"assets/models/cube.obj" ~with_tex_and_normals:true

  let green_mat = Solid_color.Green.material#get

  let cube_mat_arr = make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:green_mat

  let cube_drawable0 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 0.0 0.0 0.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable1 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 0.0 0.0 10.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable2 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 0.0 10.0 0.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable3 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 0.0 10.0 10.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable4 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 10.0 0.0 0.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable5 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 10.0 0.0 10.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable6 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 10.0 10.0 0.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)
  let cube_drawable7 = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:(vec3 10.0 10.0 10.0) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)

  (*light*)
  let light_position = vec3 20.0 20.0 20.0
  let light_color = vec3 1.0 1.0 0.9
  let light_intensity = 50.0

  let light_model = new model ~path:"assets/models/sphere.obj" ~with_tex_and_normals:true

  let light_mat = Solid_color.Light.material#get

  let light_mat_arr = make_single_material_array ~slot_names:light_model#get_material_slot_names ~material:light_mat

  let light_drawable = new multi_mesh_drawable ~vertex_arrays:light_model#get_vaos ~materials:light_mat_arr ~scene_coordinates:light_position ~local_rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)

  (*car*)
  let car_model = new model ~path:"assets/models/car.obj" ~with_tex_and_normals:true

  let car_interior_material = Car_interior.material#get
  let car_exterior_material = Car_exterior.material#get
  let car_windows_material = Car_windows.material#get

  let phong_mats = [car_exterior_material;car_interior_material;car_windows_material]

  let car_mat_arr = new material_array ~nb_of_slots:car_model#get_nb_material_slots ~slot_names:car_model#get_material_slot_names ~materials:([car_windows_material;car_exterior_material;car_interior_material] :> material list) ()

  let car_drawable = new multi_mesh_drawable ~vertex_arrays:car_model#get_vaos ~materials:car_mat_arr ~scene_coordinates:(vec3 5.0 5.0 5.0) ~local_rotation:(rotation_quat ~axis:y_axis ~angle:pi) ~scale:(vec3 0.1 0.1 0.1)

  let () = camera#set_bound_actor (car_drawable :> movable)

  let movables = to_movable [cube_drawable0;cube_drawable1;cube_drawable2;cube_drawable3;cube_drawable4;cube_drawable5;cube_drawable6;cube_drawable7;car_drawable;light_drawable]
  let opaque_drawables = to_drawable [cube_drawable0;cube_drawable1;cube_drawable2;cube_drawable3;cube_drawable4;cube_drawable5;cube_drawable6;cube_drawable7;car_drawable;light_drawable]
  let translucent_drawables = []
  let to_delete = [(cube_model :> deletable); (car_model :> deletable)]
  let active_camera = (camera :> camera)

  let camera_persistent = new persistent ~handle:"camera" ~to_string:json_of_movable ~from_string:movable_of_json ~read_target:(fun () -> (active_camera :> movable)) ~write_target:(fun m -> active_camera#copy_movable m)
  let movables_persistents = List.mapi (fun i m ->
    new persistent ~handle:("movable"^string_of_int i) ~to_string:json_of_movable ~from_string:movable_of_json ~read_target:(fun () -> m) ~write_target:(fun m' -> m#copy_movable m')
  ) movables
  let save = new json_save ~path:"save/main_scene.json"
  let to_save = 
    (module struct type persistent_type = float vector3;; let persistent = camera_persistent end : Persistent) ::
    (List.map (fun p -> (module struct type persistent_type = float vector3;; let persistent = p end : Persistent)) movables_persistents)

  let tick ~(window : window) ~(elapsed_time : float) = (
    camera#tick ~elapsed_time;
    let time = GLFW.getTime () in
    List.iter (fun phong_mat -> (
      phong_mat#set_light_position (vec3 (20.0*.cos(time)) 5.0 (20.0*.sin(time)));
      phong_mat#set_light_color light_color)) phong_mats;
    light_drawable#set_coordinates (vec3 (20.0*.cos(time)) 5.0 (20.0*.sin(time))));
    (*/!\ This changes the color of the material stored in Solid_color.Light !!!*)
    light_mat#get_shader_program#set_uniform3f ~name:"color" ~value:light_color
end : Scene))
