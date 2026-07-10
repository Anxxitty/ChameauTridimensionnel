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

  let basic_vert = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex.glsl"
  let basic_frag = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment.glsl"
  let green_shader = new shader_program ~vertex_shader:basic_vert ~fragment_shader:basic_frag
  let () = green_shader#set_uniform3f ~name:"color" ~value:(vec3 0.0 1.0 0.0)

  let green_mat = new generic_material ~shader_program:green_shader

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

  let basic_vert = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex.glsl"
  let basic_frag = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment.glsl"
  let light_shader = new shader_program ~vertex_shader:basic_vert ~fragment_shader:basic_frag
  let () = light_shader#set_uniform3f ~name:"color" ~value:light_color

  let light_mat = new generic_material ~shader_program:light_shader

  let light_mat_arr = make_single_material_array ~slot_names:light_model#get_material_slot_names ~material:light_mat

  let light_drawable = new multi_mesh_drawable ~vertex_arrays:light_model#get_vaos ~materials:light_mat_arr ~scene_coordinates:light_position ~local_rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)


  (*cat*)
  let cat_model = new model ~path:"assets/models/cat.obj" ~with_tex_and_normals:true

  let cat_texture = new texture ~path:"assets/textures/cat_texture.png"

  let cat_material = new textured_phong_material ~textures:[cat_texture] ~alpha:1.0 ~ambient_coeff:0.3 ~specular_expo:32 ~light_position ~light_color ~light_intensity

  let phong_mats = [cat_material]

  let cat_mat_arr = new material_array ~nb_of_slots:cat_model#get_nb_material_slots ~slot_names:cat_model#get_material_slot_names ~materials:([cat_material] :> material list) ()

  let cat_drawable = new multi_mesh_drawable ~vertex_arrays:cat_model#get_vaos ~materials:cat_mat_arr ~scene_coordinates:(vec3 5.0 5.0 5.0) ~local_rotation:(rotation_quat ~axis:y_axis ~angle:pi) ~scale:(vec3 0.1 0.1 0.1)

  let () = camera#set_bound_actor (cat_drawable :> movable)

  let movables = ([cube_drawable0;cube_drawable1;cube_drawable2;cube_drawable3;cube_drawable4;cube_drawable5;cube_drawable6;cube_drawable7;cat_drawable;light_drawable] :> movable list)
  let opaque_drawables = ([cube_drawable0;cube_drawable1;cube_drawable2;cube_drawable3;cube_drawable4;cube_drawable5;cube_drawable6;cube_drawable7;cat_drawable;light_drawable] :> drawable list)
  let translucent_drawables = []
  let to_delete = [(green_shader :> deletable); (light_shader :> deletable); (cat_texture :> deletable); (cube_model :> deletable); (cat_model :> deletable)]
  let active_camera = (camera :> camera)

  let save = new json_save ~path:"save/test_scene.json"
  let to_save = []

  let tick ~(window : window) ~(elapsed_time : float) = (
    camera#tick ~elapsed_time;
    let time = GLFW.getTime () in
    List.iter (fun phong_mat -> (
      phong_mat#set_light_position (vec3 (20.0*.cos(time)) 5.0 (20.0*.sin(time)));
      phong_mat#set_light_color light_color)) phong_mats;
    light_shader#set_uniform3f ~name:"color" ~value:light_color;
    light_drawable#set_coordinates (vec3 (20.0*.cos(time)) 5.0 (20.0*.sin(time))))
end : Scene))
