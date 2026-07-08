open ChameauTridimensionnel
open Scene_handler
open Window
open Model
open Shader
open Logger
open Graphics
open Materials
open Bigarray
open Math
open Tgl3
open Actor
open Data_structures

let scene = make_scene ~scene_init:(fun () -> (module struct

let cube_model = new model ~path:"assets/models/cube.obj" ~with_tex_and_normals:true
let cat_model = new model ~path:"assets/models/cat.obj" ~with_tex_and_normals:true
let tie_model = new model ~path:"assets/models/tie.obj" ~with_tex_and_normals:false
let dark_vador_tie_model = new model ~path:"assets/models/darkvadortie.obj" ~with_tex_and_normals:false
let hexagon_model = new model ~path:"assets/models/hexagon.obj" ~with_tex_and_normals:true
let table_model = new model ~path:"assets/models/table.obj" ~with_tex_and_normals:false
let torus_model = new model ~path:"assets/models/torus.obj" ~with_tex_and_normals:true
let dirac_model = new model ~path:"assets/models/dirac.obj" ~with_tex_and_normals:false
let bike_model = new model ~path:"assets/models/bike.obj" ~with_tex_and_normals:false
let knob_model = new model ~path:"assets/models/testObj.obj" ~with_tex_and_normals:true
let knob_mat = new material_array ~nb_of_slots:(List.length knob_model#get_material_slot_names) ~slot_names:knob_model#get_material_slot_names ()

let sphere_model = new model ~path:"assets/models/sphere.obj" ~with_tex_and_normals:true
let sphere_mat = make_3D_textured_mesh_material_array ~slot_names:sphere_model#get_material_slot_names ~textures_paths:[["assets/textures/sphere_texture.png"]]
let sphere_drawable = new multi_mesh_drawable ~vertex_arrays:sphere_model#get_vaos ~materials:sphere_mat ~scene_coordinates:(vec3 10.0 5.0 50.0) ~local_rotation:identity_quat ~scale:(vec3 1.0 1.0 1.0)

let car_model = new model ~path:"assets/models/car.obj" ~with_tex_and_normals:true

let car_ext_texture = new texture ~path:"assets/textures/car_exterior.jpeg"
let car_int_texture = new texture ~path:"assets/textures/car_interior.jpeg"
let brick_texture = new texture ~path:"assets/textures/brick.png"

let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex_tex.glsl"
let fragment_shader1 = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment_tex.glsl"
let fragment_shader2 = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment_tex2.glsl"
let tex_shader = new shader_program ~vertex_shader ~fragment_shader:fragment_shader1
let tex_shader2 = new shader_program ~vertex_shader ~fragment_shader:fragment_shader2

let frag_windows = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment_windows.glsl"
let windows_shader = new shader_program ~vertex_shader ~fragment_shader:frag_windows

let windows_material = new generic_material ~shader_program:windows_shader
let body_material = new textured_material ~shader_program:tex_shader ~textures:[car_ext_texture]
let int_material = new textured_material ~shader_program:tex_shader ~textures:[car_int_texture]
let car_material_array = new material_array ~nb_of_slots:car_model#get_nb_material_slots ~slot_names:car_model#get_material_slot_names ()
let () = (car_material_array#set_content ~name:"Windows_SG" ~content:windows_material;
car_material_array#set_content ~name:"Body_SG1" ~content:(body_material :> material);
car_material_array#set_content ~name:"Interior_SG" ~content:(int_material :> material))

let cube_mat = new textured_material ~shader_program:tex_shader2 ~textures:[brick_texture;car_ext_texture]
let cube_mat_arr = new material_array ~nb_of_slots:cube_model#get_nb_material_slots ~slot_names:cube_model#get_material_slot_names ~materials:[(cube_mat :> material)] ()

let tex_cube_drawable = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:cube_mat_arr ~scene_coordinates:{x=0.0;y=10.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=5.0;y=5.0;z=5.0}
let car_drawable = new multi_mesh_drawable ~vertex_arrays:car_model#get_vaos ~materials:car_material_array ~scene_coordinates:{x=0.0;y=10.0;z=(-15.0)} ~local_rotation:identity_quat ~scale:{x=(0.1);y=(0.1);z=(0.1)}

let car_mat_arr2 = make_3D_textured_mesh_material_array ~slot_names:car_model#get_material_slot_names ~textures_paths:[["assets/textures/car_exterior.jpeg"];["assets/textures/car_exterior.jpeg"];["assets/textures/car_interior.jpeg"]]
let () = car_mat_arr2#set_content ~name:"Windows_SG" ~content:windows_material
let car_drawable2 = new multi_mesh_drawable ~vertex_arrays:car_model#get_vaos ~materials:car_mat_arr2 ~scene_coordinates:(vec3 0. 0. 0.) ~local_rotation:identity_quat ~scale:(vec3 0.1 0.1 0.1)

  (*Compiling shaders*)
let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex2.glsl"
let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment.glsl"
let shader_program = new shader_program ~vertex_shader ~fragment_shader

let mat = new generic_material ~shader_program

let bike_mat = make_single_material_array ~slot_names:bike_model#get_material_slot_names ~material:mat
let bike_drawable = new multi_mesh_drawable ~vertex_arrays:bike_model#get_vaos ~materials:bike_mat ~scene_coordinates:(vec3 0. 100. 0.) ~local_rotation:identity_quat ~scale:(vec3 1. 1. 1.) 

let cube_drawable = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:(make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=0.0;y=3.0;z=(-5.0)} ~local_rotation:identity_quat ~scale:{x=1.5;y=1.5;z=1.5}
let cat_initial_rot = rotation_quat ~axis:{x=1.0;y=0.0;z=0.0} ~angle:(rad_of_deg (-90.0))
let cat_drawable = new multi_mesh_drawable ~vertex_arrays:cat_model#get_vaos ~materials:(make_3D_textured_mesh_material_array ~slot_names:cat_model#get_material_slot_names ~textures_paths:[["assets/textures/cat_texture.png"]]) ~scene_coordinates:{x=(-16.0);y=5.0;z=0.0} ~local_rotation:cat_initial_rot ~scale:{x=0.25;y=0.25;z=0.25}
let tie_drawable = new multi_mesh_drawable ~vertex_arrays:tie_model#get_vaos ~materials:(make_single_material_array ~slot_names:tie_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=0.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=0.1;y=0.1;z=0.1}
let dark_vador_tie_drawable = new multi_mesh_drawable ~vertex_arrays:dark_vador_tie_model#get_vaos ~materials:(make_single_material_array ~slot_names:dark_vador_tie_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=12.0;y=3.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=0.05;y=0.05;z=0.05}
let hexagon_drawable = new multi_mesh_drawable ~vertex_arrays:hexagon_model#get_vaos ~materials:(make_single_material_array ~slot_names:hexagon_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=0.0;y=3.0;z=(-20.0)} ~local_rotation:identity_quat ~scale:{x=0.5;y=0.5;z=0.5}
let table_drawable = new multi_mesh_drawable ~vertex_arrays:table_model#get_vaos ~materials:(make_single_material_array ~slot_names:table_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=0.0;y=(-16.0);z=0.0} ~local_rotation:identity_quat ~scale:{x=0.25;y=0.25;z=0.25}
let torus_drawable = new multi_mesh_drawable ~vertex_arrays:torus_model#get_vaos ~materials:(make_single_material_array ~slot_names:torus_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=0.0;y=3.0;z=12.0} ~local_rotation:identity_quat ~scale:{x=3.0;y=3.0;z=3.0}
let dirac_drawable = new multi_mesh_drawable ~vertex_arrays:dirac_model#get_vaos ~materials:(make_single_material_array ~slot_names:dirac_model#get_material_slot_names ~material:mat) ~local_rotation:identity_quat ~scene_coordinates:{x=30.0;y=5.0;z=(-50.0)} ~scale:{x=1.0;y=1.0;z=1.0}

let dirac = new multi_mesh_drawable ~vertex_arrays:cube_model#get_vaos ~materials:(make_single_material_array ~slot_names:cube_model#get_material_slot_names ~material:mat) ~scene_coordinates:{x=10.0;y=0.0;z=25.0} ~local_rotation:identity_quat ~scale:{x=1.0;y=1e38;z=1.0}

let () = (
knob_mat#set_content ~name:"InnerMat" ~content:(windows_material);
knob_mat#set_content ~name:"OuterMat" ~content:(cube_mat :> material))

let knob_drawable = new multi_mesh_drawable ~vertex_arrays:knob_model#get_vaos ~materials:knob_mat ~scene_coordinates:(vec3 0. 10. 20.) ~local_rotation:identity_quat ~scale:(vec3 1. 1. 1.)

(*trying to create a sky*)
let sky_pos = new vertex_attribute ~attribute_type:(Vector4_kind Float32) ~number_of_vertices:4 ~vector_list:[
  Vector4 {x=(-1.0); y=(-1.0); z=0.9999; w=1.0};
  Vector4 {x=1.0; y=(-1.0); z=0.9999; w=1.0};
  Vector4 {x=1.0; y=1.0; z=0.9999; w=1.0};
  Vector4 {x=(-1.0); y=1.0; z=0.9999; w=1.0}
] ()

let sky_ebo = new buffer ~buffer_type:Gl.element_array_buffer ~kind:Int16_unsigned
let () = sky_ebo#write_element_buffer ~index:[0;1;2;0;2;3] ~usage:Gl.static_draw

let sky_vao = new vertex_array ~kind:Float32 ~vertex_attributes:[sky_pos] ~element_buffer:sky_ebo ~number_of_drawn_vertices:6 ~drawing_type:Gl.static_draw

let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/sky_vert.glsl"
let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/sky_frag.glsl"
let sky_shader = new shader_program ~vertex_shader ~fragment_shader

let sky_mat = new generic_material ~shader_program:sky_shader

let sky_drawable = new simple_drawable ~vertex_array:sky_vao ~material:sky_mat ~scene_coordinates:{x=0.0;y=0.0;z=0.0} ~local_rotation:identity_quat ~scale:{x=1.0;y=1.0;z=1.0}

let camera = new pov_camera ~bound_actor:(car_drawable :> movable) ~distance:(10.0) ~fov:(rad_of_deg 45.0) ~near_plane:0.1 ~far_plane:1000.0

let () = car_drawable#rotate_local ~axis:(vec3 0.0 1.0 0.0) ~angle:pi

let movables = [(car_drawable :> movable)]
let opaque_drawables = [(car_drawable :> drawable)]
let translucent_drawables = [(car_drawable :> drawable);(sky_drawable :> drawable)]
let active_camera = (camera :> camera)
let to_delete = ([car_model; cube_model] :> < delete : unit -> unit > list)
let tick ~(window : window) ~(elapsed_time : float) = ()

end : Scene))