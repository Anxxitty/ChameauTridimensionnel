open Shader
open Material

let initialize_defaults () =
  default_shader#init ();
  texture_shader#init ();
  phong_shader#init ();
  default_texture#init ();
  default_specular#init ();
  default_normal_map#init ();
  local_bases_visualizer_shader#init ();
  normals_visualizer_shader#init ()

let delete_defaults () =
  default_shader#delete ();
  texture_shader#delete ();
  phong_shader#delete ();
  default_texture#delete ();
  default_specular#delete ();
  default_normal_map#delete ();
  local_bases_visualizer_shader#delete ();
  normals_visualizer_shader#delete ()
