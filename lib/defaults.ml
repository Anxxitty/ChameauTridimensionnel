open Shader
open Material

let initialize_defaults () =
  default_shader#init ();
  texture_shader#init ();
  phong_shader#init ()

let delete_defaults () =
  default_shader#delete ();
  texture_shader#delete ();
  phong_shader#delete ()
