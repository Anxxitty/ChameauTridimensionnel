open Shader
open Materials

let initialize_defaults () =
  default_shader#init ();
  texture_shader#init ()

let delete_defaults () =
  default_shader#delete ();
  texture_shader#delete ()
