open ChameauTridimensionnel
open Shader
open Material
open Math
open Tgl3
open Data_structures

module type Color = sig val color : float vector3 end
let color x = (module struct let color = x end : Color)

module Create ( C : Color ) = struct
  type material_type = generic_material

  let material = make_material 
    ~init:(fun () -> (
      let basic_vert = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/vertex.glsl" in
      let basic_frag = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/fragment.glsl" in
      let green_shader = new shader_program ~vertex_shader:basic_vert ~fragment_shader:basic_frag () in
      green_shader#set_uniform3f ~name:"color" ~value:C.color;
      new generic_material ~shader_program:green_shader))
    ~delete:(fun m -> (
      m#get_shader_program#delete ())) 
end

module Green = Create (val color (vec3 0.0 1.0 0.0))

module Blue = Create (val color (vec3 0.0 0.0 1.0))

module Red = Create (val color (vec3 1.0 0.0 0.0))

module White = Create (val color (vec3 1.0 1.0 1.0))

module Light = Create (val color (vec3 1.0 1.0 1.0)) (*One is created specially for light so that changing its color won't affect other objects*)
