open ChameauTridimensionnel
open Shader
open Material
open Math
open Tgl3
open Data_structures

module type Color = sig val color : float vector4 end
let color x = (module struct let color = x end : Color)

module Create ( C : Color ) = struct
  type material_type = generic_material

  let material = make_material 
    ~init:(fun () -> (
      let basic_vert = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"assets/shaders/translucent_color_vert.glsl" in
      let basic_frag = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"assets/shaders/translucent_color_frag.glsl" in
      let shader_program = new shader_program ~vertex_shader:basic_vert ~fragment_shader:basic_frag () in
      shader_program#set_uniform4f ~name:"color" ~value:C.color;
      new generic_material ~shader_program))
    ~delete:(fun m -> (
      m#get_shader_program#delete ())) 
end

module Green = Create (val color (vec4 0.0 1.0 0.0 0.2))
