open Shader
open Logger
open Data_structures
open Tgl3

class virtual material =
  object (self)
    val virtual _shader_program : shader_program
    method virtual get_shader_program : shader_program
    method virtual prep_render : (uniforms : uniform list -> unit)
  end

class generic_material ~shader_program =
  object (self)
    inherit material
    val _shader_program = shader_program
    method get_shader_program =
      _shader_program
    method prep_render ~uniforms =
      _shader_program#use ();
      let rec uniform_setter l = match l with
        | [] -> ()
        | a::q -> (
          _shader_program#set_uniform a;
          uniform_setter q)
      in uniform_setter uniforms;
  end

(*Wrapper for OpenGL 2D texture object*)
class texture ~path =
  (*loads the texture with stb_image*)
  let texture_image_try = Stb_image.load path in
  let texture_image = match texture_image_try with
    | Ok a -> texture_image_try
    | Error e -> match e with `Msg e -> (logger Warning ("texture: Failed to load texture at path \""^path^"\". Defaulting to default texture. Error message: "^e));
      Stb_image.load "defaults/textures/default.png" in

  let id = get_first_int (Gl.gen_textures 1) in

  let () = (
    (*setting default parameters for the texture*)
    Gl.bind_texture Gl.texture_2d id;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.repeat;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
    
    (*checks if the image loading has been successful and loads the image in OpenGL accordingly*)
    (*if image loading failed, the texture will default to all-black (default OpenGL behavior when no texture data is given)*)
    (*Note: the ocaml binding of stb_image does not provide stbi_set_flip_vertically_on_load and I can't seem to get Stb_image.vflip to have any effect*)
    (*Note: thus the UVs have to be flipped in the vertex shader, be careful !*)
    match texture_image with
      | Ok a -> (
        let tex_format = match a.channels with
          | 1 -> Gl.r8
          | 2 -> Gl.rg
          | 3 -> Gl.rgb
          | 4 -> Gl.rgba
          | _ -> logger Warning ("texture: Invalid number of channels for texture at path \""^path^"\"."); 0 in
        if tex_format <> 0 then
          Gl.tex_image2d Gl.texture_2d 0 tex_format a.width a.height 0 tex_format Gl.unsigned_byte (`Data a.data);
        Gl.generate_mipmap Gl.texture_2d;
      )
      | Error e -> match e with `Msg e -> (logger Warning ("texture: Failed to load default texture. Error message: "^e));
    
    Gl.bind_texture Gl.texture_2d 0
  ) in

  object (self)
    val _id = id
    val _texture_image = texture_image 
    method set_texture_filtering param = 
      Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter param;
      Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter param
    method set_mag_texture_filtering param =
      Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter param
    method set_min_texture_filtering param =
      Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter param
    method set_texture_wrapping param =
      Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s param;
      Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t param
    method set_texture_wrapping_s param =
      Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s param
    method set_texture_wrapping_t param =
      Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t param
    method bind () =
      Gl.bind_texture Gl.texture_2d _id
    method unbind () =
      Gl.bind_texture Gl.texture_2d 0
    method delete () =
      Gl.delete_textures 1 (set_first_int _id);
  end

class textured_material ~shader_program ~(textures : texture list) = 
  object (self)
    inherit generic_material ~shader_program as super
    val mutable _textures = textures
    method get_textures =
      _textures
    method set_textures texs =
      _textures <- texs
    method! prep_render ~uniforms =
      let rec bind_textures l i = match l with
        | [] -> ()
        | a::q -> 
          (*OpenGL does not provide more than 16 texture units*)
          if i > 15 then logger Warning "textured_material: exceeded the maximum of 16 texture units. Some textures will not display correctly."
          else (
            Gl.active_texture (Gl.texture0 + i);
            a#bind ();
            _shader_program#set_uniform1i ~name:("texture"^(string_of_int i)) ~value:{x=1};
            bind_textures q (i+1) 
          ) in
      bind_textures textures 0;
      super#prep_render ~uniforms
  end

class material_array ~nb_of_slots ~(slot_names : string list) ?(materials : material list option) () =
  (*generates a default material to provide as initialisation value to the array*)
  let def_vert = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"defaults/shaders/default_vert.glsl" in
  let def_frag = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"defaults/shaders/default_frag.glsl" in
  (*Note: this default shader lives indefinitely because it would be difficult to tell when to delete it except if I create a delete function for the material array, which would not have much sense...*)
  (*Note: this is theoretically a memory leak, but it should not cause much of an issue except if someone repeatedly creates massive amounts of material arrays...*)
  let def_shader = new shader_program ~vertex_shader:def_vert ~fragment_shader:def_frag in
  let def_mat = new generic_material ~shader_program:def_shader in
  object (self)
    inherit [material] named_array ~nb_of_slots ~slot_names ~default_content:def_mat ~contents:(match materials with | Some a -> a | None -> []) ()
  end
