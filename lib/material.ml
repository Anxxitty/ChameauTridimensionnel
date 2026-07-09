open Shader
open Logger
open Data_structures
open Tgl3
open Math

class virtual material =
  object (self)
    val virtual _shader_program : shader_program
    method virtual get_shader_program : shader_program
    method virtual prep_render : (uniforms : uniform list -> unit)
  end

let to_material (a : #material list) =
  List.map (fun x -> (x :> material)) a

module type Material = sig
  type material_type
  val material : material_type default
end

let material_registry = ref []

let make_material ~init ~delete = 
  let m = new default ~init ~delete in
  material_registry := (m :> material default)::(!material_registry); m

let get_material (module M : Material) = M.material#get

let initialize_materials () =
  List.iter (fun m -> m#init ()) (!material_registry)

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

let phong_shader = new default ~init:(fun x ->(
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"defaults/shaders/3D_textured_phong_vert.glsl" in
  let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"defaults/shaders/3D_textured_phong_frag.glsl" in
  new shader_program ~vertex_shader ~fragment_shader
)) ~delete:(fun x -> x#delete ())

class textured_phong_material ~(textures : texture list) ~(alpha : float) ~(ambient_coeff : float) ~(specular_expo : int) ~(light_position : float vector3) ~(light_color : float vector3) ~(light_intensity : float) =
  object (self)
    inherit textured_material ~shader_program:(phong_shader#get) ~textures as super
    val mutable _alpha = alpha
    val mutable _ambient_coeff = ambient_coeff
    val mutable _specular_expo = specular_expo
    val mutable _light_position = light_position
    val mutable _light_color = light_color
    val mutable _light_intensity = light_intensity
    (*getters and setters*)
    method get_alpha =
      _alpha
    method get_ambient_coeff =
      _ambient_coeff
    method get_specular_expo =
      _specular_expo
    method get_light_position =
      _light_position
    method get_light_color =
      _light_color
    method get_light_intensity =
      _light_intensity
    method set_alpha x =
      _alpha <- x
    method set_ambient_coeff x =
      _ambient_coeff <- x
    method set_specular_expo x =
      _specular_expo <- x
    method set_light_position x =
      _light_position <- x
    method set_light_color x =
      _light_color <- x
    method set_light_intensity x =
      _light_intensity <- x
    
    method! prep_render ~uniforms =
      _shader_program#set_uniform1f ~name:"alpha" ~value:(vec1 _alpha);
      _shader_program#set_uniform1f ~name:"ambient" ~value:(vec1 _ambient_coeff);
      _shader_program#set_uniform1i ~name:"specular_expo" ~value:(vec1 _specular_expo);
      _shader_program#set_uniform1f ~name:"light_intensity" ~value:(vec1 _light_intensity);
      _shader_program#set_uniform3f ~name:"light_pos" ~value:_light_position;
      _shader_program#set_uniform3f ~name:"light_col" ~value:_light_color;
      super#prep_render ~uniforms
      
  end

class material_array ~nb_of_slots ~(slot_names : string list) ?(materials : material list option) () =
  (*generates a default material to provide as initialisation value to the array*)
  let def_mat = new generic_material ~shader_program:(default_shader#get) in
  object (self)
    inherit [material] named_array ~nb_of_slots ~slot_names ~default_content:def_mat ~contents:(match materials with | Some a -> a | None -> []) ()
  end

let texture_shader = new default ~init:(fun x ->(
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"defaults/shaders/3D_textured_mesh_vert.glsl" in
  let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"defaults/shaders/3D_textured_mesh_frag.glsl" in
  new shader_program ~vertex_shader ~fragment_shader
)) ~delete:(fun x -> x#delete ())

let make_3D_textured_mesh_material_array ~slot_names ~textures_paths =
  let n = List.length slot_names in
  let rec gen_tex tex_l = match tex_l with
    | [] -> []
    | b::v -> (new texture ~path:b)::(gen_tex v) in
  let rec gen_mats texs_paths i mats = match texs_paths with
    | [] -> mats
    | [a] -> (
      let texs = gen_tex a in
      gen_mats
        (if i <> (n-1) then (logger Warning "make_3D_textured_mesh_material_array: missing textures, defaulting to default texture."; (List.init (n-(i+2)) (fun x -> ["defaults/textures/default.png"]))) else []) 
        (i+1) ((new textured_material ~shader_program:(texture_shader#get) ~textures:texs)::mats))
    | a::q -> (
      let texs = gen_tex a in
      gen_mats q (i+1) ((new textured_material ~shader_program:(texture_shader#get) ~textures:texs)::mats))
  in let mats = gen_mats textures_paths 0 [] in
  new material_array ~nb_of_slots:n ~slot_names ~materials:((List.rev mats):>material list) ()


let make_single_material_array ~slot_names ~material =
  let n = List.length slot_names in
  new material_array ~nb_of_slots:n ~slot_names ~materials:(List.init n (fun x -> material)) ()
