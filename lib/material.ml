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

let initialize_materials () =
  List.iter (fun m -> m#init ()) (!material_registry)

let delete_materials () =
  List.iter (fun m -> m#delete ()) (!material_registry)

class generic_material ~shader_program =
  object (self)
    inherit material
    val _shader_program = shader_program
    method get_shader_program =
      _shader_program
    method prep_render ~uniforms =
      _shader_program#use ();
      List.iter (fun u -> _shader_program#set_uniform u) uniforms;
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

let default_texture = new default ~init:(fun () -> new texture ~path:"defaults/textures/default.png") ~delete:(fun t -> t#delete ())

class textured_material ~shader_program ~(textures : texture list) = 
  object (self)
    inherit generic_material ~shader_program as super
    val mutable _textures = textures
    method get_textures =
      _textures
    method set_textures texs =
      _textures <- texs
    method! prep_render ~uniforms =
      _shader_program#use ();
      let rec bind_textures l i = match l with
        | [] -> ()
        | a::q -> 
          (*OpenGL does not provide more than 16 texture units*)
          if i > 15 then logger Warning "textured_material: exceeded the maximum of 16 texture units. Some textures will not display correctly."
          else (
            Gl.active_texture (Gl.texture0 + i);
            a#bind ();
            _shader_program#set_uniform1i ~name:("texture"^(string_of_int i)) ~value:(vec1 i); (*Changed vec1 1 to vec1 i because im suspecting it was a typo... not sure!*)
            bind_textures q (i+1) 
          ) in
      bind_textures textures 0;
      super#prep_render ~uniforms
  end

let phong_shader = new default ~init:(fun x ->(
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"defaults/shaders/blinn_phong_shader/blinn_phong_shader.vert" in
  let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"defaults/shaders/blinn_phong_shader/blinn_phong_shader.frag" in
  new shader_program ~vertex_shader ~fragment_shader ()
)) ~delete:(fun x -> x#delete ())

let default_specular = new default ~init:(fun () -> new texture ~path:"defaults/textures/default_specular.png") ~delete:(fun t -> t#delete ())
let default_emission = new default ~init:(fun () -> new texture ~path:"defaults/textures/default_emission.png") ~delete:(fun t -> t#delete ())
let default_normal_map = new default ~init:(fun () -> new texture ~path:"defaults/textures/default_normal_map.png") ~delete:(fun t -> t#delete ())

class textured_phong_material ~(diffuse : texture) ?(specular = default_specular#get) ?(emission = default_emission#get) ?(emission_coeff = 1.0) ?(normal_map = default_normal_map#get) ~(shininess : float) () =
  object (self)
    inherit generic_material ~shader_program:(phong_shader#get) as super
    val mutable _diffuse = diffuse
    val mutable _specular = specular
    val mutable _emission = emission
    val mutable _emission_coeff = emission_coeff
    val mutable _normal_map = normal_map
    val mutable _shininess = shininess
    (*getters and setters*)
    method get_textures =
      [_diffuse; _specular; _emission; normal_map]
    method get_shininess =
      _shininess
    method get_specular =
      _specular
    method get_emission =
      _emission
    method get_diffuse =
      _diffuse
    method get_normal_map =
      _normal_map
    method get_emission_coeff =
      _emission_coeff
    method set_emission x =
      _emission <- x
    method set_diffuse x =
      _diffuse <- x
    method set_normal_map x =
      _normal_map <- x
    method set_specular x =
      _specular <- x
    method set_shininess x =
      _shininess <- x
    method set_emission_coeff x =
      _emission_coeff <- x
    
    method! prep_render ~uniforms =
      _shader_program#use ();
      List.iteri (fun i (tex, name) -> (
        Gl.active_texture (Gl.texture0 + i);
        tex#bind ();
        _shader_program#set_uniform1i ~name ~value:(vec1 i)
      )) [
        (_emission, "material.emission");
        (_diffuse, "material.diffuse");
        (_specular, "material.specular");
        (_normal_map, "material.normal_map")
      ];
      _shader_program#set_uniform1f ~name:"material.shininess" ~value:(vec1 _shininess);
      _shader_program#set_uniform1f ~name:"material.emission_coeff" ~value:(vec1 _emission_coeff);
      super#prep_render ~uniforms    
  end

class material_array ~nb_of_slots ~(slot_names : string list) ?(materials : material list option) () =
  (*generates a default material to provide as initialisation value to the array*)
  let def_mat = new generic_material ~shader_program:(default_shader#get) in
  object (self)
    inherit [material] named_array ~nb_of_slots ~slot_names ~default_content:def_mat ~contents:(match materials with | Some a -> a | None -> []) ()
  end

let texture_shader = new default ~init:(fun () -> (
  let vertex_shader = new shader ~shader_type:Gl.vertex_shader ~shader_source_path:"defaults/shaders/texture_shader/texture_shader.vert" in
  let fragment_shader = new shader ~shader_type:Gl.fragment_shader ~shader_source_path:"defaults/shaders/texture_shader/texture_shader.frag" in
  new shader_program ~vertex_shader ~fragment_shader ()
)) ~delete:(fun x -> x#delete ())

let make_3D_textured_mesh_material_array ~slot_names ~textures_paths =
  let n = List.length slot_names in
  let rec gen_tex tex_l = match tex_l with
    | [] -> []
    | b::v -> (match b with | None -> default_texture#get | Some b -> new texture ~path:b)::(gen_tex v) in
  let rec gen_mats texs_paths i mats = match texs_paths with
    | [] -> mats
    | [a] -> (
      let texs = gen_tex a in
      gen_mats
        (if i <> (n-1) then (logger Warning "make_3D_textured_mesh_material_array: missing textures, defaulting to default texture."; (List.init (n-(i+2)) (fun x -> [None]))) else []) 
        (i+1) ((new textured_material ~shader_program:(texture_shader#get) ~textures:texs)::mats))
    | a::q -> (
      let texs = gen_tex a in
      gen_mats q (i+1) ((new textured_material ~shader_program:(texture_shader#get) ~textures:texs)::mats))
  in let mats = gen_mats (List.map (fun a -> List.map (fun t -> Some t) a) textures_paths) 0 [] in
  new material_array ~nb_of_slots:n ~slot_names ~materials:((List.rev mats) :> material list) ()


let make_single_material_array ~slot_names ~material =
  let n = List.length slot_names in
  new material_array ~nb_of_slots:n ~slot_names ~materials:(List.init n (fun x -> material)) ()

let make_textured_phong_material_from_paths ~diffuse ?(specular = None) ?(emission = None) ?(normal_map = None) =
  new textured_phong_material
    ~diffuse:(new texture ~path:diffuse)
    ~specular:(Option.fold specular ~some:(fun a -> new texture ~path:a) ~none:default_specular#get)
    ~emission:(Option.fold emission ~some:(fun a -> new texture ~path:a) ~none:default_emission#get)
    ~normal_map:(Option.fold normal_map ~some:(fun a -> new texture ~path:a) ~none:default_normal_map#get)
