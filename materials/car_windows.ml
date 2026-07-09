open ChameauTridimensionnel
open Material
open Math
open Data_structures

type material_type = textured_phong_material

let material = make_material 
  ~init:(fun () -> 
    let car_exterior_texture = new texture ~path:"assets/textures/car_exterior.jpeg" in
    new textured_phong_material ~textures:[car_exterior_texture] ~alpha:0.5 ~ambient_coeff:0.3 ~specular_expo:128 ~light_position:(vec3 0.0 0.0 0.0) ~light_color:(vec3 1.0 1.0 1.0) ~light_intensity:10.0)
  ~delete:(fun m -> List.iter (fun t -> t#delete ()) m#get_textures)
