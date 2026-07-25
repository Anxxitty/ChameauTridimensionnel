open ChameauTridimensionnel
open Material
open Math
open Data_structures

type material_type = textured_phong_material

let material = make_material 
  ~init:(fun () -> 
    make_textured_phong_material_from_paths
      ~diffuse:"assets/textures/brick.png"
      ~shininess:32.0
      ())
  ~delete:(fun m -> List.iter (fun t -> t#delete ()) m#get_textures)
