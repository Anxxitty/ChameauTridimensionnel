open Logger
open Tgl3
open Math
open Bigarray
open Graphics
open Data_structures
open Material
open Shader


(*Helper parse functions*)
let rec parse_float_list data bigarray index = match data with
  | [] -> ()
  | a::q -> bigarray.{index} <- float_of_string a; parse_float_list q bigarray (index+1)

let copy_vecn ba1 ba2 index1 index2 n =
  for i=0 to (n-1) do
    ba1.{n*index1+i} <- ba2.{n*index2+i}
  done


(*Returns a triplet of indices containing the processed data at the top of the indices*)
(*Since it adds recursively at the beginning of the input indices, it effectively reverses the input unprocessed data*)
(*"1// 2// 3// \n" and "4// 5// 6// \n" processed one after the other will result in [5;4;3;2;1;0],[],[]*)
let parse_face_triangles data (vert_indices,tex_indices,norm_indices) = 
  let rec aux data (vert_indices,tex_indices,norm_indices) = match data with
    | [] -> vert_indices,tex_indices,norm_indices
    | a::q ->
      let split_data = String.split_on_char '/' a in
      let v_index = List.hd split_data in
      let vt_index = try List.hd (List.tl split_data) with e -> "" in (*try with is necessary to catch errors that occur if the face input contains less than two slashes (can happen apparently...) -> then the missing input defaults to "" just like if it was voluntarily ommitted with the v//vn input for example*)
      let vn_index = try List.hd (List.tl (List.tl split_data)) with e -> "" in
      let v = (int_of_string v_index - 1)::vert_indices in                                         (*The -1 are because .obj starts indexing at 1, not 0...*)
      let vt = if vt_index = "" then tex_indices else (int_of_string vt_index - 1)::tex_indices in
      let vn = if vn_index = "" then norm_indices else (int_of_string vn_index - 1)::norm_indices in
      aux q (v,vt,vn)
  in aux data (vert_indices,tex_indices,norm_indices)
(*Note to self: not sure normals and texture coordinates are supposed to be processed like this at all*)
(*will adapt when I'll implement textures and need normals*)


(*Cuts the input quad in two triangles and processes them*)
let parse_face_quads data (vert_indices,tex_indices,norm_indices) =
  let rec triangle_from_quad quad excluded_index i = match quad with
    | [] -> []
    | a::q -> if i = excluded_index then triangle_from_quad q excluded_index (i+1) else a::(triangle_from_quad q excluded_index (i+1)) in
  (*In a quad formed by points 0,1,2,3 there are two triangles: 0,1,2 and 0,2,3*)
  let first_triangle = triangle_from_quad data 3 0 in
  let second_triangle = triangle_from_quad data 1 0 in
  let v,vt,vn = parse_face_triangles first_triangle (vert_indices,tex_indices,norm_indices) in
  parse_face_triangles second_triangle (v,vt,vn)


(*Takes a list of strings containing lines of a .obj file and removes leading/trailing/excess whitespaces*)
(*Discards lines with useless information*)
(*It normalizes the input for the parser*)
let rec format_model_source model_source = match model_source with
  | [] -> []
  | a::q ->
    if (String.starts_with ~prefix:"v" a || String.starts_with ~prefix:"vt" a || String.starts_with ~prefix:"vn" a || String.starts_with ~prefix:"f" a || String.starts_with ~prefix:"usemtl" a)
    then (
      let split_line = String.split_on_char ' ' (String.trim a) in (*removing leading and trailing whitespaces, splitting on whitespaces*)
      (*removes empty strings created by String.split_on_char when there are successive whitespaces*)
      let rec remove_empty_string l = match l with
        | [] -> []
        | a::q -> if a = "" then remove_empty_string q else a::remove_empty_string q 
      in remove_empty_string split_line::format_model_source q)
    else format_model_source q (*discard line if it starts by an unsupported prefix*)


(*Parses a well-formatted source written in .obj syntax*)
(*Does not support vp parameters*)
let parse source =
  (*creating output bigarrays*)
  let rec determine_bigarray_sizes l nb_vertices nb_normals nb_tex_coords tex_coord_format = match l with
    | [] -> nb_vertices, nb_normals, nb_tex_coords, tex_coord_format
    | a::q -> if (List.hd a) = "v" then determine_bigarray_sizes q (nb_vertices+1) nb_normals nb_tex_coords tex_coord_format
              else if (List.hd a) = "vn" then determine_bigarray_sizes q nb_vertices (nb_normals+1) nb_tex_coords tex_coord_format
              (*tex_coord_format := nb of coordinates in a tex coord (u; u,v or u,v,w)*)
              (*If a vt element is longer than tex_coord_format + 1 (+1 for the "vt") it means we've reached a vt with strictly more coordinates than the previous one -> we need to increase the tex_coord_format*)
              else if (List.hd a) = "vt" then determine_bigarray_sizes q nb_vertices nb_normals (nb_tex_coords+1) (if List.length a > (tex_coord_format + 1) then tex_coord_format + 1 else tex_coord_format)
              else determine_bigarray_sizes q nb_vertices nb_normals nb_tex_coords tex_coord_format in
  let number_of_vertices, number_of_normals, number_of_tex_coords, tex_coord_format = determine_bigarray_sizes source 0 0 0 1 in
  let v = create_bigarray Float32 number_of_vertices 4 in
  let vn = create_bigarray Float32 number_of_normals 3 in
  let vt = create_bigarray Float32 number_of_tex_coords tex_coord_format in

  (*parsing*)
  let rec fill_bigarrays source v_index vt_index vn_index (vert_indices,tex_indices,norm_indices) material_index material_names = match source with (*v_index vn_index and vt_index represent where we are writing in the bigarray*)
    | [] -> (List.rev vert_indices),(List.rev tex_indices),(List.rev norm_indices),(List.rev material_index),(List.rev material_names) (*we reverse the lists because new faces were added in front, and in reverse because of how parse_face_triangles works*)
    | a::q -> (
      let prefix = List.hd a in
      let data = List.tl a in
      let data_length = List.length data in

      if prefix = "v" then (
        parse_float_list data v v_index;
        if data_length = 3 then v.{v_index + 3} <- 1.0; (*if the w coordinate was ommitted it gets defaulted to 1.0*)
        fill_bigarrays q (v_index+4) vt_index vn_index (vert_indices,tex_indices,norm_indices) material_index material_names)
      else if prefix = "vt" then (
        parse_float_list data vt vt_index;
        if data_length < tex_coord_format then (*if the v or w coordinates were ommitted they get defaulted to 0.0*)
          for i = 0 to (tex_coord_format - data_length) do
            vt.{vt_index + data_length + i} <- 0.0
          done;
        fill_bigarrays q v_index (vt_index+tex_coord_format) vn_index (vert_indices,tex_indices,norm_indices) material_index material_names)
      else if prefix = "vn" then (
        parse_float_list data vn vn_index;
        fill_bigarrays q v_index vt_index (vn_index+3) (vert_indices,tex_indices,norm_indices) material_index material_names)
      else if prefix = "f" then (
        let new_vert_indices,new_tex_indices,new_norm_indices =
          if data_length = 4 then parse_face_quads data (vert_indices,tex_indices,norm_indices)
          else if data_length = 3 then parse_face_triangles data (vert_indices,tex_indices,norm_indices)
          else (vert_indices,tex_indices,norm_indices) in
        fill_bigarrays q v_index vt_index vn_index (new_vert_indices,new_tex_indices,new_norm_indices) material_index material_names)
      else if prefix = "usemtl" then
        fill_bigarrays q v_index vt_index vn_index (vert_indices,tex_indices,norm_indices) (List.length vert_indices::material_index) ((List.hd data)::material_names)
      else fill_bigarrays q v_index vt_index vn_index (vert_indices,tex_indices,norm_indices) material_index material_names) in (*if a line not matching the supported prefixes slipped in input data, it just gets ignored*)
  
  let vert_indices,tex_indices,norm_indices,material_index,material_names = fill_bigarrays source 0 0 0 ([],[],[]) [] [] in
  (*returning*)
  number_of_vertices, v, vt, vn, vert_indices, tex_indices, norm_indices, tex_coord_format, (if material_index = [] then [0] else material_index), (if material_names = [] then ["material"] else material_names) (*if the obj file does not use materials, a default material slot is created, starting at index 0, called "material"*)


class model ~path ~with_tex_and_normals =
  (*Reading the .obj file at 'path'*)
  (*Returns a list of strings containing each line of the .obj file*)
  let model_source =
    try
      let file = open_in path in
      let rec read_file new_line acc = match new_line with
        | Some line -> (
          try
            let next_line = input_line file in
            read_file (Some next_line) (line::acc)
          with
            | End_of_file -> read_file None (line::acc)
            | e -> raise e)
        | None -> acc
      in List.rev (read_file (Some "") [])
    with e ->
      logger Warning ("model: Failed to read model file at path \""^path^"\".");
      logger Warning ("model: Exception: "^(Printexc.to_string e));
      [] in
  
  (*parsing the source*)
  let nb_vertices, vert_coords, tex_coords, norm_coords, vert_indices, tex_indices, norm_indices, tex_coord_format, material_index, material_names =
    try 
      parse (format_model_source model_source)
    with e ->
      logger Warning ("model: Failed to parse model at path \""^path^"\". The file may contain unsupported .obj formatting. Try exporting it with Blender for consistent syntax.");
      logger Debug ("model: parsing error: "^(Printexc.to_string e));
      0, empty_bigarray, empty_bigarray, empty_bigarray, [], [], [], 0, [], [] in
  
  (*reindexing*)
  (*face data in .obj is stored as vertex_coord/tex_coord/norm while in OpenGL, one vertex must contain (vertex_coord,tex_coord,norm_coord) and there is only one vertex index*)
  (*thus if the extracted data is to be used with texture coordinates and normals, it needs to be reindexed so that each vertex has its own texture and normal coordinates*)
  let nb_vertices_after_reindexing, nb_drawn_vertices, vert_coords_after_reindexing, tex_coords_after_reindexing, norm_coords_after_reindexing, index, vertex_attributes =
    if with_tex_and_normals then (
      (*A unique index list is created, it contains vector-like quadruplets (v,t,n,i)*)
      (*v is for vertex index, t for texture index, n for normal index, and i for original place in the vertex index in the obj file*)
      (*mi stores the current used material*)
      (*the v,t,n indices will be used to retrieve the correct data in the non-reindexed bigarrays*)
      (*the i index will be used to put the final index in order, thus preserving faces*)
      let rec build_vectorialized_indices_list acc i indices = match indices with
          | ([],[],[]) -> acc
          | (v::vert_ind, t::tex_ind, n::norm_ind) -> 
            build_vectorialized_indices_list ((v,t,n,i)::acc) (i+1) (vert_ind,tex_ind,norm_ind)
          | _ -> logger Warning ("model: Incomplete texture or normal coordinates in file: \""^path^"\" ; the model will probably not display at all."); [] in

      (*to sort the vectorialized index without touching to the i index*)
      let compare_lexico (v,t,n,i) (v',t',n',i') =
        if v > v' then 1 else if v < v' then -1
        else if t > t' then 1 else if t < t' then -1
        else if n > n' then 1 else if n < n' then -1
        else 0 in
      
      (*to sort the final index with regards to i*)
      let compare_first_el (i,data) (i',data') = if i > i' then 1 else if i < i' then -1 else 0 in 
      
      (*to extract the index out of the list of tuples containing (i, index)*)
      let rec list_of_snd_el l = match l with
        | [] -> []
        | a::q -> snd a::list_of_snd_el q in
      
      (*sorts lexicographically the vectorialized index*)
      (*that allows for efficient detection of vertices that need to be duplicated / ones that do not*)
      let sorted_vectorialized_indices_list = List.sort compare_lexico (build_vectorialized_indices_list [] 0 (vert_indices,tex_indices,norm_indices)) in
      
      (*builds final vertex index by returning a list of increasing indices each time a new unique (v,t,n) triplet is encountered, paired with the corresponding i index*)
      (*The i index is then used to sort the vertex index to restore face order and then the final vertex index is extracted with list_of_snd_el*)
      (*the function also returns (the last index + 1) which represents the number of unique (v,t,n) elements, ie the final number of vertices in the array that will be sent to OpenGL*)
      (*That is where the lexicographic order comes in handy: duplicates are all next to each other and thus easy to eliminate*)
      let rec build_vertex_index vectorialized_indices same_as_previous acc index = match vectorialized_indices with
        | [] -> list_of_snd_el (List.sort compare_first_el acc), (index+1)
        | [(v,t,n,i)] -> if same_as_previous then build_vertex_index [] false ((i,index)::acc) index else build_vertex_index [] false ((i,index+1)::acc) (index+1)
        | (v,t,n,i)::next::q ->
            let same_as_next = (compare_lexico (v,t,n,i) next = 0) in
            if same_as_previous then build_vertex_index (next::q) same_as_next ((i,index)::acc) index
            else build_vertex_index (next::q) same_as_next ((i,index+1)::acc) (index+1) in
          
      (*same_as_previous is initialized as true so that the first index does not start at 1*)
      let processed_vertex_index, size_of_reindexed_buffer = build_vertex_index sorted_vectorialized_indices_list true [] 0 in
            
      (*creating rightly sized buffers containing the reindexed data*)
      let reindexed_vertex_coords = create_bigarray Float32 size_of_reindexed_buffer 4 in
      let reindexed_tex_coords = create_bigarray Float32 size_of_reindexed_buffer tex_coord_format in
      let reindexed_norm_coords = create_bigarray Float32 size_of_reindexed_buffer 3 in
      
      (*the final buffers are filled following the same logic as the one used to create the final vertex index*)
      (*the list of (v,t,n) is run through, ignoring duplicates, and writing correponding data of unique triplets in final buffers*)
      (*as it is run through the same way it was for building the index, it will write the right data at the right place*)
      (*here the i index is of no use*)
      let rec fill_reindexed_buffers vectorialized_indices same_as_previous data_index = match vectorialized_indices with
        | [] -> ()
        | [(v,t,n,i)] -> if same_as_previous then () else (
            copy_vecn reindexed_vertex_coords vert_coords data_index v 4;
            copy_vecn reindexed_tex_coords tex_coords data_index t tex_coord_format;
            copy_vecn reindexed_norm_coords norm_coords data_index n 3
        )
        | (v,t,n,i)::next::q -> let same_as_next = (compare_lexico (v,t,n,i) next = 0) in
            if same_as_previous then fill_reindexed_buffers (next::q) same_as_next data_index
            else (
              copy_vecn reindexed_vertex_coords vert_coords data_index v 4;
              copy_vecn reindexed_tex_coords tex_coords data_index t tex_coord_format;
              copy_vecn reindexed_norm_coords norm_coords data_index n 3;
              fill_reindexed_buffers (next::q) same_as_next (data_index+1)
            ) in
      
      fill_reindexed_buffers sorted_vectorialized_indices_list false 0;
  
      (*creating OpenGL buffers containing the model*)
      let vertex_coords_attrib = new vertex_attribute ~attribute_type:(Vector4_kind Float32) ~number_of_vertices:size_of_reindexed_buffer () in
      
      let tex_attrib_type = match tex_coord_format with
        | 1 -> Vector1_kind Float32
        | 2 -> Vector2_kind Float32
        | 3 -> Vector3_kind Float32
        | _ -> logger Warning ("model: Unsupported texture coordinates format in model file at path \""^path^"\""); Vector4_kind Float32 in
      let tex_coords_attrib = new vertex_attribute ~attribute_type:tex_attrib_type ~number_of_vertices:size_of_reindexed_buffer () in

      let norm_coords_attrib = new vertex_attribute ~attribute_type:(Vector3_kind Float32) ~number_of_vertices:size_of_reindexed_buffer () in
      
      vertex_coords_attrib#set_raw_data reindexed_vertex_coords;
      tex_coords_attrib#set_raw_data reindexed_tex_coords;
      norm_coords_attrib#set_raw_data reindexed_norm_coords;

      (size_of_reindexed_buffer, List.length processed_vertex_index, reindexed_vertex_coords, reindexed_tex_coords, reindexed_norm_coords, processed_vertex_index, [vertex_coords_attrib;tex_coords_attrib;norm_coords_attrib]) )
      else (
        let vertex_coords_attrib = new vertex_attribute ~attribute_type:(Vector4_kind Float32) ~number_of_vertices:nb_vertices () in
        vertex_coords_attrib#set_raw_data vert_coords;
        (*If the model is loaded without normals and texture coordinates, they are set to empty bigarrays*)
        (nb_vertices, List.length vert_indices, vert_coords, empty_bigarray, empty_bigarray, vert_indices, [vertex_coords_attrib])
      ) in

  (*the material_names list contains what material is used for all faces between two adjacent indices in material_index*)
  (*the same material can be used multiple times for different parts of a mesh*)
  (*therefore material_names does not describe the unique material slots used to group all vertices that can be drawn at the same time (basically vertices that have the same material)*)
  (*material_slots is the list that describes these unique mesh slots for each material used*)
  let rec rem_duplicates l acc = match l with
    | [] -> List.rev acc
    | a::q -> if List.mem a acc
      then rem_duplicates q acc
      else rem_duplicates q (a::acc) in
  let material_slots = rem_duplicates material_names [] in

  (*using material_slots, an array containing each submesh is created (a submesh is represented by its face index)*)
  let sliced_indices_array = new named_array ~nb_of_slots:(List.length material_slots) ~slot_names:material_slots ~default_content:[] () in

  (*the array is filled by slicing the face index representing the whole object at the indices in material_index*)
  (*these indices represent where there was an occurence of "usemtl", so a change in material*)
  (*up until the next encounter of an index in material_index, the faces get added to the material slot correponding to the material in use (which is indicated by the list material_names)*)
  let rec slice_index ind i mats = match ind with
    | [] -> ()
    | a::q -> (
      let mat = List.hd mats in
      let index = sliced_indices_array#get_content ~name:(mat) in
      sliced_indices_array#set_content ~name:(mat) ~content:(a::index);
      slice_index q (i+1) (if (List.mem (i+1) material_index) then (if List.tl mats = [] then mats else List.tl mats) else mats)
    ) in let () = slice_index index 0 material_names in
  
  let rec reverse_sliced_index mats = match mats with
    | [] -> ()
    | mat::q -> (
      let index = sliced_indices_array#get_content ~name:(mat) in
      sliced_indices_array#set_content ~name:(mat) ~content:(List.rev index);
      reverse_sliced_index q
    ) in let () = reverse_sliced_index material_slots in (*when sliced, index lists are reversed, so they are reversed back so that faces are drawn in the right order*)


  let rec generate_opengl_objects slots ebos vaos = match slots with
    | [] -> List.rev ebos, List.rev vaos (*reversed so that they are in the same order as material_slots*) (*otherwise the wrong vaos would be associated to the wrong material slots*)
    | a::q -> (
      let ind = sliced_indices_array#get_content ~name:(a) in
      let ebo = new buffer ~buffer_type:Gl.element_array_buffer ~kind:Int16_unsigned in
      ebo#write_element_buffer ~index:ind ~usage:Gl.static_draw;
      let vao = new vertex_array ~kind:Float32 ~vertex_attributes ~element_buffer:ebo ~number_of_drawn_vertices:(List.length ind) ~drawing_type:Gl.static_draw in
      generate_opengl_objects q (ebo::ebos) (vao::vaos)
    ) in
  
  let ebos, vaos = generate_opengl_objects material_slots [] [] in

  let dummy_vao = new vertex_array ~kind:Float32 ~vertex_attributes:[] ~element_buffer:(new buffer ~buffer_type:Gl.element_array_buffer ~kind:Int16_unsigned) ~number_of_drawn_vertices:0 ~drawing_type:Gl.static_draw in
  let vaos_array = new named_array ~nb_of_slots:(List.length material_slots) ~slot_names:material_slots ~default_content:dummy_vao ~contents:vaos () in

  object (self)
    val _tot_number_of_vertices = nb_vertices_after_reindexing
    val _tot_number_of_drawn_vertices = nb_drawn_vertices
    val _vertex_coordinates = vert_coords_after_reindexing
    val _texture_coordinates = tex_coords_after_reindexing
    val _normal_coordinates = norm_coords_after_reindexing
    val _unsliced_vertex_index = index
    val _vaos = vaos_array
    val _material_slot_names = material_slots
    method get_vertex_coordinates = _vertex_coordinates
    method get_texture_coordinates = _texture_coordinates
    method get_normal_coordinates = _normal_coordinates
    method get_unsliced_vertex_index = _unsliced_vertex_index
    method get_tot_nb_vertices = _tot_number_of_vertices
    method get_tot_nb_drawn_vertices = _tot_number_of_drawn_vertices
    method get_vaos = _vaos
    method get_material_slot_names = _material_slot_names
    method get_nb_material_slots = List.length _material_slot_names
    method delete () =
      let rec del objs = match objs with
        | [] -> ()
        | a::q -> a#delete (); del q in
      del ebos;
      let rec del_arr names = match names with
        | [] -> ()
        | a::q -> ((_vaos#get_content ~name:a)#delete (); del_arr q) in
      del_arr (_vaos#get_slot_names);
      dummy_vao#delete ()
  end
