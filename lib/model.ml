open Logger
open Tgl3
open Math
open Bigarray
open Asset
open Bigarray_helper


(*Helper parse functions*)
let rec parse_float_list data bigarray index = match data with
  | [] -> ()
  | a::q -> bigarray.{index} <- float_of_string a; parse_float_list q bigarray (index+1)


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
    if (String.starts_with ~prefix:"v" a || String.starts_with ~prefix:"vt" a || String.starts_with ~prefix:"vn" a || String.starts_with ~prefix:"f" a)
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
  let numberOfVertices, numberOfNormals, numberOfTexCoords, texCoordFormat = determine_bigarray_sizes source 0 0 0 1 in
  let v = create_bigarray Float32 numberOfVertices 4 in
  let vn = create_bigarray Float32 numberOfNormals 3 in
  let vt = create_bigarray Float32 numberOfTexCoords texCoordFormat in

  (*parsing*)
  let rec fill_bigarrays source v_index vt_index vn_index (vert_indices,tex_indices,norm_indices) = match source with (*v_index vn_index and vt_index represent where we are writing in the bigarray*)
    | [] -> (List.rev vert_indices),(List.rev tex_indices),(List.rev norm_indices) (*we reverse the lists because new faces were added in front, and in reverse because of how parse_face_triangles works*)
    | a::q -> (
      let prefix = List.hd a in
      let data = List.tl a in
      let data_length = List.length data in

      if prefix = "v" then (
        parse_float_list data v v_index;
        if data_length = 3 then v.{v_index + 3} <- 1.0; (*if the w coordinate was ommitted it gets defaulted to 1.0*)
        fill_bigarrays q (v_index+4) vt_index vn_index (vert_indices,tex_indices,norm_indices))
      else if prefix = "vt" then (
        parse_float_list data vt vt_index;
        if data_length < texCoordFormat then (*if the v or w coordinates were ommitted they get defaulted to 0.0*)
          for i = 0 to (texCoordFormat - data_length) do
            vt.{vt_index + data_length + i} <- 0.0
          done;
        fill_bigarrays q v_index (vt_index+texCoordFormat) vn_index (vert_indices,tex_indices,norm_indices))
      else if prefix = "vn" then (
        parse_float_list data vn vn_index;
        fill_bigarrays q v_index vt_index (vn_index+3) (vert_indices,tex_indices,norm_indices))
      else if prefix = "f" then (
        let vertIndices,texIndices,normIndices =
          if data_length = 4 then parse_face_quads data (vert_indices,tex_indices,norm_indices)
          else if data_length = 3 then parse_face_triangles data (vert_indices,tex_indices,norm_indices)
          else (vert_indices,tex_indices,norm_indices) in
        fill_bigarrays q v_index vt_index vn_index (vertIndices,texIndices,normIndices))
      else fill_bigarrays q v_index vt_index vn_index (vert_indices,tex_indices,norm_indices)) in (*if a line not matching the supported prefixes slipped in input data, it just gets ignored*)
  
  let vert_indices,tex_indices,norm_indices = fill_bigarrays source 0 0 0 ([],[],[]) in
  (*returning*)
  numberOfVertices, v, vt, vn, vert_indices, tex_indices, norm_indices


class model ~path =
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
  let nb_vertices, v, vt, vn, vert_indices, tex_indices, norm_indices =
    try 
      parse (format_model_source model_source)
    with e ->
      logger Warning ("model: Failed to parse model at path \""^path^"\". The file may contain unsupported .obj formatting. Try exporting it with Blender for consistent syntax.");
      let empty_bigarray = create_bigarray Float32 0 0 in
      0, empty_bigarray, empty_bigarray, empty_bigarray, [], [], [] in
  
  (*creating OpenGL buffers containing the model*)
  let vertex_coords_attrib = new vertex_attribute ~attribute_type:(Vector4_kind Float32) ~number_of_vertices:nb_vertices () in

  let ebo = new buffer ~buffer_type:Gl.element_array_buffer ~kind:Int16_unsigned in
  let () = (
    ebo#write_element_buffer ~indices:vert_indices ~usage:Gl.static_draw;
    vertex_coords_attrib#set_raw_data v
  ) in

  let vao = new vertex_array ~kind:Float32 ~vertex_attributes:[vertex_coords_attrib] ~element_buffer:ebo ~drawing_type:Gl.static_draw in

  object (self)
    val _number_of_vertices = nb_vertices
    val _number_of_drawn_vertices = List.length vert_indices
    val _vertex_coordinates = v
    val _texture_coordinates = vt
    val _normal_coordinates = vn
    val _vertex_indices = vert_indices
    val _texture_indices = tex_indices
    val _normal_indices = norm_indices
    val _vao = vao
    method get_vertex_coordinates = _vertex_coordinates
    method get_texture_coordinates = _texture_coordinates
    method get_normal_coordinates = _normal_coordinates
    method getuvertex_indices = _vertex_indices
    method get_texture_indices = _texture_indices
    method get_normal_indices = _normal_indices
    method get_number_of_vertices = _number_of_vertices
    method get_number_of_drawn_vertices = _number_of_drawn_vertices
    method get_vao = _vao
    method delete () =
      _vao#delete ();
      ebo#delete ()
  end