open Math
open Actor
open Shader
open Logger
open Window
open Bigarray
open Bigarray_helper
open Tgl3

let get_gl_type : type a b. ((a, b) kind -> int) = function
| Int8_signed -> Gl.byte
| Int8_unsigned -> Gl.unsigned_byte
| Int16_signed -> Gl.short
| Int16_unsigned -> Gl.unsigned_short
| Int32 | Int64 | Nativeint | Int -> Gl.int
| Float16 -> Gl.half_float
| Float32 -> Gl.float
| Float64 -> Gl.double
| Char -> Gl.unsigned_byte
| _ -> Gl.float (*This is arbitrary! (it actually only covers bigarray complex kinds which are never used in this project)*)

type dimension = Dim1 | Dim2 | Dim3 | Dim4 

(*Wraps a Bigarray in a class that suits the project's vector structures, with handful methods for later defining vertex attributes*)
class ['a, 'b] vertex_attribute ~(attribute_type : ('a, 'b) vector_kind) ~number_of_vertices ?(vector_list : 'a vector list option) () =
  (*Generates the bigarray containing the vertex attribute data, extracts the size and kind parameters*)
  let kind, size, dim, bigarray = match attribute_type with
    | Vector1_kind kind -> kind, kind_size_in_bytes kind, Dim1, Array1.init kind C_layout (1 * number_of_vertices) (fun i -> (zero kind))
    | Vector2_kind kind -> kind, 2*(kind_size_in_bytes kind), Dim2, Array1.init kind C_layout (2 * number_of_vertices) (fun i -> (zero kind))
    | Vector3_kind kind -> kind, 3*(kind_size_in_bytes kind), Dim3, Array1.init kind C_layout (3 * number_of_vertices) (fun i -> (zero kind))
    | Vector4_kind kind -> kind, 4*(kind_size_in_bytes kind), Dim4, Array1.init kind C_layout (4 * number_of_vertices) (fun i -> (zero kind)) in
  (*Fills the bigarray with the given input data*)
  let () = (
    match vector_list with
      | Some vector_list_val -> (
          logger Debug "vertex_attribute: Filling vertex_attribute with data given on construction.";
          let rec fill_bigarray i init_list = match init_list with
           | [] -> ()
           | a::q -> match a with
              | Vector1 vec -> bigarray.{i} <- vec.x; fill_bigarray (i+1) q
              | Vector2 vec -> let j = 2*i in (bigarray.{j} <- vec.x; bigarray.{j+1} <- vec.y); fill_bigarray (i+1) q
              | Vector3 vec -> let j = 3*i in (bigarray.{j} <- vec.x; bigarray.{j+1} <- vec.y; bigarray.{j+2} <- vec.z); fill_bigarray (i+1) q
              | Vector4 vec -> let j = 4*i in (bigarray.{j} <- vec.x; bigarray.{j+1} <- vec.y; bigarray.{j+2} <- vec.z; bigarray.{j+3} <- vec.w); fill_bigarray (i+1) q
          in fill_bigarray 0 vector_list_val)
      | None -> ()
  ) in
  object (self)
    (*contains the actual vertex attribute data*)
    val mutable _data = bigarray
    (*contains the type of the data as a bigarray kind*)
    val _kind = kind
    (*defines if the vertex attribute contains 1- 2- 3- or 4-dimensional vectors*)
    val _dimension = dim
    (*the overall size of ONE vertex attribute (ex: for a vertex_attribute of type Float32 Vertex4, size = size(Float32)*4 )*)
    val _size = size
    (*the number of vertices contained in data*)
    val _number_of_vertices = number_of_vertices
    (*simple getter methods*)
    method get_kind =
      _kind
    method get_dimension = match _dimension with
      | Dim1 -> 1
      | Dim2 -> 2
      | Dim3 -> 3
      | Dim4 -> 4
    method get_number_of_vertices =
      _number_of_vertices
    method get_raw_data =
      _data
    method set_raw_data ba =
      _data <- ba
    method get_size =
      _size
    (*returns the vertex_attribute of the i-th vertex*)
    method get_vertex_attribute ~index =
      match _dimension with
        | Dim1 -> Vector1 { x = _data.{index} }
        | Dim2 -> Vector2 { x = _data.{2*index}; y = _data.{2*index+1} }
        | Dim3 -> Vector3 { x = _data.{3*index}; y = _data.{3*index+1}; z = _data.{3*index+2} }
        | Dim4 -> Vector4 { x = _data.{4*index}; y = _data.{4*index+1}; z = _data.{4*index+2}; w = _data.{4*index+3} }
    (*allows for changing the vertex_attribute of the i-th vertex*)
    (*Note: the bigarray size is fixed: it is impossible to add a vertex to a vertex_attribute object*) 
    method set_vertex_attribute ~index ~value = 
      let () = logger Debug "vertex_attribute: setting a vertex_attribute" in
      match _dimension with
        | Dim1 -> (match value with 
          | Vector1 vec -> _data.{index} <- vec.x
          | _ -> logger Warning "Trying to change a 1D vertex attribute with a non-1D vector.")
        | Dim2 -> (match value with 
          | Vector2 vec -> (_data.{2*index} <- vec.x; _data.{2*index+1} <- vec.y)
          | _ -> logger Warning "Trying to change a 2D vertex attribute with a non-2D vector.")
        | Dim3 -> (match value with 
          | Vector3 vec -> (_data.{3*index} <- vec.x; _data.{3*index+1} <- vec.y; _data.{3*index+2} <- vec.z)
          | _ -> logger Warning "Trying to change a 3D vertex attribute with a non-3D vector.")
        | Dim4 -> (match value with 
          | Vector4 vec -> (_data.{4*index} <- vec.x; _data.{4*index+1} <- vec.y; _data.{4*index+2} <- vec.z; _data.{4*index+3} <- vec.w)
          | _ -> logger Warning "Trying to change a 4D vertex attribute with a non-4D vector.")
  end

(*Manages OpenGL buffer objects (like VBOs and EBOs)*)
class ['a, 'b] buffer ~buffer_type ~(kind : ('a, 'b) kind) =
  object (self)
    val _id = get_first_int (Gl.gen_buffers 1)
    (*Gl.array_buffer ; Gl.element_array_buffer...*)
    val _buffer_type = buffer_type
    (*kind of what will be stored in the buffer*)
    val _kind = kind
    (*simple getters*)
    method get_id =
      _id
    method get_buffer_type =
      _buffer_type
    method get_kind =
      _kind
    (*Binds the buffer to the correct target (to the VBO target if bufferType is VBO...)*)
    method bind () =
      Gl.bind_buffer _buffer_type _id
    (*Wrapper for the OpenGL function that writes data to a buffer (Gl.buffer_data)*)
    method write_raw_data ~(size : int) ~(data : ('a, 'b) Gl.bigarray) ~(usage : int) =
      logger Debug "buffer: Copying data into a buffer through write_raw_data.";
      self#bind ();
      Gl.buffer_data _buffer_type size (Some data) usage;
    (*Takes a list of vertex attributes and generates a well-structured data array and copies it in the buffer*)
    (*Note: Only appropriate for VBOs; don't try using this for an EBO, it would make no sense*)
    (*For a list of attributes like this: [position_vertex_attrib; color_vertex_attrib], the data would look like:*)
    (*[Vertex1;      Vertex2      ...]*)
    (*[pos;  color;  pos;  color  ...]*)
    (*[x,y,z,r,g,b,a,x,y,z,r,g,b,a...]*)
    method write_element_buffer ~(index : 'a list) ~(usage : int) =
      logger Debug "buffer: Copying data into a buffer through write_element_buffer.";
      let len = List.length index in
      let ba = create_bigarray _kind 1 len in
      let rec fill l i = match l with
        | [] -> ba
        | a::q -> (ba.{i} <- a); fill q (i+1)
      in let data = fill index 0 in
      self#write_raw_data ~size:(Gl.bigarray_byte_size data) ~data ~usage
    method write_vertex_attributes ~(vertex_attributes : (('a, 'b) vertex_attribute) list) ~(usage : int) =
      logger Debug "buffer: Copying data into a buffer through write_vertex_attributes.";
      self#bind ();
      match vertex_attributes with
        | [] -> () (*If the provided list is empty, there's nothing to do*)  
        | a::q -> (
            (*we fetch the kind and number of vertices of the first vertexAttribute to ensure the others match*)
            let number_of_vertices = a#get_number_of_vertices in
            (*scans the whole list, asserts that each vertexAttribute is of the same kind as the first one and has the same number of vertices*)
            (*returns the size of the bigarray that will contain all the data (returns 0 if the assert fails)*)
            let rec check_data_coherence_and_determine_size size_acc l = match l with
              | [] -> size_acc
              | a::q ->
                if a#get_number_of_vertices <> number_of_vertices
                  then (logger Warning "Failed to write vertex attributes to buffer: the attributes do not contain the same number of vertices."; 0)
                  else check_data_coherence_and_determine_size (size_acc + (a#get_number_of_vertices * a#get_size)) q
            in let size = check_data_coherence_and_determine_size 0 vertex_attributes in
            (*if the final bigarray is of size 0, there is nothing to copy*)
            if size = 0 then () else
            (*scans the list over and over until all vertices have been copied in the dataAcc*)
            let rec read_vertex_attributes data_acc vertex_index data_index top_pile bottom_pile = match top_pile with
              | [] -> 
                if vertex_index = (number_of_vertices - 1) (*when there are 12 vertices, they range from 0 to 11*)
                  then data_acc 
                  (*once the top pile is empty, it means we have copied all the attributes of the i-th vertex: we refill the top pile and continue with vertex i+1*)
                  else read_vertex_attributes data_acc (vertex_index+1) data_index (List.rev bottom_pile) top_pile
              | a::q -> (
                (*retrieve the attribute data of the i-th vertex in the vertexAttribute a*)
                (*this will be an n-vector of type kind*)
                (*we match on the dimension of the vector to add the data correctly to the dataAcc*)
                (*this puts in evidence the role of dataIndex: it follows where we are in dataAcc and where we should continue writing*)
                (let new_data = a#get_vertex_attribute ~index:vertex_index in match new_data with
                  | Vector1 vec -> data_acc.{data_index} <- vec.x; read_vertex_attributes data_acc vertex_index (data_index+1) q (a::bottom_pile)
                  | Vector2 vec -> data_acc.{data_index} <- vec.x; data_acc.{data_index+1} <- vec.y; read_vertex_attributes data_acc vertex_index (data_index+2) q (a::bottom_pile)
                  | Vector3 vec -> data_acc.{data_index} <- vec.x; data_acc.{data_index+1} <- vec.y; data_acc.{data_index+2} <- vec.z; read_vertex_attributes data_acc vertex_index (data_index+3) q (a::bottom_pile)
                  | Vector4 vec -> data_acc.{data_index} <- vec.x; data_acc.{data_index+1} <- vec.y; data_acc.{data_index+2} <- vec.z; data_acc.{data_index+3} <- vec.w; read_vertex_attributes data_acc vertex_index (data_index+4) q (a::bottom_pile)))
            in let data = read_vertex_attributes (create_bigarray kind size 1) 0 0 vertex_attributes [] in
            (*logger Info ("0: "^(string_of_float data.{0})^" "^"1: "^(string_of_float data.{1})^" "^"2: "^(string_of_float data.{2})^" "^"3: "^(string_of_float data.{3})^" "^"4: "^(string_of_float data.{4})^" "^"5: "^(string_of_float data.{5})^" "^"6: "^(string_of_float data.{6})^" "^"7: "^(string_of_float data.{7})^" "^"8: "^(string_of_float data.{8})^" "^"9: "^(string_of_float data.{9})^" "^"10: "^(string_of_float data.{10})^" "^"11: "^(string_of_float data.{11})^" "^"12: "^(string_of_float data.{12})^" "^"13: "^(string_of_float data.{13})^" "^"14: "^(string_of_float data.{14})^" "^"15: "^(string_of_float data.{15})^" "^"16: "^(string_of_float data.{16})^" "^"17: "^(string_of_float data.{17})^" "^"18: "^(string_of_float data.{18})^" "^"19: "^(string_of_float data.{19})^" "^"20: "^(string_of_float data.{20})^" ");*)
            (*copies the data in the buffer*)
            Gl.buffer_data _buffer_type size (Some data) usage;)
    (*Don't forget to delete !*)
    method delete () =
      Gl.delete_buffers 1 (set_first_int _id)
  end

class ['a, 'b, 'c] vertex_array ~kind ~(vertex_attributes : (('a, 'b) vertex_attribute) list) ~(element_buffer : (int, 'c) buffer) ~(drawing_type : int) =
  (*Generates the object's ID (it is a VAO)*)
  let id = get_first_int (Gl.gen_vertex_arrays 1) in
  (*Generates the VBO associated to our VAO*)
  let vbo : (('a, 'b) buffer) = new buffer ~buffer_type:Gl.array_buffer ~kind in
  (*Retrieves the number of vertices in the array*)
  let number_of_vertices = match vertex_attributes with
    | [] -> 0
    | a::q -> a#get_number_of_vertices in
  let () = (
    (*setting up the VAO*)
    Gl.bind_vertex_array id;
    vbo#bind ();
    element_buffer#bind ();
    (**copying data in the VBO*)
    vbo#write_vertex_attributes ~vertex_attributes ~usage:drawing_type;
    (**setting up the vertex attribute pointers*)
    let rec determine_stride stride_acc (l : (('a, 'b) vertex_attribute) list) = match l with
      | [] -> stride_acc
      | a::q -> determine_stride (a#get_size + stride_acc) q
    in let stride = determine_stride 0 vertex_attributes in
    let rec set_attribute_pointers index offset l = match l with
      | [] -> ()
      | a::q -> (
        Gl.vertex_attrib_pointer index a#get_dimension (get_gl_type a#get_kind) false stride (`Offset offset);
        Gl.enable_vertex_attrib_array index;
        set_attribute_pointers (index+1) (offset+a#get_size) q
      )
    in set_attribute_pointers 0 0 vertex_attributes;
    (*Do not forget to unbind the VAO to avoid modifying it outside the scope of this constructor*)
    Gl.bind_vertex_array 0
  ) in
  object (self)
    val _id = id
    val mutable _number_of_vertices = number_of_vertices
    val mutable _vertex_buffer = vbo
    val mutable _element_buffer = element_buffer
    val mutable _vertex_attributes = vertex_attributes
    val mutable _drawing_type = drawing_type
    (*getters and setters*)
    method get_id =
      _id
    method get_number_of_vertices =
      _number_of_vertices
    method get_vertex_buffer =
      _vertex_buffer
    method get_element_buffer =
      _element_buffer
    method get_vertex_attributes =
      _vertex_attributes
    method get_drawing_type =
      _drawing_type
    (*changes the vertex attributes and redraws the object*)
    method set_vertex_attributes ~(vertex_attributes : (('a, 'b) vertex_attribute) list) =
      _vertex_attributes <- vertex_attributes;
      _number_of_vertices <- (match vertex_attributes with
        | [] -> 0 
        | a::q -> a#get_number_of_vertices);
      (*Reconfiguring the VAO*)
      Gl.bind_vertex_array _id;
      (*replacing the VBO*)
      _vertex_buffer#delete ();
      _vertex_buffer <- new buffer ~buffer_type:Gl.array_buffer ~kind;
      _vertex_buffer#write_vertex_attributes ~vertex_attributes ~usage:drawing_type;
      (*setting up the new vertex attribute pointers*)
      let rec determine_stride stride_acc (l : (('a, 'b) vertex_attribute) list) = match l with
        | [] -> stride_acc
        | a::q -> determine_stride (a#get_size + stride_acc) q
      in let stride = determine_stride 0 vertex_attributes in
      let rec set_attribute_pointers index offset l = match l with
        | [] -> ()
        | a::q -> (
          Gl.vertex_attrib_pointer index a#get_dimension (get_gl_type a#get_kind) false stride (`Offset offset);
          Gl.enable_vertex_attrib_array index;
          set_attribute_pointers (index+1) (offset+a#get_size) q
        )
      in set_attribute_pointers 0 0 vertex_attributes;
      Gl.bind_vertex_array 0;
    method set_drawing_type ~drawing_type =
      _drawing_type <- drawing_type
    method bind () =
      Gl.bind_vertex_array _id
    method unbind () =
      Gl.bind_vertex_array 0;
    (*If vertexAttribute contents are changed or if drawing type is changed, this redraws the VBO accordingly*)
    method redraw () =
      _vertex_buffer#write_vertex_attributes ~vertex_attributes ~usage:drawing_type;
    method delete () =
      Gl.delete_vertex_arrays 1 (set_first_int _id);
      _vertex_buffer#delete ();
      _element_buffer#delete ()
  end

(*Wrapper for OpenGL 2D texture object*)
class texture ~path =
  (*loads the texture with stb_image*)
  let texture_image = Stb_image.load path in
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
      | Error e -> match e with `Msg e -> (logger Warning ("texture: Failed to load texture at path \""^path^"\". Error message: "^e));
    
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


class ['a, 'b, 'c] drawable ~(vertex_array : ('a, 'b, 'c) vertex_array) ~(shader_program : shader_program) ~(number_of_drawn_vertices : int) ~(scene_coordinates : float vector3) ~(local_rotation : quaternion) ~(scale : float vector3) =
  object (self)
    inherit movable ~scene_coordinates ~local_rotation ~scale
    val mutable _vertex_array = vertex_array
    val mutable _shader_program = shader_program
    val _number_of_drawn_vertices = number_of_drawn_vertices
    (*getters and setters*)
    method get_vertex_array =
      _vertex_array
    method get_shader_program =
      _shader_program
    method set_vertex_array (vertex_array : ('a, 'b, 'c) vertex_array) =
      _vertex_array <- vertex_array
    method set_shader_program (shader_program : shader_program) =
      _shader_program <- shader_program
    (*other methods*)
    method render ~(window : window) ~(uniforms : uniform list) =
      (*logger Debug_main_loop "drawable: Rendering a drawable.";*)
      _shader_program#use ();
      _vertex_array#bind ();
      let rec uniform_setter l = match l with
        | [] -> ()
        | a::q -> (
          _shader_program#set_uniform a;
          uniform_setter q)
      in uniform_setter uniforms;
      (*apply local rotation, scale and scene position*)
      let model_matrix = translation_matrix4f _coordinates *::. scale_matrix4f _scale *::. (rotation_matrix4f_from_quat _rotation) in
      _shader_program#set_uniform (Matrix_uniform4f ("model_matrix", model_matrix));
      _shader_program#use ();
      Gl.draw_elements Gl.triangles _number_of_drawn_vertices (get_gl_type _vertex_array#get_element_buffer#get_kind) (`Offset 0);
      _vertex_array#unbind ();
  end

(*Same as a drawable but handles itself its textures*)
(*Supports multiple textures with texture units*)
(*Then texture uniforms are labelled texture0,texture1,...,texture15*)
class ['a, 'b, 'c] textured_drawable ~(vertex_array : ('a, 'b, 'c) vertex_array) ~(shader_program : shader_program) ~(textures : texture list) ~(number_of_drawn_vertices : int) ~(scene_coordinates : float vector3) ~(local_rotation : quaternion) ~(scale : float vector3) =
  object (self)
    inherit ['a, 'b, 'c] drawable ~vertex_array ~shader_program ~number_of_drawn_vertices ~scene_coordinates ~local_rotation ~scale as super
    val mutable _textures = textures
    method get_textures =
      _textures
    method set_textures texs =
      _textures <- texs
    method! render ~(window : window) ~(uniforms : uniform list) =
      let rec bind_textures l i = match l with
        | [] -> ()
        | a::q -> 
          (*OpenGL does not provide more than 16 texture units*)
          if i > 15 then logger Warning "textured_drawable: exceeded the maximum of 16 texture units. Some textures will not display correctly."
          else (
            Gl.active_texture (Gl.texture0 + i);
            a#bind ();
            _shader_program#set_uniform1i ~name:("texture"^(string_of_int i)) ~value:{x=1};
            bind_textures q (i+1) 
          ) in
      bind_textures textures 0;
      super#render ~window ~uniforms
  end
