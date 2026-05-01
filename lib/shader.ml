open Tgl3
open Logger
open Math
open Bigarray
open Data_structures

type uniform = Vector_uniform1f of string * (float vector1)
             | Vector_uniform1i of string * (int vector1)
             | Vector_uniform2f of string * (float vector2)
             | Vector_uniform2i of string * (int vector2)
             | Vector_uniform3f of string * (float vector3)
             | Vector_uniform3i of string * (int vector3)
             | Vector_uniform4f of string * (float vector4)
             | Vector_uniform4i of string * (int vector4)
             | Matrix_uniform4f of string * (float matrix4)


class shader ~(shader_type : int) ~(shader_source_path : string) =
  (*constructor*)
  let shader_id = Gl.create_shader shader_type in
  (*retrieve shader source*)
  let shader_source =
    try
      let file = open_in shader_source_path in
      (*Recursively retrieves each line of file, adds them to the accumulator and when the last line is reached,*)
      (*an End_of_file error is raised: we catch it and it triggers the end of the recursion*)
      (*if another error is raised, we carry it down to the main exception catcher for exceptions while reading file*)
      let rec read_file new_line acc = match new_line with
        | Some line -> (
            try
              let next_line = input_line file in
              read_file (Some next_line) (acc^"\n"^line)
            with
              | End_of_file -> read_file None (acc^"\n"^line)
              | e -> raise e)
        | None -> acc
      in read_file (Some "") ""
    (*If an error is raised when trying to read the source file, we print it out in the log*)
    (*The source code for the shader is then initialized to an empty string*)
    (*This will cause compilation errors down the line*)
    with e ->
      logger Error ("shader: Failed to read input shader file for"^(if shader_type = Gl.vertex_shader then " vertex shader " else " fragment shader ")^"of path \""^shader_source_path^"\".");
      logger Error ("shader: Exception: "^(Printexc.to_string e));
      logger Error "shader: Please ignore the following shader compilation errors.";
      "" 
  in 
  let () = 
    (*compile shader*)
    Gl.shader_source shader_id shader_source;
    Gl.compile_shader shader_id;
    (*check compilation success*)
    if get_first_int (Gl.get_shaderiv shader_id Gl.compile_status) <> Gl.true_ then
      let len = get_first_int (Gl.get_shaderiv shader_id Gl.info_log_length) in
      let infoLog = create_bigarray Char 1 len in
      Gl.get_shader_info_log shader_id len None infoLog;
      logger Error ("shader: Failed to compile shader at path \""^shader_source_path^"\". Compiler log below:");
      logger Error (Gl.string_of_bigarray infoLog)
  in
  (*class definition*)
  object (self) 
    val id = shader_id
    val success = (get_first_int (Gl.get_shaderiv shader_id Gl.compile_status) = Gl.true_)
    method get_id =
      id
    method get_success =
      success
    (*Do not forget to delete shaders manually before they get out of scope ! OCaml provides no deestructor function for us to do that automatically*)
    method delete () =
      Gl.delete_shader id
  end

class shader_program ~(vertex_shader : shader) ~(fragment_shader : shader) =
  (*constructor*)
  let program_id = Gl.create_program () in
  let () =
    (*link shader program*)
    Gl.attach_shader program_id vertex_shader#get_id;
    Gl.attach_shader program_id fragment_shader#get_id;
    Gl.link_program program_id;
    vertex_shader#delete ();
    fragment_shader#delete ();
    (*check for linking errors*)
    if get_first_int (Gl.get_programiv program_id Gl.link_status) <> Gl.true_ then
      let len = get_first_int (Gl.get_programiv program_id Gl.info_log_length) in
      let info_log = create_bigarray Char 1 len in
      Gl.get_program_info_log program_id len None info_log;
      logger Error "shader_program: Failed to link the shader program. Linker log below:";
      logger Error (Gl.string_of_bigarray info_log)
  in
  object (self)
    val id = program_id
    val success = (get_first_int (Gl.get_programiv program_id Gl.link_status) = Gl.true_)
    method get_id =
      id
    method get_success =
      success
    method use () =
      Gl.use_program id
    (*Helper uniform setters*)
    method set_uniform1i ~name ~(value : int vector1) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform1i location value.x
    method set_uniform1f ~name ~(value : float vector1) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform1f location value.x
    method set_uniform2i ~name ~(value : int vector2) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform2i location value.x value.y
    method set_uniform2f ~name ~(value : float vector2) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform2f location value.x value.y
    method set_uniform3i ~name ~(value : int vector3) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform3i location value.x value.y value.z
    method set_uniform3f ~name ~(value : float vector3) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform3f location value.x value.y value.z
    method set_uniform4i ~name ~(value : int vector4) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform4i location value.x value.y value.z value.w
    method set_uniform4f ~name ~(value : float vector4) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform4f location value.x value.y value.z value.w
    (*Same for matrix uniforms*)
    method set_uniform_matrix4f ~name ~(value : float matrix4) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform_matrix4fv location 1 true (bigarray_of_matrix4f value)
    (*generic method*)
    method set_uniform (uniform : uniform) = match uniform with
      | Vector_uniform1f (name, value) -> self#set_uniform1f ~name ~value
      | Vector_uniform1i (name, value) -> self#set_uniform1i ~name ~value
      | Vector_uniform2f (name, value) -> self#set_uniform2f ~name ~value
      | Vector_uniform2i (name, value) -> self#set_uniform2i ~name ~value
      | Vector_uniform3f (name, value) -> self#set_uniform3f ~name ~value
      | Vector_uniform3i (name, value) -> self#set_uniform3i ~name ~value
      | Vector_uniform4f (name, value) -> self#set_uniform4f ~name ~value
      | Vector_uniform4i (name, value) -> self#set_uniform4i ~name ~value
      | Matrix_uniform4f (name, value) -> self#set_uniform_matrix4f ~name ~value
    (*Do not forget to call that before program gets out of scope to avoid memory leak!*)
    method delete () =
      Gl.delete_program id
  end
