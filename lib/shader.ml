open Tgl3
open Logger
open Vector
open Bigarray

type uniform = VectorUniform1f of string * (float vector1)
             | VectorUniform1i of string * (int vector1)
             | VectorUniform2f of string * (float vector2)
             | VectorUniform2i of string * (int vector2)
             | VectorUniform3f of string * (float vector3)
             | VectorUniform3i of string * (int vector3)
             | VectorUniform4f of string * (float vector4)
             | VectorUniform4i of string * (int vector4)
             | MatrixUniform4f of string * (float matrix4)


class shader ~(shaderType : int) ~(shaderSourcePath : string) =
  (*constructor*)
  let shaderID = Gl.create_shader shaderType in
  (*retrieve shader source*)
  let shaderSource =
    try
      let file = open_in shaderSourcePath in
      (*Recursively retrieves each line of file, adds them to the accumulator and when the last line is reached,*)
      (*an End_of_file error is raised: we catch it and it triggers the end of the recursion*)
      (*if another error is raised, we carry it down to the main exception catcher for exceptions while reading file*)
      let rec readFile newLine acc = match newLine with
        | Some line -> (
            try
              let nextLine = input_line file in
              readFile (Some nextLine) (acc^"\n"^line)
            with
              | End_of_file -> readFile None (acc^"\n"^line)
              | e -> raise e)
        | None -> acc
      in readFile (Some "") ""
    (*If an error is raised when trying to read the source file, we print it out in the log*)
    (*The source code for the shader is then initialized to an empty string*)
    (*This will cause compilation errors down the line*)
    with e ->
      logger Error ("Failed to read input shader file for"^(if shaderType = Gl.vertex_shader then " vertex shader " else " fragment shader ")^"of path \""^shaderSourcePath^"\".");
      logger Error ("Exception: "^(Printexc.to_string e));
      logger Error "Please ignore the following shader compilation errors.";
      "" 
  in 
  let () = 
    (*compile shader*)
    Gl.shader_source shaderID shaderSource;
    Gl.compile_shader shaderID;
    (*check compilation success*)
    if getFirstInt (Gl.get_shaderiv shaderID Gl.compile_status) <> Gl.true_ then
      let len = getFirstInt (Gl.get_shaderiv shaderID Gl.info_log_length) in
      let infoLog = createBigarray Char 1 len in
      Gl.get_shader_info_log shaderID len None infoLog;
      logger Error "Failed to compile shader. Compiler log below:";
      logger Error (Gl.string_of_bigarray infoLog)
  in
  (*class definition*)
  object (self) 
    val id = shaderID
    val success = (getFirstInt (Gl.get_shaderiv shaderID Gl.compile_status) = Gl.true_)
    method getID =
      id
    method getSuccess =
      success
    (*Do not forget to delete shaders manually before they get out of scope ! OCaml provides no deestructor function for us to do that automatically*)
    method delete () =
      Gl.delete_shader id
  end

class shaderProgram ~(vertexShader : shader) ~(fragmentShader : shader) =
  (*constructor*)
  let programID = Gl.create_program () in
  let () =
    (*link shader program*)
    Gl.attach_shader programID vertexShader#getID;
    Gl.attach_shader programID fragmentShader#getID;
    Gl.link_program programID;
    vertexShader#delete ();
    fragmentShader#delete ();
    (*check for linking errors*)
    if getFirstInt (Gl.get_programiv programID Gl.link_status) <> Gl.true_ then
      let len = getFirstInt (Gl.get_programiv programID Gl.info_log_length) in
      let infoLog = createBigarray Char 1 len in
      Gl.get_program_info_log programID len None infoLog;
      logger Error "Failed to link the shader program. Linker log below:";
      logger Error (Gl.string_of_bigarray infoLog)
  in
  object (self)
    val id = programID
    val success = (getFirstInt (Gl.get_programiv programID Gl.link_status) = Gl.true_)
    method getID =
      id
    method getSuccess =
      success
    method use () =
      Gl.use_program id
    (*Helper uniform setters*)
    method setUniform1i ~name ~(value : int vector1) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform1i location value.x
    method setUniform1f ~name ~(value : float vector1) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform1f location value.x
    method setUniform2i ~name ~(value : int vector2) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform2i location value.x value.y
    method setUniform2f ~name ~(value : float vector2) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform2f location value.x value.y
    method setUniform3i ~name ~(value : int vector3) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform3i location value.x value.y value.z
    method setUniform3f ~name ~(value : float vector3) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform3f location value.x value.y value.z
    method setUniform4i ~name ~(value : int vector4) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform4i location value.r value.g value.b value.a
    method setUniform4f ~name ~(value : float vector4) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform4f location value.r value.g value.b value.a
    (*Same for matrix uniforms*)
    method setUniformMatrix4f ~name ~(value : float matrix4) =
      let location = Gl.get_uniform_location id name in Gl.use_program id; Gl.uniform_matrix4fv location 1 false (bigarrayOfMatrix4f value)
    (*generic method*)
    method setUniform (uniform : uniform) = match uniform with
      | VectorUniform1f (name, value) -> self#setUniform1f ~name ~value
      | VectorUniform1i (name, value) -> self#setUniform1i ~name ~value
      | VectorUniform2f (name, value) -> self#setUniform2f ~name ~value
      | VectorUniform2i (name, value) -> self#setUniform2i ~name ~value
      | VectorUniform3f (name, value) -> self#setUniform3f ~name ~value
      | VectorUniform3i (name, value) -> self#setUniform3i ~name ~value
      | VectorUniform4f (name, value) -> self#setUniform4f ~name ~value
      | VectorUniform4i (name, value) -> self#setUniform4i ~name ~value
      | MatrixUniform4f (name, value) -> self#setUniformMatrix4f ~name ~value
    (*Do not forget to call that before program gets out of scope to avoid memory leak!*)
    method delete () =
      Gl.delete_program id
  end