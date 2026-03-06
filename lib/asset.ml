open Vector
open Shader
open Logger
open Bigarray
open Tgl3

let getGlType : type a b. ((a, b) kind -> int) = function
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
class ['a, 'b] vertexAttribute ~(attributeType : ('a, 'b) vectorKind) ~numberOfVertices ?(vectorList : 'a vector list option) () =
  (*Generates the bigarray containing the vertex attribute data, extracts the size and kind parameters*)
  let kind, size, dim, bigarray = match attributeType with
    | Vector1Kind kind -> kind, kind_size_in_bytes kind, Dim1, Array1.init kind C_layout (1 * numberOfVertices) (fun i -> (zero kind))
    | Vector2Kind kind -> kind, 2*(kind_size_in_bytes kind), Dim2, Array1.init kind C_layout (2 * numberOfVertices) (fun i -> (zero kind))
    | Vector3Kind kind -> kind, 3*(kind_size_in_bytes kind), Dim3, Array1.init kind C_layout (3 * numberOfVertices) (fun i -> (zero kind))
    | Vector4Kind kind -> kind, 4*(kind_size_in_bytes kind), Dim4, Array1.init kind C_layout (4 * numberOfVertices) (fun i -> (zero kind)) in
  (*Fills the bigarray with the given input data*)
  let () = (
    match vectorList with
      | Some vectorListVal -> (
          let rec fillBigarray i initList = match initList with
           | [] -> ()
           | a::q -> match a with
              | Vector1 vec -> bigarray.{i} <- vec.x; fillBigarray (i+1) q
              | Vector2 vec -> let j = 2*i in (bigarray.{j} <- vec.x; bigarray.{j+1} <- vec.y); fillBigarray (i+1) q
              | Vector3 vec -> let j = 3*i in (bigarray.{j} <- vec.x; bigarray.{j+1} <- vec.y; bigarray.{j+2} <- vec.z); fillBigarray (i+1) q
              | Vector4 vec -> let j = 4*i in (bigarray.{j} <- vec.r; bigarray.{j+1} <- vec.g; bigarray.{j+2} <- vec.b; bigarray.{j+3} <- vec.a); fillBigarray (i+1) q
          in fillBigarray 0 vectorListVal)
      | None -> ()
  ) in
  object (self)
    (*contains the actual vertex attribute data*)
    val _data = bigarray
    (*contains the type of the data as a bigarray kind*)
    val _kind = kind
    (*defines if the vertex attribute contains 1- 2- 3- or 4-dimensional vectors*)
    val _dimension = dim
    (*the overall size of ONE vertex attribute (ex: for a vertexAttribute of type Float32 Vertex4, size = size(Float32)*4 )*)
    val _size = size
    (*the number of vertices contained in data*)
    val _numberOfVertices = numberOfVertices
    (*simple getter methods*)
    method getKind =
      _kind
    method getDimension = match _dimension with
      | Dim1 -> 1
      | Dim2 -> 2
      | Dim3 -> 3
      | Dim4 -> 4
    method getNumberOfVertices =
      _numberOfVertices
    method getRawData =
      _data
    method getSize =
      _size
    (*returns the vertexAttribute of the i-th vertex*)
    method getVertexAttribute ~index = match _dimension with
      | Dim1 -> Vector1 { x = _data.{index} }
      | Dim2 -> Vector2 { x = _data.{2*index}; y = _data.{2*index+1} }
      | Dim3 -> Vector3 { x = _data.{3*index}; y = _data.{3*index+1}; z = _data.{3*index+2} }
      | Dim4 -> Vector4 { r = _data.{4*index}; g = _data.{4*index+1}; b = _data.{4*index+2}; a = _data.{4*index+3} }
    (*allows for changing the vertexAttribute of the i-th vertex*)
    (*Note: the bigarray size is fixed: it is impossible to add a vertex to a vertexAttribute object*) 
    method setVertexAttribute ~index ~value = match _dimension with
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
        | Vector4 vec -> (_data.{4*index} <- vec.r; _data.{4*index+1} <- vec.g; _data.{4*index+2} <- vec.b; _data.{4*index+3} <- vec.a)
        | _ -> logger Warning "Trying to change a 4D vertex attribute with a non-4D vector.")
  end

(*Manages OpenGL buffer objects (like VBOs and EBOs)*)
class ['a, 'b] buffer ~bufferType ~(kind : ('a, 'b) kind) =
  object (self)
    val _id = getFirstInt (Gl.gen_buffers 1)
    (*Gl.array_buffer ; Gl.element_array_buffer...*)
    val _bufferType = bufferType
    (*kind of what will be stored in the buffer*)
    val _kind = kind
    (*simple getters*)
    method getID =
      _id
    method getBufferType =
      _bufferType
    method getKind =
      _kind
    (*Binds the buffer to the correct target (to the VBO target if bufferType is VBO...)*)
    method bind () =
      Gl.bind_buffer _bufferType _id
    (*Wrapper for the OpenGL function that writes data to a buffer (Gl.buffer_data)*)
    method writeRawData ~(size : int) ~(data : ('a, 'b) Gl.bigarray) ~(usage : int) =
      self#bind ();
      Gl.buffer_data _bufferType size (Some data) usage;
    (*Takes a list of vertex attributes and generates a well-structured data array and copies it in the buffer*)
    (*Note: Only appropriate for VBOs; don't try using this for an EBO, it would make no sense*)
    (*For a list of attributes like this: [positionVertexAttrib; colorVertexAttrib], the data would look like:*)
    (*[Vertex1;      Vertex2      ...]*)
    (*[pos;  color;  pos;  color  ...]*)
    (*[x,y,z,r,g,b,a,x,y,z,r,g,b,a...]*)
    method writeElementBuffer ~(indices : 'a list) ~(usage : int) =
      let len = List.length indices in
      let ba = createBigarray _kind 1 len in
      let rec fill l i = match l with
        | [] -> ba
        | a::q -> (ba.{i} <- a); fill q (i+1)
      in let data = fill indices 0 in
      self#writeRawData ~size:(Gl.bigarray_byte_size data) ~data ~usage
    method writeVertexAttributes ~(vertexAttributes : (('a, 'b) vertexAttribute) list) ~(usage : int) =
      self#bind ();
      match vertexAttributes with
        | [] -> () (*If the provided list is empty, there's nothing to do*)  
        | a::q -> (
            (*we fetch the kind and number of vertices of the first vertexAttribute to ensure the others match*)
            let numberOfVertices = a#getNumberOfVertices in
            let kind = a#getKind in
            (*scans the whole list, asserts that each vertexAttribute is of the same kind as the first one and has the same number of vertices*)
            (*returns the size of the bigarray that will contain all the data (returns 0 if the assert fails)*)
            let rec checkDataCoherenceAndDetermineSize sizeAcc l = match l with
              | [] -> sizeAcc
              | a::q ->
                if a#getNumberOfVertices <> numberOfVertices || a#getKind <> kind 
                  then (logger Warning "Failed to write vertex attributes to buffer: either the attributes do not contain the same number of vertices or they are not all of the same data type."; 0)
                  else checkDataCoherenceAndDetermineSize (sizeAcc + (a#getNumberOfVertices * a#getSize)) q
            in let size = checkDataCoherenceAndDetermineSize 0 vertexAttributes in
            (*if the final bigarray is of size 0, there is nothing to copy*)
            if size = 0 then () else
            (*scans the list over and over until all vertices have been copied in the dataAcc*)
            let rec readVertexAttributes dataAcc vertexIndex dataIndex topPile bottomPile = match topPile with
              | [] -> 
                if vertexIndex = (numberOfVertices - 1) (*when there are 12 vertices, they range from 0 to 11*)
                  then dataAcc 
                  (*once the top pile is empty, it means we have copied all the attributes of the i-th vertex: we refill the top pile and continue with vertex i+1*)
                  else readVertexAttributes dataAcc (vertexIndex+1) dataIndex (List.rev bottomPile) topPile
              | a::q -> (
                (*retrieve the attribute data of the i-th vertex in the vertexAttribute a*)
                (*this will be an n-vector of type kind*)
                (*we match on the dimension of the vector to add the data correctly to the dataAcc*)
                (*this puts in evidence the role of dataIndex: it follows where we are in dataAcc and where we should continue writing*)
                let newData = a#getVertexAttribute ~index:vertexIndex in match newData with
                  | Vector1 vec -> dataAcc.{dataIndex} <- vec.x; readVertexAttributes dataAcc vertexIndex (dataIndex+1) q (a::bottomPile)
                  | Vector2 vec -> dataAcc.{dataIndex} <- vec.x; dataAcc.{dataIndex+1} <- vec.y; readVertexAttributes dataAcc vertexIndex (dataIndex+2) q (a::bottomPile)
                  | Vector3 vec -> dataAcc.{dataIndex} <- vec.x; dataAcc.{dataIndex+1} <- vec.y; dataAcc.{dataIndex+2} <- vec.z; readVertexAttributes dataAcc vertexIndex (dataIndex+3) q (a::bottomPile)
                  | Vector4 vec -> dataAcc.{dataIndex} <- vec.r; dataAcc.{dataIndex+1} <- vec.g; dataAcc.{dataIndex+2} <- vec.b; dataAcc.{dataIndex+3} <- vec.a; readVertexAttributes dataAcc vertexIndex (dataIndex+4) q (a::bottomPile))
            in let data = readVertexAttributes (createBigarray kind size 1) 0 0 vertexAttributes [] in
            (*logger Info ("0: "^(string_of_float data.{0})^" "^"1: "^(string_of_float data.{1})^" "^"2: "^(string_of_float data.{2})^" "^"3: "^(string_of_float data.{3})^" "^"4: "^(string_of_float data.{4})^" "^"5: "^(string_of_float data.{5})^" "^"6: "^(string_of_float data.{6})^" "^"7: "^(string_of_float data.{7})^" "^"8: "^(string_of_float data.{8})^" "^"9: "^(string_of_float data.{9})^" "^"10: "^(string_of_float data.{10})^" "^"11: "^(string_of_float data.{11})^" "^"12: "^(string_of_float data.{12})^" "^"13: "^(string_of_float data.{13})^" "^"14: "^(string_of_float data.{14})^" "^"15: "^(string_of_float data.{15})^" "^"16: "^(string_of_float data.{16})^" "^"17: "^(string_of_float data.{17})^" "^"18: "^(string_of_float data.{18})^" "^"19: "^(string_of_float data.{19})^" "^"20: "^(string_of_float data.{20})^" ");*)
            (*copies the data in the buffer*)
            Gl.buffer_data _bufferType size (Some data) usage;)
    (*Don't forget to delete !*)
    method delete () =
      Gl.delete_buffers 1 (setFirstInt _id)
  end

class ['a, 'b, 'c] vertexArray ~kind ~(vertexAttributes : (('a, 'b) vertexAttribute) list) ~(elementBuffer : (int, 'c) buffer) ~(drawingType : int) =
  (*Generates the object's ID (it is a VAO)*)
  let id = getFirstInt (Gl.gen_vertex_arrays 1) in
  (*Generates the VBO associated to our VAO*)
  let vbo : (('a, 'b) buffer) = new buffer ~bufferType:Gl.array_buffer ~kind in
  (*Retrieves the number of vertices in the array*)
  let numberOfVertices = match vertexAttributes with
    | [] -> 0
    | a::q -> a#getNumberOfVertices in
  let () = (
    (*setting up the VAO*)
    Gl.bind_vertex_array id;
    vbo#bind ();
    elementBuffer#bind ();
    (**copying data in the VBO*)
    vbo#writeVertexAttributes ~vertexAttributes ~usage:drawingType;
    (**setting up the vertex attribute pointers*)
    let rec determineStride strideAcc (l : (('a, 'b) vertexAttribute) list) = match l with
      | [] -> strideAcc
      | a::q -> determineStride (a#getSize + strideAcc) q
    in let stride = determineStride 0 vertexAttributes in
    let rec setAttributePointers index offset l = match l with
      | [] -> ()
      | a::q -> (
        Gl.vertex_attrib_pointer index a#getDimension (getGlType a#getKind) false stride (`Offset offset);
        Gl.enable_vertex_attrib_array index;
        setAttributePointers (index+1) (offset+a#getSize) q
      )
    in setAttributePointers 0 0 vertexAttributes 
  ) in
  object (self)
    val _id = id
    val mutable _numberOfVertices = numberOfVertices
    val mutable _vertexBuffer = vbo
    val mutable _elementBuffer = elementBuffer
    val mutable _vertexAttributes = vertexAttributes
    val mutable _drawingType = drawingType
    (*getters and setters*)
    method getID =
      _id
    method getNumberOfVertices =
      _numberOfVertices
    method getVertexBuffer =
      _vertexBuffer
    method getElementBuffer =
      _elementBuffer
    method getVertexAttributes =
      _vertexAttributes
    method getDrawingType =
      _drawingType
    (*changes the vertex attributes and redraws the object*)
    method setVertexAttributes ~(vertexAttributes : (('a, 'b) vertexAttribute) list) =
      _vertexAttributes <- vertexAttributes;
      _numberOfVertices <- (match vertexAttributes with
        | [] -> 0 
        | a::q -> a#getNumberOfVertices);
      (*replacing the VBO*)
      _vertexBuffer#delete ();
      _vertexBuffer <- new buffer ~bufferType:Gl.array_buffer ~kind;
      _vertexBuffer#writeVertexAttributes ~vertexAttributes ~usage:drawingType;
      (*setting up the new vertex attribute pointers*)
      let rec determineStride strideAcc (l : (('a, 'b) vertexAttribute) list) = match l with
        | [] -> strideAcc
        | a::q -> determineStride (a#getSize + strideAcc) q
      in let stride = determineStride 0 vertexAttributes in
      let rec setAttributePointers index offset l = match l with
        | [] -> ()
        | a::q -> (
          Gl.vertex_attrib_pointer index a#getDimension (getGlType a#getKind) false stride (`Offset offset);
          Gl.enable_vertex_attrib_array index;
          setAttributePointers (index+1) (offset+a#getSize) q
        )
      in setAttributePointers 0 0 vertexAttributes
    method setDrawingType ~drawingType =
      _drawingType <- drawingType
    method bind () =
      Gl.bind_vertex_array _id
    (*If vertexAttribute contents are changed or if drawing type is changed, this redraws the VBO accordingly*)
    method redraw () =
      _vertexBuffer#writeVertexAttributes ~vertexAttributes ~usage:drawingType;
    method delete () =
      Gl.delete_vertex_arrays 1 (setFirstInt _id);
      _vertexBuffer#delete ();
      _elementBuffer#delete ()
  end

class ['a, 'b, 'c] drawable ~(vertexArray : ('a, 'b, 'c) vertexArray) ~(shaderProgram : shaderProgram) ~(numberOfDrawnVertices : int) ~(sceneCoordinates : float vector3) ~(localRotation : float vector3) ~(scale : float vector3) =
  object (self)
    val mutable _vertexArray = vertexArray
    val mutable _shaderProgram = shaderProgram
    val mutable _coordinates = sceneCoordinates
    val mutable _rotation = localRotation
    val mutable _scale = scale
    val _numberOfDrawnVertices = numberOfDrawnVertices
    (*getters and setters*)
    method getVertexArray =
      _vertexArray
    method getShaderProgram =
      _shaderProgram
    method getCoordinates =
      _coordinates
    method getRotation =
      _rotation
    method getScale =
      _scale
    method setVertexArray (vertexArray : ('a, 'b, 'c) vertexArray) =
      _vertexArray <- vertexArray
    method setShaderProgram (shaderProgram : shaderProgram) =
      _shaderProgram <- shaderProgram
    method setCoordinates (coordinates : float vector3) =
      _coordinates <- coordinates
    method setRotation (rotation : float vector3) =
      _rotation <- rotation
    method setScale (scale : float vector3) =
      _scale <- scale
    (*other methods*)
    (*translates the object by the amount given*)
    (*sets position to 0,0,0 if result of addition is not a Vector3 (this cannot actually happen)*)
    method translate (translation : float vector3) =
      _coordinates <- match (Vector3 _coordinates) +:. (Vector3 translation) with | Vector3 vec -> vec | _ -> {x=0.0;y=0.0;z=0.0}
    method rotate ~(rotationAxis : float vector3) ~(angle : float) =
      logger Warning "drawable#rotate is not implemented yet."
    method scale (scalingVector : float vector3) =
      _scale <- { x = _scale.x *. scalingVector.x; y = _scale.y *. scalingVector.y; z = _scale.z *. scalingVector.z }
    method render ~(window : GLFW.window) ~(uniforms : uniform list) =
      _vertexArray#bind ();
      let rec uniformSetter l = match l with
        | [] -> ()
        | a::q -> (
          _shaderProgram#setUniform a;
          uniformSetter q)
      in uniformSetter uniforms;
      _shaderProgram#use ();
      Gl.draw_elements Gl.triangles _numberOfDrawnVertices (getGlType _vertexArray#getElementBuffer#getKind) (`Offset 0);
      
  end
