open Vector
open Logger
open Bigarray
open Tgl3

type dimension = Dim1 | Dim2 | Dim3 | Dim4 

(*Wraps a Bigarray in a class that suits the project's vector structures, with handful methods for later defining vertex attributes*)
class ['a, 'b] vertexAttribute ~(attributeType : ('a, 'b) vectorKind) ~numberOfVertices ?(vectorList : 'a vector list option) () =
  (*Generates the bigarray containing the vertex attribute data*)
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
    val data = bigarray
    val kind = kind
    val dimension = dim
    val size = size
    val numberOfVertices = numberOfVertices
    method getKind =
      kind
    method getDimension = match dimension with
      | Dim1 -> 1
      | Dim2 -> 2
      | Dim3 -> 3
      | Dim4 -> 4
    method getNumberOfVertices =
      numberOfVertices
    method getRawData =
      data
    method getSize =
      size
    method getVertexAttribute ~index = match dimension with
      | Dim1 -> Vector1 { x = data.{index} }
      | Dim2 -> Vector2 { x = data.{2*index}; y = data.{2*index+1} }
      | Dim3 -> Vector3 { x = data.{3*index}; y = data.{3*index+1}; z = data.{3*index+2} }
      | Dim4 -> Vector4 { r = data.{4*index}; g = data.{4*index+1}; b = data.{4*index+2}; a = data.{4*index+3} }
    method setVertexAttribute ~index ~value = match dimension with
      | Dim1 -> (match value with 
        | Vector1 vec -> data.{index} <- vec.x
        | _ -> logger Warning "Trying to change a 1D vertex attribute with a non-1D vector.")
      | Dim2 -> (match value with 
        | Vector2 vec -> (data.{2*index} <- vec.x; data.{2*index+1} <- vec.y)
        | _ -> logger Warning "Trying to change a 2D vertex attribute with a non-2D vector.")
      | Dim3 -> (match value with 
        | Vector3 vec -> (data.{3*index} <- vec.x; data.{3*index+1} <- vec.y; data.{3*index+2} <- vec.z)
        | _ -> logger Warning "Trying to change a 3D vertex attribute with a non-3D vector.")
      | Dim4 -> (match value with 
        | Vector4 vec -> (data.{4*index} <- vec.r; data.{4*index+1} <- vec.g; data.{4*index+2} <- vec.b; data.{4*index+3} <- vec.a)
        | _ -> logger Warning "Trying to change a 4D vertex attribute with a non-4D vector.")
  end

class ['a, 'b] buffer ~bufferType =
  object (self)
    val id = getFirstInt (Gl.gen_buffers 1)
    val bufferType = bufferType
    method getID =
      id
    method getBufferType =
      bufferType
    method bind () =
      Gl.bind_buffer bufferType id
    method writeRawData ~(size : int) ~(data : ('a, 'b) Gl.bigarray) ~(usage : int) =
      self#bind ();
      Gl.buffer_data id size (Some data) usage;
    method writeVertexAttributes ~(vertexAttributes : (('a, 'b) vertexAttribute) list) ~(usage : int) =
      self#bind ();
      match vertexAttributes with   
        | [] -> () 
        | a::q -> (
            let numberOfVertices = a#getNumberOfVertices in
            let kind = a#getKind in
            let rec checkDataCoherenceAndDetermineSize sizeAcc l = match l with
              | [] -> sizeAcc
              | a::q ->
                if a#getNumberOfVertices <> numberOfVertices || a#getKind <> kind 
                  then (logger Warning "Failed to write vertex attributes to buffer: attributes do not contain the same number of vertices."; 0)
                  else checkDataCoherenceAndDetermineSize (sizeAcc + (a#getNumberOfVertices * a#getSize)) q
            in let size = checkDataCoherenceAndDetermineSize 0 vertexAttributes in
            if size = 0 then () else
            let rec readVertexAttributes dataAcc vertexIndex dataIndex topPile bottomPile = match topPile with
              | [] -> 
                if vertexIndex = (numberOfVertices - 1)
                  then dataAcc 
                  else readVertexAttributes dataAcc (vertexIndex+1) dataIndex (List.rev bottomPile) topPile
              | a::q -> (
                let newData = a#getVertexAttribute ~index:vertexIndex in match newData with
                  | Vector1 vec -> dataAcc.{dataIndex} <- vec.x; readVertexAttributes dataAcc vertexIndex (dataIndex+1) q (a::bottomPile)
                  | Vector2 vec -> dataAcc.{dataIndex} <- vec.x; dataAcc.{dataIndex+1} <- vec.y; readVertexAttributes dataAcc vertexIndex (dataIndex+2) q (a::bottomPile)
                  | Vector3 vec -> dataAcc.{dataIndex} <- vec.x; dataAcc.{dataIndex+1} <- vec.y; dataAcc.{dataIndex+2} <- vec.z; readVertexAttributes dataAcc vertexIndex (dataIndex+3) q (a::bottomPile)
                  | Vector4 vec -> dataAcc.{dataIndex} <- vec.r; dataAcc.{dataIndex+1} <- vec.g; dataAcc.{dataIndex+2} <- vec.b; dataAcc.{dataIndex+3} <- vec.a; readVertexAttributes dataAcc vertexIndex (dataIndex+4) q (a::bottomPile))
            in let data = readVertexAttributes (createBigarray kind size 1) 0 0 vertexAttributes [] in
            (*logger Info ("0: "^(string_of_float data.{0})^" "^"1: "^(string_of_float data.{1})^" "^"2: "^(string_of_float data.{2})^" "^"3: "^(string_of_float data.{3})^" "^"4: "^(string_of_float data.{4})^" "^"5: "^(string_of_float data.{5})^" "^"6: "^(string_of_float data.{6})^" "^"7: "^(string_of_float data.{7})^" "^"8: "^(string_of_float data.{8})^" "^"9: "^(string_of_float data.{9})^" "^"10: "^(string_of_float data.{10})^" "^"11: "^(string_of_float data.{11})^" "^"12: "^(string_of_float data.{12})^" "^"13: "^(string_of_float data.{13})^" "^"14: "^(string_of_float data.{14})^" "^"15: "^(string_of_float data.{15})^" "^"16: "^(string_of_float data.{16})^" "^"17: "^(string_of_float data.{17})^" "^"18: "^(string_of_float data.{18})^" "^"19: "^(string_of_float data.{19})^" "^"20: "^(string_of_float data.{20})^" ");*)
            Gl.buffer_data Gl.array_buffer size (Some data) usage;)
    (*Don't forget to delete !*)
    method deleteBuffer =
      Gl.delete_buffers 1 (setFirstInt id)
  end



