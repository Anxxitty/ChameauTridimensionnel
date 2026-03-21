open Logger
open Vector
open Bigarray


(*Helper parse functions*)
let rec parseFloatList data bigarray index = match data with
  | [] -> ()
  | a::q -> bigarray.{index} <- float_of_string a; parseFloatList q bigarray (index+1)


(*Returns a triplet of indices containing the processed data at the top of the indices*)
(*Since it adds recursively at the beginning of the input indices, it effectively reverses the input unprocessed data*)
(*"1// 2// 3// \n" and "4// 5// 6// \n" processed one after the other will result in [5;4;3;2;1;0],[],[]*)
let parseFaceTriangles data (vertIndices,texIndices,normIndices) = 
  let rec aux data (vertIndices,texIndices,normIndices) = match data with
    | [] -> vertIndices,texIndices,normIndices
    | a::q ->
      let splitData = String.split_on_char '/' a in
      let vIndex = List.hd splitData in
      let vtIndex = try List.hd (List.tl splitData) with e -> "" in (*try with is necessary to catch errors that occur if the face input contains less than two slashes (can happen apparently...) -> then the missing input defaults to "" just like if it was voluntarily ommitted with the v//vn input for example*)
      let vnIndex = try List.hd (List.tl (List.tl splitData)) with e -> "" in
      let v = (int_of_string vIndex - 1)::vertIndices in                                         (*The -1 are because .obj starts indexing at 1, not 0...*)
      let vt = if vtIndex = "" then texIndices else (int_of_string vtIndex - 1)::texIndices in
      let vn = if vnIndex = "" then normIndices else (int_of_string vnIndex - 1)::normIndices in
      aux q (v,vt,vn)
  in aux data (vertIndices,texIndices,normIndices)
(*Note to self: not sure normals and texture coordinates are supposed to be processed like this at all*)
(*will adapt when I'll implement textures and need normals*)


(*Cuts the input quad in two triangles and processes them*)
let parseFaceQuads data (vertIndices,texIndices,normIndices) =
  let rec triangleFromQuad quad excludedIndex i = match quad with
    | [] -> []
    | a::q -> if i = excludedIndex then triangleFromQuad q excludedIndex (i+1) else a::(triangleFromQuad q excludedIndex (i+1)) in
  (*In a quad formed by points 0,1,2,3 there are two triangles: 0,1,2 and 0,2,3*)
  let firstTriangle = triangleFromQuad data 3 0 in
  let secondTriangle = triangleFromQuad data 1 0 in
  let v,vt,vn = parseFaceTriangles firstTriangle (vertIndices,texIndices,normIndices) in
  parseFaceTriangles secondTriangle (v,vt,vn)


(*Takes a list of strings containing lines of a .obj file and removes leading/trailing/excess whitespaces*)
(*Discards lines with useless information*)
(*It normalizes the input for the parser*)
let rec formatModelSource modelSource = match modelSource with
  | [] -> []
  | a::q ->
    if (String.starts_with ~prefix:"v" a || String.starts_with ~prefix:"vt" a || String.starts_with ~prefix:"vn" a || String.starts_with ~prefix:"f" a)
    then (
      let splitLine = String.split_on_char ' ' (String.trim a) in (*removing leading and trailing whitespaces, splitting on whitespaces*)
      (*removes empty strings created by String.split_on_char when there are successive whitespaces*)
      let rec removeEmptyStrings l = match l with
        | [] -> []
        | a::q -> if a = "" then removeEmptyStrings q else a::removeEmptyStrings q 
      in removeEmptyStrings splitLine::formatModelSource q)
    else formatModelSource q (*discard line if it starts by an unsupported prefix*)


(*Parses a well-formatted source written in .obj syntax*)
(*Does not support vp parameters*)
let parse source =
  (*creating output bigarrays*)
  let rec determineBigArraySizes l numberOfVertices numberOfNormals numberOfTexCoords texCoordFormat = match l with
    | [] -> numberOfVertices, numberOfNormals, numberOfTexCoords, texCoordFormat
    | a::q -> if (List.hd a) = "v" then determineBigArraySizes q (numberOfVertices+1) numberOfNormals numberOfTexCoords texCoordFormat
              else if (List.hd a) = "vn" then determineBigArraySizes q numberOfVertices (numberOfNormals+1) numberOfTexCoords texCoordFormat
              (*texCoordFormat := nb of coordinates in a tex coord (u; u,v or u,v,w)*)
              (*If a vt element is longer than texCoordFormat + 1 (+1 for the "vt") it means we've reached a vt with strictly more coordinates than the previous one -> we need to increase the texCoordFormat*)
              else if (List.hd a) = "vt" then determineBigArraySizes q numberOfVertices numberOfNormals (numberOfTexCoords+1) (if List.length a > (texCoordFormat + 1) then texCoordFormat + 1 else texCoordFormat)
              else determineBigArraySizes q numberOfVertices numberOfNormals numberOfTexCoords texCoordFormat in
  let numberOfVertices, numberOfNormals, numberOfTexCoords, texCoordFormat = determineBigArraySizes source 0 0 0 1 in
  let v = createBigarray Float32 numberOfVertices 4 in
  let vn = createBigarray Float32 numberOfNormals 3 in
  let vt = createBigarray Float32 numberOfTexCoords texCoordFormat in

  (*parsing*)
  let rec fillBigarrays source vIndex vtIndex vnIndex (vertIndices,texIndices,normIndices) = match source with (*vIndex vnIndex and vtIndex represent where we are writing in the bigarray*)
    | [] -> (List.rev vertIndices),(List.rev texIndices),(List.rev normIndices) (*we reverse the lists because new faces were added in front, and in reverse because of how parseFaceTriangles works*)
    | a::q -> (
      let prefix = List.hd a in
      let data = List.tl a in
      let dataLength = List.length data in

      if prefix = "v" then (
        parseFloatList data v vIndex;
        if dataLength = 3 then v.{vIndex + 3} <- 1.0; (*if the w coordinate was ommitted it gets defaulted to 1.0*)
        fillBigarrays q (vIndex+4) vtIndex vnIndex (vertIndices,texIndices,normIndices))
      else if prefix = "vt" then (
        parseFloatList data vt vtIndex;
        if dataLength < texCoordFormat then (*if the v or w coordinates were ommitted they get defaulted to 0.0*)
          for i = 0 to (texCoordFormat - dataLength) do
            vt.{vtIndex + dataLength + i} <- 0.0
          done;
        fillBigarrays q vIndex (vtIndex+texCoordFormat) vnIndex (vertIndices,texIndices,normIndices))
      else if prefix = "vn" then (
        parseFloatList data vn vnIndex;
        fillBigarrays q vIndex vtIndex (vnIndex+3) (vertIndices,texIndices,normIndices))
      else if prefix = "f" then (
        let vertIndices,texIndices,normIndices =
          if dataLength = 4 then parseFaceQuads data (vertIndices,texIndices,normIndices)
          else if dataLength = 3 then parseFaceTriangles data (vertIndices,texIndices,normIndices)
          else (vertIndices,texIndices,normIndices) in
        fillBigarrays q vIndex vtIndex vnIndex (vertIndices,texIndices,normIndices))
      else fillBigarrays q vIndex vtIndex vnIndex (vertIndices,texIndices,normIndices)) in (*if a line not matching the supported prefixes slipped in input data, it just gets ignored*)
  
  let vertIndices,texIndices,normIndices = fillBigarrays source 0 0 0 ([],[],[]) in
  (*returning*)
  numberOfVertices, v, vt, vn, vertIndices, texIndices, normIndices


class model ~path =
  (*Reading the .obj file at 'path'*)
  (*Returns a list of strings containing each line of the .obj file*)
  let modelSource =
    try
      let file = open_in path in
      let rec readFile newLine acc = match newLine with
        | Some line -> (
          try
            let nextLine = input_line file in
            readFile (Some nextLine) (line::acc)
          with
            | End_of_file -> readFile None (line::acc)
            | e -> raise e)
        | None -> acc
      in List.rev (readFile (Some "") [])
    with e ->
      logger Warning ("Failed to read model file at path \""^path^"\".");
      logger Warning ("Exception: "^(Printexc.to_string e));
      [] in
  
  let nbVertices, v, vt, vn, vertIndices, texIndices, normIndices =
    try 
      parse (formatModelSource modelSource)
    with e ->
      logger Warning ("Failed to parse model at path \""^path^"\". The file may contain unsupported .obj formatting. Try exporting it with Blender for consistent syntax.");
      let emptyBigArray = createBigarray Float32 0 0 in
      0, emptyBigArray, emptyBigArray, emptyBigArray, [], [], [] in

  object (self)
    val _numberOfVertices = nbVertices
    val _numberOfDrawnVertices = List.length vertIndices
    val _vertexCoordinates = v
    val _textureCoordinates = vt
    val _normalCoordinates = vn
    val _vertexIndices = vertIndices
    val _textureIndices = texIndices
    val _normalIndices = normIndices
    method getVertexCoordinates = _vertexCoordinates
    method getTextureCoordinates = _textureCoordinates
    method getNormalCoordinates = _normalCoordinates
    method getVertexIndices = _vertexIndices
    method getTextureIndices = _textureIndices
    method getNormalIndices = _normalIndices
    method getNumberOfVertices = _numberOfVertices
    method getNumberOfDrawnVertices = _numberOfDrawnVertices
  end