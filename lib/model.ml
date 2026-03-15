open Logger
open Vector
open Bigarray

let parseVector4f string bigarray index =
  let splitString = String.split_on_char ' ' string in
  let rec fillBigarray l i = match l with
    | [] -> ()
    | a::q -> bigarray.{i} <- float_of_string (String.trim a); fillBigarray q (i+1)
  in match splitString with
    | [] -> ()
    | a::b::q when b = "" -> (  (*In some .obj files v is followed by two whitespaces which leads to an empty string in the split string*)
        if List.length splitString = 4 then bigarray.{(index*4)+3} <- 1.0; (*If the vertex's w coordinate is ommited, default to 1.0*)
        fillBigarray q (index*4)) 
    | a::q -> (   (*This match is used to ignore the leading "v " in the list*)
        if List.length splitString = 4 then bigarray.{(index*4)+3} <- 1.0; (*If the vertex's w coordinate is ommited, default to 1.0*)
        fillBigarray q (index*4))

let rec parseFaceTriangles stringList faces = match stringList with
    | [] -> faces
    | a::q when a = "f" -> parseFaceTriangles q faces
    | a::q -> (
      let faceInfo = String.split_on_char '/' a in
      match faceInfo with | [] -> [] | v::l -> parseFaceTriangles q (int_of_string v::faces)
    )

let parseFaceQuads stringList faces =
  let rec triangle l i tri indexToIgnore = match l with
    | [] -> tri
    | a::q -> if i <> indexToIgnore then triangle q (i+1) (a::tri) indexToIgnore else triangle q (i+1) tri indexToIgnore in
  let firstTri = triangle stringList 0 [] 3 in
  let secondTri = triangle stringList 0 [] 1 in
  let faces2 = parseFaceTriangles firstTri faces in
  parseFaceTriangles secondTri faces2

let parseFace stringList faces =
  let len = List.length stringList in
  if len = 4 then parseFaceTriangles stringList faces
  else if len = 5 then parseFaceQuads stringList faces
  else faces

let loadObjModel modelPath =
  let modelLinesUnparsed =
    try
      let file = open_in modelPath in
      let rec readFile newLine acc = match newLine with
        | Some line -> (
          try
            let nextLine = input_line file in
            readFile (Some nextLine) (line::acc)
          with
            | End_of_file -> readFile None (line::acc)
            | e -> raise e)
        | None -> acc
      in readFile (Some "") []
    with e ->
      logger Error ("Failed to read input model file for model of path \""^modelPath^"\".");
      logger Error ("Exception: "^(Printexc.to_string e));
      [] in
  let rec determineNumberOfVertices lineList numberOfVertices = match lineList with
    | [] -> numberOfVertices
    | a::q -> if (String.starts_with ~prefix:"v " a) then determineNumberOfVertices q (numberOfVertices+1) else determineNumberOfVertices q numberOfVertices in
  let numberOfVertices = determineNumberOfVertices modelLinesUnparsed 0 in
  let mesh = createBigarray Float32 numberOfVertices 4 in
  let rec parse lineList meshIndex faces = match lineList with
    | [] -> numberOfVertices, mesh, List.rev faces             (*RETURN IS HERE*)
    | a::q -> (
      if String.starts_with ~prefix:"v " a then (parseVector4f (String.trim a) mesh meshIndex; parse q (meshIndex+1) faces) (*vertex coordinates line*)
      else if String.starts_with ~prefix:"f " a then parse q meshIndex (parseFace (String.split_on_char ' ' (String.trim a)) faces) (*face definition line*)
      else parse q meshIndex faces (*unknown line -> ignored*)
    )
  in parse modelLinesUnparsed 0 []
