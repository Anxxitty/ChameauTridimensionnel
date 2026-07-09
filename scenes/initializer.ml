let initialize_scenes () =
  Main_scene.scene#init ();
  Test_scene.scene#init ()

let delete_scenes () =
  Main_scene.scene#delete ();
  Test_scene.scene#delete ()