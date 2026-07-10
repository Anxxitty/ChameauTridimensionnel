open Graphics
open Shader
open Window
open Actor
open Data_structures
open Math
open Save

type deletable = < delete : unit -> unit >

module type Scene = sig
  val movables : movable list
  val opaque_drawables : drawable list
  val translucent_drawables : drawable list
  val to_delete : deletable list
  val to_save : (module Persistent) list
  val save : save
  val active_camera : camera
  val tick : (window : window -> elapsed_time : float -> unit)
end

type scene = (module Scene)

let make_scene ~(scene_init : unit -> scene) = new default
  ~init:(fun () -> (
    let (module S) = scene_init () in
    List.iter (fun p -> S.save#register_persistent p) S.to_save;
    S.save#load ();
    (module S : Scene)
  )) 
  ~delete:(fun (module S) -> List.iter (fun x -> x#delete ()) S.to_delete; S.save#save ())

class scene_handler ~(initial_scene : scene) =
  object (self)
    val mutable _active_scene = initial_scene
    method switch_scene scene =
      _active_scene <- scene
    method render ~(window : window) ~(uniforms : uniform list) ~aspect_ratio =
      let (module S) = _active_scene in
      let camera = S.active_camera in

      let view_matrix = camera#gen_view_matrix in
      let proj_matrix = camera#gen_projection_matrix ~aspect_ratio in
      let uniforms = [Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)] in

      let rec aux d = match d with
        | [] -> ()
        | a::q -> (a#render ~window ~uniforms; aux q)
      in aux (let (module S) = _active_scene in S.opaque_drawables)
    method tick ~window ~elapsed_time =
      let (module S) = _active_scene in S.tick ~window ~elapsed_time
  end
