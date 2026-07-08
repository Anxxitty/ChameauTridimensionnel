open Graphics
open Shader
open Window
open Actor
open Data_structures

module type Scene = sig
  val movables : movable list
  val opaque_drawables : drawable list
  val translucent_drawables : drawable list
  val to_delete : < delete : unit -> unit > list
  val active_camera : camera
  val tick : (window : window -> elapsed_time : float -> unit)
end

type scene = (module Scene)

let make_scene ~(scene_init : unit -> scene) = new default ~init:scene_init ~delete:(fun (module S) -> List.iter (fun x -> x#delete ()) S.to_delete)

class scene_handler ~(initial_scene : scene) =
  object (self)
    val mutable _active_scene = initial_scene
    method switch_scene scene =
      _active_scene <- scene
    method render ~(window : window) ~(uniforms : uniform list) ~aspect_ratio =

      let camera = let (module S) = _active_scene in S.active_camera in

      let view_matrix = camera#gen_view_matrix in
      let proj_matrix = camera#gen_projection_matrix ~aspect_ratio in
      let uniforms = [Matrix_uniform4f ("view_matrix", view_matrix);Matrix_uniform4f ("projection_matrix", proj_matrix)] in

      let rec aux d = match d with
        | [] -> ()
        | a::q -> (a#render ~window ~uniforms; aux q)
      in aux (let (module S) = _active_scene in S.opaque_drawables)
  end
