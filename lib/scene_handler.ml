open Graphics
open Shader
open Window
open Actor
open Data_structures
open Math
open Save
open Window
open Tgl3

type deletable = < delete : unit -> unit >

module type Scene = sig
  val movables : movable list
  val opaque_drawables : drawable list
  val translucent_drawables : drawable list
  val lights : (string * light list) list
  val keybindings : input_handler list
  val to_delete : deletable list
  val to_save : (module Persistent) list
  val save : save
  val active_camera : camera
  val captures_cursor : bool
  val mouse_callback : float -> float -> unit
  val scroll_callback : float -> float -> unit
  val on_load : (window : window -> unit)
  val on_unload : (window : window -> unit)
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

class scene_handler ~(initial_scene : scene) ~(window : window) =
  object (self)
    initializer 
      let (module S) = initial_scene in List.iter window#register_scene_specific_input_handler S.keybindings;
      let mouse_callback = if S.captures_cursor 
          then (fun x y -> (
            if window#is_cursor_shown = false then S.mouse_callback x y
          ))
          else S.mouse_callback in
      window#register_mouse_callback mouse_callback;
      window#register_scroll_callback S.scroll_callback;
      S.on_load ~window
    val mutable _active_scene = initial_scene
    val _window = window

    method switch_scene (module S : Scene) =
      let (module Old_S) = _active_scene in Old_S.on_unload ~window:_window;
      _window#clear_scene_specific_input_handlers;
      List.iter _window#register_scene_specific_input_handler S.keybindings;
      let mouse_callback = if S.captures_cursor 
          then (fun x y -> (
            if window#is_cursor_shown = false then S.mouse_callback x y
          ))
          else S.mouse_callback in
      _window#register_mouse_callback mouse_callback;
      _window#register_scroll_callback S.scroll_callback;
      _active_scene <- (module S);
      S.on_load ~window:_window

    method render ~(uniforms : uniform list) ~aspect_ratio ~(render_flags : render_flag list) =
      let (module S) = _active_scene in
      let camera = S.active_camera in

      (*Generating scene uniforms*)
      let view_matrix = camera#gen_view_matrix in
      let proj_matrix = camera#gen_projection_matrix ~aspect_ratio in
      let uniforms = [
        Matrix_uniform4f ("view_matrix", view_matrix);
        Matrix_uniform4f ("projection_matrix", proj_matrix)
      ]
      @ List.flatten (List.map (fun (light_type, lights) -> (
        Vector_uniform1i ("number_of_"^light_type, vec1 (List.length lights))
        ::List.flatten (List.mapi (fun i light -> light#get_uniforms ~view_matrix ~uniform_prefix:(light_type^"["^(string_of_int i)^"]")) lights)
      )) S.lights) in

      (*Rendering opaque drawables*)
      List.iter (fun d -> d#render ~window:_window ~uniforms ~render_flags) (let (module S) = _active_scene in S.opaque_drawables);

      (*Rendering translucent drawables in order*)
      let sorted_translucent_drawables = List.rev (List.sort
        (fun d1 d2 -> (
          let d1_cam = vec3_op ( -. ) camera#get_position d1#get_position in
          let d2_cam = vec3_op ( -. ) camera#get_position d2#get_position in
          let d1_squared_dist = dot3f d1_cam d1_cam in
          let d2_squared_dist = dot3f d2_cam d2_cam in
          if d1_squared_dist = d2_squared_dist then 0
          else if d1_squared_dist > d2_squared_dist then 1
          else -1
        )) S.translucent_drawables) in
      List.iter (fun d -> d#render_translucent ~window:_window ~uniforms ~camera_position:camera#get_position ~render_flags) sorted_translucent_drawables;

      (*Handling render flags*)
      List.iter (fun r -> match r with
        | Debug_display_local_bases vector_length -> (
            (*Local bases visualizer*)
            (*Generating VAO*)
            let nr_of_objects = List.length S.movables in
            let local_bases_vao = new vertex_array 
              ~kind:Float32
              ~vertex_attributes:(
                let pos,x_axes,y_axes,z_axes = List.fold_left (fun (pos,x_axes,y_axes,z_axes) d -> (
                  let x,y,z = base_of_rot ~quat:d#get_rotation in
                  ((Vector3 d#get_position)::pos, (Vector3 x)::x_axes, (Vector3 y)::y_axes, (Vector3 z)::z_axes)
                )) ([],[],[],[]) (S.movables) in
                [new vertex_attribute ~attribute_type:(Vector3_kind Float32) ~number_of_vertices:nr_of_objects ~vector_list:pos ();
                 new vertex_attribute ~attribute_type:(Vector3_kind Float32) ~number_of_vertices:nr_of_objects ~vector_list:x_axes ();
                 new vertex_attribute ~attribute_type:(Vector3_kind Float32) ~number_of_vertices:nr_of_objects ~vector_list:y_axes ();
                 new vertex_attribute ~attribute_type:(Vector3_kind Float32) ~number_of_vertices:nr_of_objects ~vector_list:z_axes ();])
              ~element_buffer:(new buffer ~buffer_type:Gl.element_array_buffer ~kind:Int16_unsigned)
              ~number_of_drawn_vertices:nr_of_objects
              ~drawing_type:Gl.static_draw in
            (*rendering local bases*)
            local_bases_vao#bind ();
            local_bases_visualizer_shader#get#use ();
            local_bases_visualizer_shader#get#set_uniform_matrix4f ~name:"view_matrix" ~value:view_matrix;
            local_bases_visualizer_shader#get#set_uniform_matrix4f ~name:"projection_matrix" ~value:proj_matrix;
            local_bases_visualizer_shader#get#set_uniform1f ~name:"vector_length" ~value:(vec1 vector_length);
            Gl.draw_arrays Gl.points 0 nr_of_objects;
            local_bases_vao#unbind ();
            local_bases_vao#delete ())
        (*Once turned on, this is permanent unless the app is restarted, I'm too lazy to do better*)
        | Debug_wireframe -> Gl.polygon_mode Gl.front_and_back Gl.line;
        | _ -> ()
      ) render_flags

    method tick ~elapsed_time =
      let (module S) = _active_scene in
      S.tick ~window:_window ~elapsed_time;
      List.iter (fun m -> m#tick ~elapsed_time) S.movables
  end
