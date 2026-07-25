open Data_structures
open Save

class ['a] setting ~handle ~to_string ~from_string ~default_value =
  let _setting = ref default_value in
  object (self)
    inherit ['a] persistent ~handle ~to_string ~from_string
      ~read_target:(fun () -> (!_setting))
      ~write_target:(fun x -> _setting := x)
    method get =
      (!_setting)
    method to_string = to_string
  end

let max_number_of_lights = new setting 
  ~handle:"ENGINE_SETTINGS_MAX_NUMBER_OF_LIGHTS"
  ~to_string:string_of_int
  ~from_string:int_of_string
  ~default_value:16

module type Setting = sig
  type setting_type
  val setting : setting_type setting
end

let settings = [(module struct type setting_type = int;; let setting = max_number_of_lights end : Setting)]

let settings_as_persistents = [
  (module struct type persistent_type = int;; let persistent = (max_number_of_lights :> int persistent) end : Persistent)
]

let find_and_replace working_str ident value =
  let rec find_ident_indices str i starting_index =
    if str = "" then [] else
    if i = String.length str then [] else
    if str.[i] = ident.[i - starting_index] then
      if i - starting_index = String.length ident - 1 then
        starting_index::(find_ident_indices str (i+1) (i+1))
      else find_ident_indices str (i+1) starting_index
    else find_ident_indices str (i+1) (i+1)
  in let ident_indices = find_ident_indices working_str 0 0 in
  fst (List.fold_left (fun (str, nr_of_replace) index -> (
    let corrected_index = index - (String.length ident - String.length value)*nr_of_replace in
    ((String.sub str 0 corrected_index)^value^(String.sub str (corrected_index + (String.length ident)) (String.length str - (corrected_index + (String.length ident)))), nr_of_replace+1)
  )) (working_str, 0) ident_indices)


let replace_string_with_settings str =
  List.fold_left (fun str (module Setting : Setting) -> (
    find_and_replace str Setting.setting#get_handle (Setting.setting#to_string Setting.setting#get)
  )) str settings

let settings_save = new json_save ~path:"engine_settings.json"
let () = List.iter settings_save#register_persistent settings_as_persistents

let load_settings = settings_save#load

let generate_settings_file = settings_save#save
