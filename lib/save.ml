open Data_structures
open Logger
open Yojson.Basic

module type Persistent = sig
  type persistent_type
  val persistent : persistent_type persistent
end 

class virtual save =
  object (self)
    method virtual register_persistent : (module Persistent) -> unit
    method virtual save : unit -> unit
    method virtual load : unit -> unit
  end

class json_save ~(path : string) =
  object (self)
    inherit save
    val _path = path 
    val mutable _save_contents : (module Persistent) list = []
    method register_persistent p =
      _save_contents <- p::_save_contents
    method save () =
      try
        let file = Out_channel.open_gen [Open_trunc;Open_append;Open_creat] 0o644 _path in
        output_string file "{\n";
        let rec write_persistents l = match l with
          | [] -> ()
          | a::q -> (
            let (module P : Persistent) = a in
            output_string file ("\""^P.persistent#get_handle^"\": ");
            P.persistent#save file;
            output_string file (if q = [] then "\n" else ",\n");
            write_persistents q)
        in write_persistents _save_contents;
        output_string file "}"; close_out file
      with e ->
        logger Warning ("json_save: failed to save at path \""^_path^"\". See error below:");
        logger Warning ("json_save: "^Printexc.to_string e)
    method load () =
      try
        let json = from_file _path in
        List.iter (fun (module P : Persistent) ->(
          try
            let var = to_string (Util.member P.persistent#get_handle json) in
            with_channel_from_string var P.persistent#load
          with 
            | Util.Type_error (e, t) -> logger Warning ("json_save: failed to load persistent with handle \""^P.persistent#get_handle^"\" because of corrupted json save. Exception: "^e)
            | e -> logger Warning ("json_save: failed to load persistent with handle \""^P.persistent#get_handle^"\". Exception: "^Printexc.to_string e)
        )) _save_contents
      with e ->
        logger Warning ("json_save: failed to load save at path \""^_path^"\". See error below:");
        logger Warning ("json_save: "^Printexc.to_string e)
  end