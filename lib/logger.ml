type logType = Info | Warning | Error | Debug | Debug_main_loop

let can_log_to_file = ref true
let is_initialized = ref false
let file : out_channel option ref = ref None
let debug = ref false
let debug_main_loop = ref false

(*Note: as the logger function is used so often, labelled parameters are discarded here to improve readability*)
let log_to_console log_type message = 
  match log_type with
  | Info -> print_endline ("INFO: "^message)
  | Warning -> print_endline ("WARNING: "^message)
  | Error -> print_endline ("ERROR: "^message)
  | Debug -> if !debug then print_endline ("DEBUG: "^message)
  | Debug_main_loop -> if !debug_main_loop then print_endline ("DEBUG: "^message)

let init_logger ~enable_log_to_file ~log_file_path ~enable_debug ~enable_debug_main_loop = debug := enable_debug; debug_main_loop := enable_debug_main_loop; if enable_log_to_file
  then ((
      try
        file := Some (open_out log_file_path)
      with e -> can_log_to_file := false; log_to_console Error "Failed to open the log file. Will only log to console. (Have you tried manually initializing the logger ?)"); 
      is_initialized := true;)
  else can_log_to_file := false; is_initialized := true

(*Note to self: '!' is not a binary negation operator but is the dereference operator in OCaml*)
let log_to_file log_type message =
  if not !is_initialized then init_logger ~enable_log_to_file:true ~log_file_path:"log/latest_log.txt" ~enable_debug:false ~enable_debug_main_loop:false;
  if !can_log_to_file then match !file with
    | Some oc -> (match log_type with
      | Info -> output_string oc ("INFO: "^message^"\n")
      | Warning -> output_string oc ("WARNING: "^message^"\n")
      | Error -> output_string oc ("ERROR: "^message^"\n")
      | Debug -> if !debug then output_string oc ("DEBUG: "^message^"\n")
      | Debug_main_loop -> if !debug_main_loop then output_string oc ("DEBUG: "^message^"\n"))
    | None -> ()

let logger ?should_log_to_file log_type message = match should_log_to_file with
  | Some true | None -> log_to_file log_type message; log_to_console log_type message
  | Some false -> log_to_console log_type message


let string_of_int_list l =
  let rec aux v = match v with
    | [] -> ""
    | a::q -> if q = [] then (string_of_int a)^(aux q) else (string_of_int a)^", "^(aux q) 
  in "["^(aux l)^"]"

let string_of_int_list_capped l n =
  let rec aux v i = match v with
    | [] -> ""
    | a::q -> if i >= n then aux [] i else (if q = [] then (string_of_int a)^(aux q (i+1)) else (string_of_int a)^", "^(aux q (i+1)))
  in "["^(aux l 0)^"]"
