type logType = Info | Warning | Error

let canLogToFile = ref true
let isInitialized = ref false
let file : out_channel option ref = ref None

(*Note: as the logger function is used so often, labelled parameters are discarded here to improve readability*)

let logToConsole logType message = 
  match logType with
  | Info -> print_endline ("INFO: "^message)
  | Warning -> print_endline ("WARNING: "^message)
  | Error -> print_endline ("ERROR: "^message)

let initLogger ~enableLogToFile ~logFilePath = if enableLogToFile
  then ((
      try
        file := Some (open_out logFilePath)
      with e -> canLogToFile := false; logToConsole Error "Failed to open the log file. Will only log to console. (Have you tried manually initializing the logger ?)"); 
      isInitialized := true;)
  else canLogToFile := false; isInitialized := true

(*Note: here '!' is not the binary negation operator but is the dereference operator from OCaml*)
let logToFile logType message =
  if not !isInitialized then initLogger ~enableLogToFile:true ~logFilePath:"log/latest_log.txt";
  if !canLogToFile then match !file with
    | Some oc -> (match logType with
      | Info -> output_string oc ("INFO: "^message^"\n")
      | Warning -> output_string oc ("WARNING: "^message^"\n")
      | Error -> output_string oc ("ERROR: "^message^"\n"))
    | None -> ()

let logger ?shouldLogToFile logType message = match shouldLogToFile with
  | Some true | None -> logToFile logType message; logToConsole logType message
  | Some false -> logToConsole logType message
  
