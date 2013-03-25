open Webs;;


let main () = 
  let str = Printf.sprintf in
  let exec = Filename.basename Sys.executable_name in
  let pr_err fmt = Printf.eprintf ("%s:" ^^ fmt ^^ "%!") exec in
  let usage = 
    str "Usage: %s <option>\nDumps Wsi requests as text/plain.\nOptions:" exec
  in
  let connector = ref (module Connector.CGI : Connector.T) in
  let host = ref "localhost" in
  let port = ref 9398 in
  let options = [
    "-cgi", 
    Arg.Unit (fun () -> connector := (module Connector.CGI : Connector.T)),
    "use the CGI connector (default).";
    "-scgi",
    Arg.Unit (fun () -> connector := (module Connector.SCGI : Connector.T)),
    "act as a SCGI server on <host>:<port>.";
    "-h", Arg.Set_string host,
    "<host>, host for SCGI server (defaults to localhost).";
    "-p", Arg.Set_int port, 
    "<int>, port for SCGI server (defaults to 9398)."; ]
  in
  try
    Arg.parse options (fun _ -> ()) usage;
    let module C = (val !connector : Connector.T) in
    let addr = 
      try Unix.ADDR_INET ((Unix.gethostbyname !host).Unix.h_addr_list.(0),!port)
      with Not_found -> failwith (str "%s: unknown host." !host)
    in
    let conf = Connector.Conf.(add default listen (`Addr addr)) in 
    match Connector.(C.connect conf Service.echo) with 
    | `Ok -> exit 0 
    | `Error (`Connector e) -> pr_err " connector: %s\n" e; exit 1
    | `Error (`Service exn) -> pr_err " service error\n"; exit 1
    | `Error (`Webserver e) -> pr_err " webserver error: %s\n" e; exit 1
    with Failure e -> pr_err " %s" e; exit 1
    
let () = main ()
