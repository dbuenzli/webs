(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ----------------------------------------------------------------------------*)

let dump req oc = 
  let pr oc status content = 
    Printf.fprintf oc "Status: %s\x0D\x0A" status;
    Printf.fprintf oc "Content-type: text/plain\x0D\x0A\x0D\x0A";
    Printf.fprintf oc "%s%!" content
  in
  try match req with 
  | `Error e -> failwith e
  | `Ok req ->
      pr oc "200 OK"
	(try Cgir.to_string req with 
	| End_of_file -> failwith "truncated request body")
  with
  | Failure e -> 
      pr oc "500 Internal server error" e
      
let serve = function
  | `CGI -> dump (Cgir.input_cgi ()) stdout
  | `SCGI (host, port) -> 
    let addr = 
      try (Unix.gethostbyname host).Unix.h_addr_list.(0) with 
      | Not_found -> failwith (Printf.sprintf "%s: unknown host." host)
    in 
    let saddr = Unix.ADDR_INET (addr, port) in
    let dump ic oc = dump (Cgir.input_scgi ic) oc in 
    Unix.establish_server dump saddr
      
let main () = 
  let exec = Filename.basename Sys.executable_name in
  let pr_err fmt = Printf.eprintf ("%s:" ^^ fmt ^^ "%!") exec in
  let usage = Printf.sprintf
      "Usage: %s <option>\nDumps (S)CGI requests as text/plain.\nOptions:" exec
  in
  let protocol = ref `CGI in
  let host = ref "localhost" in
  let port = ref 9398 in
  let options = [
    "-cgi", Arg.Unit (fun () -> protocol := `CGI),
    "act as a CGI script (default).";
    "-scgi", Arg.Unit (fun () -> protocol := `SCGI),
    "act as a SCGI server on <host>:<port>.";
    "-h", Arg.Set_string host,
    "<host>, host for SCGI server (defaults to localhost).";
    "-p", Arg.Set_int port, 
    "<int>, port for SCGI server (defaults to 9999)."; ]
  in
  try 
    Arg.parse options (fun _ -> ()) usage;
    match !protocol with 
    | `CGI -> serve `CGI
    | `SCGI -> serve (`SCGI (!host, !port))
  with 
  | Failure e -> (pr_err " %s\n" e; exit 1)
  | Unix.Unix_error (e, f, v) -> 
      let v = if v = "" then "" else Printf.sprintf " on %s" v in 
      pr_err " %s%s: %s\n" f v (Unix.error_message e); exit 1

let () = main ()

(*----------------------------------------------------------------------------
  Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the Daniel C. Bünzli nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)
