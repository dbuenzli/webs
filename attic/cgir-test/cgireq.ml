(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ----------------------------------------------------------------------------*)

let str = Printf.sprintf
let apply f x ~finally y = 
  let result = try f x with exn -> finally y; raise exn in
  finally y;
  result

let copy i o = 
  let buf_size = 8192 in 
  let buf = String.create buf_size in 
  let rec aux () = match input i buf 0 buf_size with
  | 0 -> () 
  | n -> ignore (output o buf 0 n); aux () in
  aux ()

let cgi_request cmd headers = 
  let env = Array.of_list (List.map (fun (h, v) -> h ^ "=" ^ v) headers) in
  try Unix.execvpe cmd [| cmd |] env with
  | Unix.Unix_error (e, _, _) -> 
      failwith (str "%s: %s." cmd (Unix.error_message e))

let scgi_request host port headers =
  try
    let aux ic oc headers = 
      Cgir.output_scgi oc headers;
      copy stdin oc;
      flush oc;
      Unix.shutdown_connection ic;
      copy ic stdout;
      close_out stdout;
    in
    let addr = 
      try (Unix.gethostbyname host).Unix.h_addr_list.(0) with
      | Not_found -> failwith (str "host %s is unknown" host)
    in
    let saddr = Unix.ADDR_INET (addr, port) in
    let ic, oc = Unix.open_connection saddr in
    apply (aux ic oc) headers ~finally:close_in ic
  with
  | Unix.Unix_error (Unix.EPIPE, _, _) -> 
      failwith (str "connection closed by %s (broken pipe)." host)
  | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
      failwith (str "connection reset by %s." host)
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      failwith (str "connection on %s:%d refused." host port)

let main () = 
  let exec = Filename.basename Sys.executable_name in
  let pr_err fmt = Printf.eprintf ("%s:" ^^ fmt ^^ "%!") exec in
  let usage = str 
      "Usage: %s <options> (<header>=<value>)*\n  \
       Makes a (S)CGI request with given headers, <value> can be empty. \
       Request body\n  (if any) is read from stdin and reply is written back \
       on stdout. For SCGI,\n  headers are written in the given order.\n\
       Options:" exec
  in
  let split_header h = 
    try
      let l = String.length h in 
      let i = String.index h '=' in 
      String.sub h 0 i, String.sub h (i + 1) (l - i - 1)
    with Not_found ->
      raise (Arg.Bad (str "argument %s not of the form <header>=<value>" h))
  in
  let headers = ref [] in 
  let protocol = ref `SCGI in
  let host = ref "localhost" in
  let port = ref 9999 in
  let options = [
    "-cgi", Arg.String (fun cmd -> protocol := `CGI cmd),
    "<script>, CGI request with <script>";
    "-scgi", Arg.Unit (fun () -> protocol := `SCGI),
    "SCGI request on <host>:<port> (default).";
    "-h", Arg.Set_string host,
    "<host>, host for SCGI requests (defaults to localhost).";
    "-p", Arg.Set_int port, 
    "<int>, port for SCGI requests (defaults to 9999)."; ]
  in
  try 
    Arg.parse options (fun h -> headers := split_header h :: !headers) usage;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore; (* exceptions vs. kill *)
    match !protocol with 
    | `CGI cmd -> cgi_request cmd (List.rev !headers)
    | `SCGI -> scgi_request !host !port (List.rev !headers)
  with 
  | Failure e | Sys_error e -> pr_err " %s\n" e; exit 1
  | Unix.Unix_error (e, f, v) -> 
      let v = if v = "" then "" else str " on %s" v in 
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
