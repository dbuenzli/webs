(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Bytesrw
open Webs

let repeat n = Test.range ~kind:"slice_length" ~first:1 ~last:n

let test_version =
  Test.test "Webs_http11.{encode,decode}_version" @@ fun () ->
  let snap = Snap.result (module Http.Version) in
  snap (Webs_http11.Version.decode "HTTP/0.9") @> __POS_OF__ (Ok (0, 9));
  snap (Webs_http11.Version.decode "HTTP/1.0") @> __POS_OF__ (Ok (1, 0));
  snap (Webs_http11.Version.decode "HTTP/1.1") @> __POS_OF__ (Ok (1, 1));
  snap (Webs_http11.Version.decode "HTTP/1.2") @> __POS_OF__ (Ok (1, 2));
  snap (Webs_http11.Version.decode "HTTP/2.2") @> __POS_OF__ (Ok (2, 2));
  (* curl -i gives us HTTP/2 so we accept single digits. *)
  snap (Webs_http11.Version.decode "HTTP/2") @> __POS_OF__ (Ok (2, 0));
  snap (Webs_http11.Version.decode "HTTP/3") @> __POS_OF__ (Ok (3, 0));
  snap (Webs_http11.Version.decode "HTTP /1.1")
  @> __POS_OF__ (Error "not an HTTP version");
  snap (Webs_http11.Version.decode "HTTP/1.1 ")
  @> __POS_OF__ (Error "not an HTTP version");
  snap (Webs_http11.Version.decode "HTTP/1.10")
  @> __POS_OF__ (Error "not an HTTP version");
  snap (Webs_http11.Version.decode "HTTP/10.1")
  @> __POS_OF__ (Error "not an HTTP version");
  Snap.string (Webs_http11.Version.encode (0, 9)) @> __POS_OF__ "HTTP/0.9";
  Snap.string (Webs_http11.Version.encode (1, 0)) @> __POS_OF__ "HTTP/1.0";
  Snap.string (Webs_http11.Version.encode (1, 1)) @> __POS_OF__ "HTTP/1.1";
  Snap.string (Webs_http11.Version.encode (2, 2)) @> __POS_OF__ "HTTP/2.2";
  ()

let raw_http11_request =
  "GET / HTTP/1.1\r\n\
   Host: example.org\r\n\
   User-Agent: curl/8.7.1\r\n\
   Accept: */*\r\n\
   \r\n"

let http11_request () =
  let version = Http.Version.v11 in
  let headers =
    Http.Headers.empty
    |> Http.Headers.(define host) "example.org"
    |> Http.Headers.(define user_agent) "curl/8.7.1"
    |> Http.Headers.(define accept) "*/*"
  in
  let body = Http.Body.of_bytes_reader (Bytes.Reader.empty ()) in
  Http.Request.make ~version `GET ~raw_path:"/" ~headers body

let raw_http11_response =
  "HTTP/1.1 200 OK\r\n\
   Content-Type: text/html\r\n\
   Content-Length: 121\r\n\
   Connection: keep-alive\r\n\
   allow: GET, HEAD\r\n\
   Accept-Ranges: bytes\r\n\
   Age: 2428\r\n\
   Cache-Control: public, max-age=14400\r\n\
   \r\n\
   <!doctype html><html lang=\"en\">\
   <head><title>Example Domain</title></head>\
   <body><div><h1>Example Domain</h1></body></html>"

let http11_response () =
  let version = Http.Version.v11 in
  let headers =
    Http.Headers.empty
    |> Http.Headers.(define connection) "keep-alive"
    |> Http.Headers.(define allow) "GET, HEAD"
    |> Http.Headers.(define accept_ranges) "bytes"
    |> Http.Headers.(define age) "2428"
    |> Http.Headers.(define cache_control) "public, max-age=14400"
    |> Http.Headers.(define content_type) "text/html"
    |> Http.Headers.(define content_length) "121"
  in
  let body =
    Http.Body.of_bytes_reader
      ~content_type:"text/html"
      ~content_length:121
      (Bytes.Reader.of_string
         "<!doctype html><html lang=\"en\">\
          <head><title>Example Domain</title></head>\
          <body><div><h1>Example Domain</h1></body></html>")
  in
  Http.Response.make ~version Http.Status.ok_200 ~headers body

let test_write_request =
  Test.test "Webs_http11.Request.write" @@ fun () ->
  let writes w = Webs_http11.Request.write ~eod:true w (http11_request ()) in
  Snap.string (Bytes.Writer.writes_to_string writes) @> __POS_OF__
    "GET / HTTP/1.1\r\n\
     user-agent: curl/8.7.1\r\n\
     host: example.org\r\n\
     content-type: application/octet-stream\r\n\
     accept: */*\r\n\
     \r\n";
  ()

let test_write_response =
  Test.test "Webs_http11.Response.write" @@ fun () ->
  let writes w = Webs_http11.Response.write ~eod:true w (http11_response ()) in
  Snap.string (Bytes.Writer.writes_to_string writes) @> __POS_OF__
    "HTTP/1.1 200 OK\r\n\
     content-type: text/html\r\n\
     content-length: 121\r\n\
     connection: keep-alive\r\n\
     cache-control: public, max-age=14400\r\n\
     allow: GET, HEAD\r\n\
     age: 2428\r\n\
     accept-ranges: bytes\r\n\
     \r\n\
     <!doctype html><html lang=\"en\"><head><title>Example Domain</title></head><body><div><h1>Example Domain</h1></body></html>";
  ()

let test_read_request =
  Test.test "Webs_http11.Request.read" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let r = Bytes.Reader.of_string ~slice_length:n raw_http11_request in
    match Webs_http11.Request.read ~service_path:Http.Path.root r with
    | Error _ -> Test.failstop "Could not parse request"
    | Ok request -> Test.eq (module Http.Request) request (http11_request ());
  end

let test_read_response =
  Test.test "Webs_http11.Response.read" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let r = Bytes.Reader.of_string ~slice_length:n raw_http11_response in
    match Webs_http11.Response.read r with
    | Error e -> Test.failstop "Could not parse response: %s" e
    | Ok response ->
        Test.eq (module Http.Response) response (http11_response ())
  end

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
