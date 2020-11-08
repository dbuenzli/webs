/*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   --------------------------------------------------------------------------*/

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_WEBS_DARWIN

#elif defined(__linux__)
  #define OCAML_WEBS_LINUX

#elif defined (_WIN32)
  #define OCAML_WEBS_WINDOWS
  #define WIN32_LEAN_AND_MEAN
#endif

/* OCaml includes */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#define OCAML_WEBS_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Webs stubs: " ERR)); } \
  while (0)

/* Darwin */

#if defined(OCAML_WEBS_DARWIN)
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

CAMLprim value ocaml_webs_sendfile (value src, value off, value len, value dst)
{
  CAMLparam4 (src, off, len, dst);
  int res;
  off_t l = Int_val (len);
  caml_release_runtime_system ();
  res = sendfile (Int_val (src), Int_val (dst), Int_val (off), &l, NULL, 0);
  caml_acquire_runtime_system ();
  if (res == -1) { uerror ("sendfile", Nothing); }
  CAMLreturn (Val_long (l));
}

/* Linux */

#elif defined(OCAML_WEBS_LINUX)

#include <sys/sendfile.h>

CAMLprim value ocaml_webs_sendfile (value src, value off, value len, value dst)
{
  CAMLparam4 (src, off, len, dst);
  off_t off = Int_val (off);
  ssize_t res;
  caml_release_runtime_system ();
  res = sendfile (Int_val (dst), Int_val (src), &off, Int_val (len));
  caml_acquire_runtime_system ();
  if (res == -1) { uerror ("sendfile", Nothing); }
  CAMLreturn (Val_long (res));
}

/* Unsupported */

#else
#warning OCaml webs library: unsupported platform, sendfile will fail.

CAMLprim value ocaml_webs_sendfile (value src, value dst, value off, value len)
{
  OCAML_WEBS_RAISE_SYS_ERROR ("sendfile unimplemented on this platform");
}
#endif

/*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
