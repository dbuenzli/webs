/*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

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
  off_t offs = Int_val (off);
  ssize_t res;
  caml_release_runtime_system ();
  res = sendfile (Int_val (dst), Int_val (src), &offs, Int_val (len));
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
