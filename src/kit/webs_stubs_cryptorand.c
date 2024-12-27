/*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_WEBS_DARWIN
  #include <sys/random.h>
  #include <stdlib.h>

#elif defined (_WIN32)
  #define OCAML_WEBS_WINDOWS
  #include <windows.h>
  # define RtlGenRandom SystemFunction036
  BOOLEAN NTAPI RtlGenRandom(PVOID RandomBuffer, ULONG RandomBufferLength);
  # pragma comment(lib, "advapi32.lib")

#elif defined(__linux__)
  #define OCAML_WEBS_LINUX
  #include <sys/random.h>

#elif defined(__unix__) || defined(__unix) /* This should catch the BSDs */
 #include <unistd.h>
 #include <stdlib.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_WEBS_POSIX
 #endif
#endif

/* OCaml includes */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define CAML_INTERNALS /* Not good */
#include <caml/sys.h>

#define OCAML_WEBS_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Webs stubs: " ERR)); } \
  while (0)

/* getrandom */

#if defined (OCAML_WEBS_LINUX)

CAMLprim value ocaml_webs_getrandom (value b)
{
  int n = caml_string_length (b);
  if (getrandom (Bytes_val (b), caml_string_length (b), 0) != n)
    /* Note normally this should not happen */
    caml_sys_error (caml_copy_string("getrandom"));
  return Val_unit;
}

#elif (defined (OCAML_WEBS_DARWIN) || defined (OCAML_WEBS_POSIX))

CAMLprim value ocaml_webs_getrandom (value b)
{
  arc4random_buf (Bytes_val (b), caml_string_length (b));
  return Val_unit;
}

#elif defined (_WIN32)

void ocaml_webs_getrandom (value b)
{
  if (!RtlGenRandom((PVOID)Bytes_val (b), (ULONG)caml_string_length (b))
  { OCAML_WEBS_RAISE_SYS_ERROR ("RtlGenRandom() failed"); };
}

#elif /* Unsupported */

#warning ocaml_webs_getrandom: unsupported platform
void ocaml_webs_getrandom (value b)
{
  OCAML_WEBS_RAISE_SYS_ERROR("getrandom() unimplemented on this platform");
}

#endif

/* getentropy */

#if (defined (OCAML_WEBS_LINUX) || defined (OCAML_WEBS_DARWIN) || \
     defined (OCAML_WEBS_POSIX))

CAMLprim value ocaml_webs_getentropy (value b)
{
  if (getentropy (Bytes_val (b), caml_string_length (b)) != 0)
    caml_sys_error (caml_copy_string("getentropy"));
  return Val_unit;
}

#elif defined (_WIN32)

void ocaml_webs_getentropy (value b)
{
  if (!RtlGenRandom((PVOID)Bytes_val (b), (ULONG)caml_string_length (b))
  { OCAML_WEBS_RAISE_SYS_ERROR ("RtlGenRandom() failed"); };
}

#elif /* Unsupported */

#warning ocaml_webs_getentropy: unsupported platform
void ocaml_webs_getentropy (value b)
{
  OCAML_WEBS_RAISE_SYS_ERROR("getentropy() unimplemented on this platform");
}

#endif
