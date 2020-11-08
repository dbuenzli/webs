/*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   --------------------------------------------------------------------------*/

/* Note, this can go once we require an OCaml version that has
   https://github.com/ocaml/ocaml/pull/10047 */

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_WEBS_DARWIN

#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_WEBS_POSIX
 #endif

#elif defined (_WIN32)
#define OCAML_WEBS_WINDOWS
#define WIN32_LEAN_AND_MEAN

#endif

/* OCaml includes */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#define OCAML_WEBS_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Webs stubs: " ERR)); } \
  while (0)

/* POSIX */

#if defined(OCAML_WEBS_DARWIN) || defined(OCAML_WEBS_POSIX)

CAMLprim value ocaml_webs_realpath (value p)
{
  CAMLparam1 (p);
  char *r;
  value rp;

  caml_unix_check_path (p, "realpath");
  r = realpath (String_val (p), NULL);
  if (r == NULL) { uerror ("realpath", p); }
  rp = caml_copy_string (r);
  free (r);
  CAMLreturn (rp);
}

/* Windows */

#elif

#include <windows.h>
#include <fileapi.h>
#include <stdio.h>

CAMLprim value ocaml_webs_realpath (value p)
{
  CAMLparam1 (p);
  HANDLE h;
  wchar_t *wp;
  wchar_t *wr;
  DWORD wr_len;
  value rp;

  caml_unix_check_path (p, "realpath");
  wp = caml_stat_strdup_to_utf16 (String_val (p));
  h = CreateFile (wp, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
                  FILE_FLAG_BACKUP_SEMANTICS, NULL);
  caml_stat_free (wp);

  if (h == INVALID_HANDLE_VALUE)
  {
    win32_maperr (GetLastError ());
    uerror ("realpath", p);
  }

  wr_len = GetFinalPathNameByHandle (h, NULL, 0, VOLUME_NAME_DOS);
  if (wr_len == 0)
  {
    win32_maperr (GetLastError ());
    CloseHandle (h);
    uerror ("realpath", p);
  }

  wr = caml_stat_alloc ((wr_len + 1) * sizeof (wchar_t));
  wr_len = GetFinalPathNameByHandle (h, wr, wr_len, VOLUME_NAME_DOS);

  if (wr_len == 0)
  {
    win32_maperr (GetLastError ());
    CloseHandle (h);
    caml_stat_free (wr);
    uerror ("realpath", p);
  }

  rp = caml_copy_string_of_utf16 (wr);
  CloseHandle (h);
  caml_stat_free (wr);
  CAMLreturn (rp);
}

#else
#warning OCaml webs library: unsupported platform, realpath will fail.

CAMLprim value ocaml_webs_realpath (value p)
{
  OCAML_WEBS_RAISE_SYS_ERROR ("realpath unimplemented on this platform");
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
