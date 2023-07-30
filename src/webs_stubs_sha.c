/*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISCe.
   --------------------------------------------------------------------------*/

#include "vendor/sha256.h"
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value ocaml_webs_sha_256 (value msg)
{
  CAMLparam1 (msg);
  CAMLlocal1 (hash);
  struct sha256_ctx ctx;
  sha256_digest d;

  sha256_init (&ctx);
  sha256_update (&ctx, (unsigned char*) String_val (msg),
                 caml_string_length (msg));
  sha256_finalize (&ctx, &d);

  hash = caml_alloc_string (32);
  sha256_to_bin (&d, Bytes_val (hash));

  CAMLreturn (hash);
}
