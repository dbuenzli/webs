(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hashing tools. *)

open Webs

(** SHA-256 hashes, HMAC-SHA-256 and PBKDF2-HMAC-SHA-256. *)
module Sha_256 : sig

  (** {1:values SHA-256 hashes} *)

  type t
  (** The type for SHA-256 hashes. *)

  val length : t -> int
  (** [length h] is the length of [h] in bytes (i.e. 32). *)

  val hash : string -> t
  (** [hash s] is the SHA-256 hash of [s]. *)

  (** {1:hmac HMAC-SHA-256} *)

  val hmac : key:string -> string -> t
  (** [hmac ~key msg] is the {{:https://tools.ietf.org/html/rfc2104}RFC 2104}
      HMAC-SHA-256 for key [key] and message [msg].
      [key] should not be less than 32 bytes. *)

  (** {1:pbkdf2 PBKDF2-HMAC-SHA-256} *)

  type salt = string
  (** The type for password salt bytes. *)

  type key = string
  (** The type for password derived keys bytes. *)

  val random_salt : ?random_state:Random.State.t -> length:int -> unit -> salt
  (** [random_salt ~length ()] are [length] bytes sourced from the given
      PRNG state (defaults to {!Stdlib.Random.make_self_init}). *)

  val pbkdf2_hmac :
    key_length:int -> iterations:int -> salt:salt -> password:string ->
    unit -> key
  (** [pbkdf2_hmac ~key_length ~iterations ~salt ~password ()] derives
      a key for password [password] with a salt [salt] and
      [iterations] iterations to generate a key of length [key_length]
      using {{:https://tools.ietf.org/html/rfc8018}RFC 8018}'s
      PBKFD2-HMAC-SHA-256.

      {b Important.} Use {!equal_key} to compare derived keys,
      not {!String.equal} or [( = )].

      In 2023, here is a good baseline of parameters:

      {ul
      {- A [key_length] of [32] bytes.}
      {- A number of [iterations] of [600_000].}
      {- A [salt] length of [16] bytes.}}

      See also the
      {{:https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html}
      OWASP password storage cheet}.

      Raises [Invalid_argument] if [key_len] or [iterations] are smaller or
      equal to [0] or if [key_len] is greater than 2{^32} - 1 * 32
      or [max_int].
  *)

  val equal_key : key -> key -> bool
  (** [equal_key k0 k1] is a constant time string equality for [k0]
      and [k1] of the same length. Do not use to compare strings of
      different lengths this raises [Invalid_argument] in that
      case. *)

  (** {1:predicates Predicates} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is a constant time equality comparison function
      between hashes [h0] and [h1]. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  (** [to_binary_string h] is the sequence of bytes of [h]. *)

  val of_binary_string : string -> (t, unit) result
  (** [of_binary_string s] is the sequence of bytes of [s] as a hash value.
      An error is returned if the length of [s] in not 32. *)

  val to_hex : t -> string
  (** [to_hex h] is the sequence of bytes of [h] as US-ASCII lowercase
      hexadecimal digits. *)

  val of_hex : string -> (t, int) result
  (** [of_hex s] parses a sequence of US-ASCII (lower or upper cased)
      hexadecimal digits to its hash value. Errors with an offending
      index or the length of the string in case [s] was
      not exactly made of 64 US-ASCII hex digits. *)

  val of_hex' : string -> (t, string) result
  (** [of_hex'] is of {!of_hex} but errors with an english error message. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats digests with {!to_hex}. *)
end
