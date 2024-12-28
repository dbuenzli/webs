(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Authenticatable data.

    This module {{!t}defines} a simple US-ASCII compatible encoding
    scheme to publish {b non-encrypted}, expirable data bytes that can
    be authentified with a private key. Human readability and secrecy
    is a non-goal, storing state in non-trusted environments is.

    {b The data is not encrypted}.

    {b Note.} The encoding scheme and module is designed with extensibility
    in mind. But for now only an {{!Webs_hash.Sha_256.val-hmac}HMAC-SHA-256}
    based scheme is defined. *)

type time = int
(** The type for some notion of time to expire data.

    The semantics is left to the client. One can use a logical notion of time
    or the number of seconds since the Unix epoch. *)

(** Private keys. *)
module Private_key : sig

  type crypto_random = int -> string
  (** The type for cryptographically secure random bytes generation.
      Calling the function with [n] must return [n] cryptographically
      secure random bytes. *)

  type t = [
    | `Hs256 of string
    (** Used with {{!Webs_hash.Sha_256.val-hmac}[HMAC-SHA-256]}, hence should
        be at least 32 bytes. *)
  ]
  (** The type for private keys. This defines both a key and a correspoding
      authentification scheme. *)

  val random_hs256 : ?crypto_random:crypto_random -> unit -> t
  (** [random_hs256 ()] are 64 random bytes sourced from the generator
      [crypto_rantom] (defaults to {!Webs_cryptorand.get_random}). *)

  val to_ascii_string : t -> string
  (** [to_ascii_string k] encodes [k] to an URL safe US-ASCII
      scheme that can be read back by {!of_ascii_string}. *)

  val of_ascii_string : string -> (t, string) result
  (** [of_ascii_string s] reads back the encoding of
      {!to_ascii_string}. *)
end

(** {1:auth Authenticatable} *)

type t = string
(** The type for authenticatable bytes. The encoding scheme for bytes
    [data] and an optional expiration timestamp [expire] and private key
    [private_key] is defined by:
{[
exp = match expire with None -> "" | Some e -> string_of_int e
msg = exp ^ ":" ^ data
hs256 = "HS256:" ^ (hmac_sha_256 private_key msg)
auth = base64url_unpadded (hs256 ^ msg)
]}
    with [base64url_unpadded] being
    {{:https://www.rfc-editor.org/rfc/rfc4648#section-5}[base64url]}
    without padding. *)

(** {1:enc Encode} *)

val encode : private_key:Private_key.t -> expire:time option -> string -> t
(** [encode ~private_key ~expire data] makes data [data] expire at
    [expire] (if any) and authenticatable via the private key and
    scheme defined by [private_key]. *)

(** {1:dec Decode} *)

type format_error =
[ `Base64url of Webs.Http.Base64.error (** [base64url] decode error. *)
| `Scheme of string option (** Scheme name, if one was found. *) ]
(** The type for decode format errors. *)

val format_error_message : format_error -> string
(** [format_error_message e] is an english error message for [e]. *)

type error =
[ `Authentication (** Authentication error. *)
| `Expired of time (** Expiration time. *)
| `Missing_now_for of time (** Expiration time. *)
| `Format of format_error (** Decode format error *) ]
(** The type for decode and authentication errors. See {!decode}. *)

val error_message : error -> string
(** [error_message e] is an english error message for [e]. *)

val error_string : ('a, error) result -> ('a, string) result
(** [error_string r] is [Result.map_error error_message r]. *)

val decode :
  private_key:Private_key.t -> now:time option -> t ->
  (time option * string, error) result
(** [decode ~private_key ~now s] authenticates data [s] with
    the private key and scheme defined by [private_key] and expires it
    (if applicable) according to [now]. If [now] is [None] and [s] has
    an expiration timestamp, the result errors. More precisely the
    result is:
    {ul
    {- [Ok (expire, data)] with [data] the authenticated bytes and [expire]
       the expiration timestamp iff [s] is
       authenticated by [private_key] and either:
       {ul
       {- [expire] is [None]}
       {- [expire] is [Some t] and now is [Some now] with [now < t].}}}
    {- [Error `Authentication] if [s] cannot be authenticated by
       [private_key].}
    {- [Error (`Expired t)], if [s] is authenticated by [private_key]
       and expires at [t] but [now] is [Some now] with [now >= t].}
    {- [Error (`Missing_now_for t)], if [s] is authenticated by
       [private_key] and expires at [t] but [now] is [None].}
    {- [Error (`Format _)] if any other decoding error occurs.}} *)

(** {1:untrusted Untrusted decode} *)

type untrusted =
[`Untrusted_hs256 of Webs_hash.Sha_256.t * time option * string ]
(** The type for untrusted decode results. *)

val untrusted_decode : t -> (untrusted, format_error) result
(** [untrusted_decode s] decodes the encoding structure of [s] but
    neither authenticates nor expires the data. *)
