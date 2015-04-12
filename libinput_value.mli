(* libInput: program input representation *)

(** define input value

    @author Sang Kil Cha <sangkil.cha\@gmail.com>
    @since  2013-11-27

 *)

(*
Copyright (c) 2013, Sang Kil Cha
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL SANG KIL CHA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*)

(** We represent an input byte as a character. Each byte can either be symbolic
    or concrete. *)
type byte =
  | Symbolic of char
  | Concrete of char

(** An input value is an array of bytes *)
type value = byte array

(** A string representation of a value *)
type valuestr = string * string option

(** Input value to string *)
val value_to_string : ?tohex:bool -> value -> string

(** [valuestr_to_value sv] returns an input value from a string value [sv], which
    consists of a tuple of a string and a kind string. The kind string
    represents kinds of each byte with 0 and 1. See byte_kind_string for more
    information.
  *)
val valuestr_to_value : valuestr -> value

(** [string_to_conc_val s] returns an input value from an ascii string [s]. The
    value will contain only concrete bytes. *)
val string_to_conc_val : string -> value

(** [byte_kind_string v] returns a byte kinds string represented with 0 and 1
    from [v]. If a string "abcd" is represented with a kind string "0110", then
    this means that the first and the fourth bytes ('a' and 'd' respectively)
    are concrete bytes, and the second and the third bytes are symbolic. *)
val byte_kind_string : value -> string

(** Get a character at a position (pos) from an input value *)
val get_char_value : value -> int -> char

(** Fold on each byte of a value *)
val value_fold : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b

val make_abstract : ?makeall:bool -> value -> int -> value
val make_concrete : value -> int -> value

