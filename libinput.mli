(* libInput: program input representation *)

(** libinput main; defines input vector

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

open Libinput_type
open Libinput_value

(** Create an input vector from scratch *)
val create_vector :
     (valuestr * arg_attrib) list  (** argument list *)
  -> (string * valuestr) list      (** file list *)
  -> valuestr option               (** stdin *)
  -> (string * valuestr) list      (** env variables list *)
  -> (string * valuestr) list      (** socket list *)
  -> input_vector

(** [vector_to_string v hex] converts an input vector into a string.
    The string contains hexademical values when [hex] is true.
  *)
val vector_to_string : input_vector -> bool -> string

(** [output_vector v hex ch] prints an input vector into the channel [ch].
    The string contains hexademical values when [hex] is true.
 *)
val output_vector : input_vector -> bool -> out_channel -> unit

(** [to_json_string v] returns a json string from an input vector. *)
val to_json_string : ?encoding:bool -> input_vector -> string

(** [of_json_string s] returns an input vecdtor from a json string. *)
val of_json_string : string -> input_vector

(** Explicitly set the program target *)
val set_binary_target : args_t -> string -> args_t

(** Get the binary target, if there's no such thing raise Not_found *)
val get_binary_target : input_vector -> string

(** Get command lines as a list of string *)
val get_commandline : ?rich:bool -> ?escape:bool -> input_vector -> string list

(** Get command lines as a string *)
val get_commandline_str : ?rich:bool -> input_vector -> string

(** Check if two input vectors have the same command lines when their file name
    arguments are normalized (i.e., file name can have different string, but
    when it can be normalized) *)
val have_equiv_cmds : input_vector -> input_vector -> bool

(** Hash of an input vector *)
val input_hash : input_vector -> normalize:bool -> int

(* Check if this argument has file attribute *)
val is_file_arg : Argument.t -> bool

(** Normalize file i/o arguments to have a single filename *)
val normalize_args : input_vector -> input_vector

(** [find_arg idx iv] returns the [idx]th argument from the [iv]. *)
val find_arg : int -> input_vector -> Argument.t

(** [find_arg_val idx iv] returns the value of the [idx]th argument. *)
val find_arg_val : int -> input_vector -> value

(** [find_file n iv] returns the file of name [n] from the [iv]. *)
val find_file : string -> input_vector -> File.t

(** [find_file_val n iv] returns the value of the file [n] from the [iv]. *)
val find_file_val : string -> input_vector -> value

(** [find_stdin iv] returns the stdin from the [iv]. *)
val find_stdin : input_vector -> Stdin.t

(** [find_stdin_val iv] returns the value of the stdin from the [iv]. *)
val find_stdin_val : input_vector -> value

(** [find_env n iv] returns the env of name [n] from the [iv]. *)
val find_env : string -> input_vector -> Env.t

(** [find_env_val n iv] returns the value of the env [n] from the [iv]. *)
val find_env_val : string -> input_vector -> value

(** [find_socket n iv] returns the socket of name [n] from the [iv]. *)
val find_socket : string -> input_vector -> Socket.t

(** [find_socket_val n iv] returns the value of the socket [n] from the [iv]. *)
val find_socket_val : string -> input_vector -> value

(***** blob marshaling (blob is a string) *****)
val arg_blob : input_vector -> string
val file_blob : input_vector -> string
val stdin_blob : input_vector -> string
val env_blob : input_vector -> string
val socket_blob : input_vector -> string

val blob_to_args : string -> args_t
val blob_to_files : string -> files_t
val blob_to_stdin : string -> stdin_t
val blob_to_envs : string -> envs_t
val blob_to_sockets: string -> sockets_t

