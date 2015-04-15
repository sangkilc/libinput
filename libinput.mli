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

module V = Libinput_value
module T = Libinput_type

(** [vector_to_string v hex] converts an input vector into a string.
    The string contains hexademical values when [hex] is true.
  *)
val vector_to_string : T.input_vector -> bool -> string

(** [output_vector v hex ch] prints an input vector into the channel [ch].
    The string contains hexademical values when [hex] is true.
 *)
val output_vector : T.input_vector -> bool -> out_channel -> unit

(** [to_json v] returns a json string from an input vector. *)
val to_json : ?encoding:bool -> T.input_vector -> string

(** [of_json s] returns an input vecdtor from a json string. *)
val of_json : string -> T.input_vector

(** Explicitly set the program target *)
val set_binary_target : T.args_t -> string -> T.args_t

(** Get the binary target, if there's no such thing raise Not_found *)
val get_binary_target : T.input_vector -> string

(** Get command lines as a list of string *)
val get_commandline : ?rich:bool
                   -> ?escape:bool
                   -> T.input_vector
                   -> string list

(** Get command lines as a string *)
val get_commandline_str : ?rich:bool -> T.input_vector -> string

(** Check if two input vectors have the same command lines when their file name
    arguments are normalized (i.e., file name can have different string, but
    when it can be normalized) *)
val have_equiv_cmds : T.input_vector -> T.input_vector -> bool

(** Hash of an input vector *)
val input_hash : T.input_vector -> normalize:bool -> int

(* Check if this argument has file attribute *)
val is_file_arg : T.Argument.t -> bool

(** Normalize file i/o arguments to have a single filename *)
val normalize_args : T.input_vector -> T.input_vector

(** [find_arg idx iv] returns the [idx]th argument from the [iv]. *)
val find_arg : int -> T.input_vector -> T.Argument.t

(** [find_arg_val idx iv] returns the value of the [idx]th argument. *)
val find_arg_val : int -> T.input_vector -> V.value

(** [find_file n iv] returns the file of name [n] from the [iv]. *)
val find_file : string -> T.input_vector -> T.File.t

(** [find_file_val n iv] returns the value of the file [n] from the [iv]. *)
val find_file_val : string -> T.input_vector -> V.value

(** [find_stdin iv] returns the stdin from the [iv]. *)
val find_stdin : T.input_vector -> T.Stdin.t

(** [find_stdin_val iv] returns the value of the stdin from the [iv]. *)
val find_stdin_val : T.input_vector -> V.value

(** [find_env n iv] returns the env of name [n] from the [iv]. *)
val find_env : string -> T.input_vector -> T.Env.t

(** [find_env_val n iv] returns the value of the env [n] from the [iv]. *)
val find_env_val : string -> T.input_vector -> V.value

(** [find_socket n iv] returns the socket of name [n] from the [iv]. *)
val find_socket : string -> T.input_vector -> T.Socket.t

(** [find_socket_val n iv] returns the value of the socket [n] from the [iv]. *)
val find_socket_val : string -> T.input_vector -> V.value

(***** blob marshaling (blob is a string) *****)
val arg_blob : T.input_vector -> string
val file_blob : T.input_vector -> string
val stdin_blob : T.input_vector -> string
val env_blob : T.input_vector -> string
val socket_blob : T.input_vector -> string

val blob_to_args : string -> T.args_t
val blob_to_files : string -> T.files_t
val blob_to_stdin : string -> T.stdin_t
val blob_to_envs : string -> T.envs_t
val blob_to_sockets: string -> T.sockets_t

