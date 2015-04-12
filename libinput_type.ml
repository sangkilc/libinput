(* libInput: program input representation *)

(** define libinput types

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

open Libinput_value

(*****************************************************************************)
(* Basic Map Structures                                                      *)
(*****************************************************************************)
module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

module StringMap = Map.Make(struct
  type t = string
  let compare = String.compare
end)

module BoolMap = Map.Make(struct
  type t = bool
  let compare = compare
end)

(*****************************************************************************)
(* Basic Types                                                               *)
(*****************************************************************************)

(** To represent "no" attribute *)
type no_attrib_t = bool option
let no_attrib = None

(** This module specifies the input kind. *)
module type InputKind =
sig

  type name   (* what's the name of this input value (e.g., argument index)? *)
  type attrib (* what kind of attributes does this input have? *)

  val kind : string (* what is the input kind in string *)
  val default_attrib : attrib (* what is the default attribute *)

  val name_to_string : name -> string
  val get_attrib_string : attrib -> string

end

(** The supertype input. *)
module Input =
  functor (K: InputKind) ->
struct
  type attrib = K.attrib           (* attribute *)

  type name = K.name               (* name of the input *)

  type t = value * name * attrib   (* input type *)

  let default_attrib = K.default_attrib

  (** [to_string i hex] returns a string representation of the input. When [hex]
      is true, the returned string will use hexadecimal representation for input
      values. *)
  let to_string (v, name, _attrib) tohex =
    let vs = value_to_string ~tohex:tohex v in
    Printf.sprintf "%s[%s];%d = %s"
      K.kind (K.name_to_string name) (Array.length v) vs

  let create (n:name) (a:attrib) (v:value) = v, n, a

  let get_name ((_, n, _):t) = n

  let get_value ((v, _, _):t) = v

  let get_conc_value ((v, _, _):t) = value_to_string v

  let get_attrib ((_, _, a):t) = a

  let get_attrib_string ((_, _, a):t) = K.get_attrib_string a

  let get_length t = get_value t |> Array.length

  let get_char pos t = get_char_value (get_value t) pos

  let is_concrete t =
    let check_conc = function
      | Concrete _ -> true
      | _ -> false
    in
    BatArray.for_all check_conc (get_value t)

  let is_char_concrete pos t =
    match Array.get (get_value t) pos with
    | Concrete _ -> true
    | _ -> false
end

(*****************************************************************************)
(* Argument                                                                  *)
(*****************************************************************************)

(** Argument attribute *)
type arg_attrib =
  | ArgReg               (* regular argument *)
  | ArgFileIn of string  (* this argument stores the input file path *)
  | ArgFileOut of string (* this argument stores the output file path *)

(** Return an argument attribute from a string *)
let string_to_arg_attrib =
  let re1 = Str.regexp "in(\\(.*\\))" in
  let re2 = Str.regexp "out(\\(.*\\))" in
  function
  | "regular" | "reg" -> ArgReg
  | other ->
      if Str.string_match re1 other 0 then
        ArgFileIn (Str.matched_group 1 other)
      else if Str.string_match re2 other 0 then
        ArgFileOut (Str.matched_group 1 other)
      else
        failwith "unknown arg type string"

module Argument = Input(struct

  type name = int
  type attrib = arg_attrib

  let kind = "arg"
  let default_attrib = ArgReg

  let name_to_string = string_of_int

  let get_attrib_string = function
  | ArgReg -> "regular"
  | ArgFileIn file -> Printf.sprintf "in(%s)" file
  | ArgFileOut file -> Printf.sprintf "out(%s)" file

end)

module ArgMap = IntMap
type args_t = Argument.t ArgMap.t

(*****************************************************************************)
(* File                                                                      *)
(*****************************************************************************)
module File = Input(struct

  type name = string (* file name *)
  type attrib = int  (* permission *)

  let kind = "file"
  let default_attrib = 0o766 (* rwxrw-rw- *)

  let name_to_string = BatPervasives.identity
  let get_attrib_string attrib = Printf.sprintf "0o%o" attrib

end)

module FileMap = StringMap
type files_t = File.t FileMap.t

(*****************************************************************************)
(* Stdin                                                                     *)
(*****************************************************************************)
module Stdin = Input(struct

  type name = bool
  type attrib = no_attrib_t

  let kind = "stdin"
  let default_attrib = no_attrib

  let name_to_string _ = ""
  let get_attrib_string _ = ""

end)

module StdinMap = BoolMap
type stdin_t = Stdin.t StdinMap.t

(*****************************************************************************)
(* Envs                                                                      *)
(*****************************************************************************)
module Env = Input(struct

  type name = string
  type attrib = no_attrib_t

  let kind = "env"
  let default_attrib = no_attrib

  let name_to_string = BatPervasives.identity
  let get_attrib_string _ = ""

end)

module EnvMap = StringMap
type envs_t = Env.t EnvMap.t

(*****************************************************************************)
(* Sockets                                                                   *)
(*****************************************************************************)
module Socket = Input(struct

  type name = string
  type attrib = no_attrib_t

  let kind = "socket"
  let default_attrib = no_attrib

  let name_to_string = BatPervasives.identity
  let get_attrib_string _ = ""

end)

module SocketMap = StringMap
type sockets_t = Socket.t SocketMap.t

(** Input vector describes a set of all possible inputs in order to launch a
    program. One input vector can contain 5 different input groups, where each
    input group can have multiple inputs. *)
type input_vector =
  {
    args          : args_t;
    files         : files_t;
    stdin         : stdin_t;
    envs          : envs_t;
    sockets       : sockets_t;
  }

(** Null vector *)
let empty_vector =
  {
    args=ArgMap.empty;
    files=FileMap.empty;
    stdin=StdinMap.empty;
    envs=EnvMap.empty;
    sockets=SocketMap.empty;
  }

(*****************************************************************************)
(*****************************************************************************)

