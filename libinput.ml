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

open V
open T

let acc_str str acc =
  if String.length str = 0 then acc
  else if String.length acc = 0 then str
  else (acc ^"\n"^ str)

(** Input vector to string *)
let vector_to_string vector tohex =
  let folder fn _ arg acc = acc_str (fn arg tohex) acc in
  match vector with
  | {args=args; files=files; stdin=stdin; envs=envs; sockets=sockets} ->
      ArgMap.fold (folder Argument.to_string) args ""
      |> FileMap.fold (folder File.to_string) files
      |> StdinMap.fold (folder Stdin.to_string) stdin
      |> EnvMap.fold (folder Env.to_string) envs
      |> SocketMap.fold (folder Socket.to_string) sockets

(** Print out the input vector to channel *)
let output_vector vector tohex chan =
  let buf = vector_to_string vector tohex in
  if String.length buf = 0 then Printf.fprintf chan "(empty vector)\n"
  else output_string chan (buf^"\n")

(** Convert an input vector into a json string *)
let to_json ?(encoding=true) iv = Libinput_json.to_json encoding iv

(** Read from a json object *)
let of_json = Libinput_json.of_json

(** Explicitly set the binary target *)
let set_binary_target (args: args_t) target =
  let target = string_to_conc_val target in
  let input = Argument.create 0 ArgReg target in
  ArgMap.add 0 input args

(** Get the binary target, if there's no such thing raise Not_found *)
let get_binary_target = function
  | {args=args;} -> ArgMap.find 0 args |> Argument.get_value |> value_to_string

let get_commandline ?rich:(rich=false) ?escape:(escape=false) =
  let angle_regexp = Str.regexp "[$><\"]" in
  let escape_bash_special arg = Str.global_replace angle_regexp "\\\\1" arg in
  let folder proc _idx arg acc =
    let argstr = Argument.get_value arg |> value_to_string in
    (proc arg argstr)::acc
  in
  let color_filearg arg argstr =
    match Argument.get_attrib arg with
    | ArgReg -> argstr
    | ArgFileIn _ -> ("\x1b[35m"^argstr^"\x1b[m")
    | ArgFileOut _ -> ("\x1b[31m"^argstr^"\x1b[m")
  in
  let normal _arg argstr =
    if escape then escape_bash_special argstr else argstr
  in
  function
  | {args=args;} ->
      let cmds =
        if rich then ArgMap.fold (folder color_filearg) args []
        else ArgMap.fold (folder normal) args []
      in
      List.rev cmds

let get_commandline_str ?rich:(rich=false) vector =
  let lst = get_commandline ~rich:rich vector in
  let str = String.concat " " lst in
  if StdinMap.cardinal vector.stdin > 0 then str ^ " < stdin"
  else str

let is_file_arg arg =
  match Argument.get_attrib arg with
    | ArgFileOut _ | ArgFileIn _ -> true
    | _ -> false

let have_equiv_cmds iv1 iv2 =
  if ArgMap.cardinal iv1.args <> ArgMap.cardinal iv2.args then
    false
  else
    ArgMap.fold (fun idx arg equiv ->
      let corresponding_arg = ArgMap.find idx iv2.args in
      if is_file_arg arg && is_file_arg corresponding_arg then equiv
      else equiv
           && ((Argument.get_value arg |> value_to_string)
             = (Argument.get_value corresponding_arg |> value_to_string))
    ) iv1.args true

let normalization_target = "A" (* we make filename it to "A" *)

let arg_path_replacer _filename _fpath = normalization_target

let actual_path_replacer filename fpath =
  let dirpath = Filename.dirname filename in
  let regexp = Str.regexp_string filename in
  let basepath = Str.replace_first regexp normalization_target fpath in
  if dirpath = "." then basepath
  else Filename.concat dirpath basepath

let get_normalized_arg arg idx filename =
  let replace replacer filename actual_path =
    if filename = "/" || filename = "." then filename
    else replacer filename actual_path
  in
  let argpath = replace arg_path_replacer filename filename in
  let new_type, new_actual_name =
    match Argument.get_attrib arg with
    | ArgFileOut actual_path ->
        let newpath = replace actual_path_replacer filename actual_path in
        ArgFileOut newpath, newpath
    | ArgFileIn actual_path ->
        let newpath = replace actual_path_replacer filename actual_path in
        ArgFileIn newpath, newpath
    | _ ->
        failwith "invaid call"
  in
  Argument.create idx new_type (string_to_conc_val argpath), new_actual_name

let normalize_args iv =
  let process_file_arg idx arg iv =
    (* check if the argument contains any symbolic byte *)
    assert (Argument.is_concrete arg);
    (* reset the argument for a file *)
    let filename = Argument.get_value arg |> value_to_string in
    let arg, new_actual_name = get_normalized_arg arg idx filename in
    let args = ArgMap.add idx arg iv.args in
    (* rename the current file input *)
    let file = FileMap.find filename iv.files in
    let files = FileMap.remove filename iv.files in
    let newfile =
      File.create new_actual_name File.default_attrib (File.get_value file)
    in
    let files = FileMap.add new_actual_name newfile files in
    {iv with args=args; files=files}
  in
  ArgMap.fold (fun idx arg iv ->
    try if is_file_arg arg then process_file_arg idx arg iv else iv
    with Not_found -> iv
  ) iv.args iv

let hashfn to_string name v acc =
  (Hashtbl.hash name) + (Hashtbl.hash (to_string v false)) + acc

(** Hash of an input vector *)
let input_hash vector ~normalize:normalize =
  let vector = if normalize then normalize_args vector else vector in
  ArgMap.fold (hashfn Argument.to_string) vector.args 0
  |> FileMap.fold (hashfn File.to_string) vector.files
  |> StdinMap.fold (hashfn Stdin.to_string) vector.stdin
  |> EnvMap.fold (hashfn Env.to_string) vector.envs
  |> SocketMap.fold (hashfn Socket.to_string) vector.sockets

let find_arg idx iv = ArgMap.find idx iv.args
let find_arg_val idx iv =  find_arg idx iv |> Argument.get_value
let find_file name iv = FileMap.find name iv.files
let find_file_val name iv = find_file name iv |> File.get_value
let find_stdin iv = StdinMap.find true iv.stdin
let find_stdin_val iv = find_stdin iv |> Stdin.get_value
let find_env name iv = EnvMap.find name iv.envs
let find_env_val name iv = find_env name iv |> Env.get_value
let find_socket name iv = SocketMap.find name iv.sockets
let find_socket_val name iv = find_socket name iv |> Socket.get_value

let arg_blob vector = Marshal.to_string vector.args []
let file_blob vector = Marshal.to_string vector.files []
let stdin_blob vector = Marshal.to_string vector.stdin []
let env_blob vector = Marshal.to_string vector.envs []
let socket_blob vector = Marshal.to_string vector.sockets []

let blob_to_args blob = Marshal.from_string blob 0
let blob_to_files blob = Marshal.from_string blob 0
let blob_to_stdin blob = Marshal.from_string blob 0
let blob_to_envs blob = Marshal.from_string blob 0
let blob_to_sockets blob = Marshal.from_string blob 0

