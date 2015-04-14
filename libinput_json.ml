(* libInput: program input representation *)

(** defines libinput json format

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

open Yojson.Safe
open Libinput_type
open Libinput_value

let value_to_tuple enc value =
  let s = value_to_string value in
  `Tuple [`String (if enc then BatBase64.str_encode s else s);
          `String (byte_kind_string value)]

(* string, string *)
let pair enc name _ value =
  (name, value_to_tuple enc value)

(* string, list *)
let lst enc idx input value =
  let typ = Argument.get_attrib_string input in
  string_of_int idx, `List [value_to_tuple enc value;`String typ]

(* string *)
let str enc _ _ value =
  value_to_tuple enc value

let jsonfolder g to_json enc name input acc =
  let value = g input in
  let json = to_json enc name input value in
  json::acc

let args_to_json enc args =
  let args = ArgMap.fold (jsonfolder Argument.get_value lst enc) args [] in
  if List.length args = 0 then `Null
  else `Assoc (List.rev args)

let files_to_json enc files =
  let files = FileMap.fold (jsonfolder File.get_value pair enc) files [] in
  if List.length files = 0 then `Null
  else `Assoc (List.rev files)

let stdin_to_json enc stdin =
  let stdin = StdinMap.fold (jsonfolder Stdin.get_value str enc) stdin [] in
  if List.length stdin = 0 then `Null
  else List.hd stdin

let envs_to_json enc envs =
  let envs = EnvMap.fold (jsonfolder Env.get_value pair enc) envs [] in
  if List.length envs = 0 then `Null
  else `Assoc (List.rev envs)

let sockets_to_json enc socks =
  let socks = SocketMap.fold (jsonfolder Socket.get_value pair enc) socks [] in
  if List.length socks = 0 then `Null
  else `Assoc (List.rev socks)

let append_to_obj ((typ, json) as child) obj =
  if json <> `Null then child::obj else obj

let vector_to_json enc vector : json =
  let encoding =
    if enc then ("encoding", `String "base64") else ("encoding", `Null)
  in
  let args = args_to_json enc vector.args in
  let files = files_to_json enc vector.files in
  let stdin = stdin_to_json enc vector.stdin in
  let envs = envs_to_json enc vector.envs in
  let sockets = sockets_to_json enc vector.sockets in
  assert (args <> `Null);
  let obj =
       append_to_obj ("files", files) [("args", args)]
    |> append_to_obj ("stdin", stdin)
    |> append_to_obj ("envs", envs)
    |> append_to_obj ("sockets", sockets)
    |> append_to_obj encoding
    |> List.rev
  in
  `Assoc obj

let to_json_string enc vector =
  vector_to_json enc vector |> pretty_to_string ~std:true

let unwrap_json_array = function
  | `List lst -> lst
  | _ -> failwith "not an array"

let unwrap_json_string = function
  | `String s -> s
  | _ -> failwith "not a string"

let unwrap_json_object = function
  | `Assoc obj -> obj
  | _ -> failwith "not an assoc"

let read_file path =
  let ic = open_in path in
  let v = BatPervasives.input_all ic in
  close_in ic;
  v

let unwrap_json_value base64 = function
  | `List [`String s; `String k] ->
      (if base64 then BatBase64.str_decode s else s), Some k
  | `List [`String s] ->
      (if base64 then BatBase64.str_decode s else s), None
  | `Assoc [("path", `String p)] ->
      let s = read_file p in
      (* when we load from a real file, we make every byte symbolic *)
      let k = String.make (String.length s) '1' in
      (if base64 then BatBase64.str_decode s else s), Some k
  | `Assoc [("path", `List [`String p; `String k])] ->
      let s = read_file p in
      (if base64 then BatBase64.str_decode s else s), Some k
  | _ ->
      failwith "not a valid value-kind pair"

let json_list_to_ocaml fn lst = List.map fn lst

let json_string_tuple_list_to_ocaml base64 lst =
  json_list_to_ocaml
    (fun (k,v) -> k, unwrap_json_value base64 v) lst

let json_args base64 lst =
  json_list_to_ocaml
    (fun (_, arg) -> match unwrap_json_array arg with
      | arg::typ::[] ->
          unwrap_json_value base64 arg,
          string_to_arg_attrib (unwrap_json_string typ)
      | arg::[] ->
          unwrap_json_value base64 arg,
          Argument.default_attrib
      | _ -> failwith "invalid arguments"
    ) lst

let json_files base64 lst =
  if lst = [] then []
  else json_string_tuple_list_to_ocaml base64 lst

let json_stdin base64 lst =
  Some (unwrap_json_value base64 lst)

let json_envs base64 lst =
  if lst = [] then []
  else json_string_tuple_list_to_ocaml base64 lst

let json_socket base64 lst =
  if lst = [] then []
  else json_string_tuple_list_to_ocaml base64 lst

let of_json_string str =
  let json = from_string str in
  let tbl = unwrap_json_object json in
  let base64 = List.mem ("encoding", `String "base64") tbl in
  let rec parse_loop args files stdin envs sockets = function
  | ("args", `Assoc lst)::tl ->
      if args <> [] then failwith "duplicated args"
      else parse_loop (json_args base64 lst) files stdin envs sockets tl
  | ("files", `Assoc lst)::tl ->
      if files <> [] then failwith "duplicated files"
      else parse_loop args (json_files base64 lst) stdin envs sockets tl
  | ("stdin", lst)::tl ->
      if stdin <> None then failwith "duplicated stdin"
      else parse_loop args files (json_stdin base64 lst) envs sockets tl
  | ("envs", `Assoc lst)::tl ->
      if envs <> [] then failwith "duplicated envs"
      else parse_loop args files stdin (json_envs base64 lst) sockets tl
  | ("sockets", `Assoc lst)::tl ->
      if sockets <> [] then failwith "duplicated sockets"
      else parse_loop args files stdin envs (json_socket base64 lst) tl
  | ("encoding", _)::tl ->
      parse_loop args files stdin envs sockets tl
  | [] -> args, files, stdin, envs, sockets
  | (form, json)::_ ->
      Printf.eprintf "(%s, %s)\n" form (to_string json);
      failwith "invalid json format"
  in
  parse_loop [] [] None [] [] tbl

