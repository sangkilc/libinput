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

let strip_byte = function
  | Concrete ch -> ch
  | Symbolic ch -> ch

(** Input value to string *)
let value_to_string ?(tohex=false) value =
  let value_to_hexstring value =
    let s = Bytes.create ((Array.length value) * 2) in
    let to_str i c =
      let c = strip_byte c |> int_of_char |> Printf.sprintf "%02x" in
      Bytes.set s (i*2) c.[0]; Bytes.set s (i*2 + 1) c.[1]
    in
    Array.iteri to_str value; s
  in
  let value_to_string value =
    let s = Bytes.create (Array.length value) in
    let to_str i c = Bytes.set s i (strip_byte c) in
    Array.iteri to_str value; s
  in
  if tohex then value_to_hexstring value
  else value_to_string value

let string_to_conc_val s =
  Array.init (String.length s) (fun i -> Concrete s.[i])

let valuestr_to_value (s, k) =
  let valuestr_to_value s k =
    assert (String.length s = String.length k);
    Array.init (String.length s)
      (fun i ->
        if k.[i] = '0' then Concrete s.[i]
        else if k.[i] = '1' then Symbolic s.[i]
        else failwith "invalid kind string"
      )
  in
  match k with
  | None -> string_to_conc_val s
  | Some k -> valuestr_to_value s k

let byte_kind_string v =
  let s = Bytes.create (Array.length v) in
  let kind_check idx = function
    | Concrete _ -> Bytes.set s idx '0'
    | Symbolic _ -> Bytes.set s idx '1'
  in
  Array.iteri kind_check v; Bytes.to_string s

(** Get a character at a position (pos) from an input value *)
let get_char_value value pos =
  Array.get value pos |> strip_byte

(** Value folding *)
let value_fold = BatArray.fold_righti

let convert_byte convert value pos =
  let acc = Array.copy value in
  Array.iteri (fun idx ch ->
    Array.set acc idx (convert ch idx)
  ) value; acc

(** Make input value to have abstract value. If the original value is fully
    abstract, this function returns the original value *)
let make_abstract ?makeall:(makeall=false) value pos =
  let to_symbolic = function
    | Concrete ch -> Symbolic ch
    | ch -> ch
  in
  let conv ch idx = if (idx = pos) || makeall then to_symbolic ch else ch in
  convert_byte conv value pos

(** Make input value to have concrete value. If the original value is fully
    concrete, this function returns the original value *)
let make_concrete value pos =
  let to_conc = function
    | Symbolic ch -> Concrete ch
    | ch -> ch
  in
  let conv ch idx = if idx = pos then to_conc ch else ch in
  convert_byte conv value pos

(** Make a crossover of two values and return the new values. *)
let crossover v1 v2 point =
  let arr1 = Array.copy v1 in
  let arr2 = Array.copy v2 in
  Array.iteri (fun idx _value ->
    if idx >= point then ()
    else (Array.set arr1 idx v2.(idx); Array.set arr2 idx v1.(idx))
  ) v1;
  arr1, arr2

