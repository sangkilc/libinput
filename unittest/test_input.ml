(* libInput: program input representation *)

(** libinput test suite

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

open OUnit2
open Libinput
open Libinput.T

let json0 =
"{
  \"args\": {
    \"0\": [ [ \"L2Jpbi9jYXQ\", \"00000000\" ], \"regular\" ],
    \"1\": [ [ \"RjBGMEYwRjA\", \"00000000\" ], \"regular\" ]
  },
  \"stdin\": [ \"QUFBQUFBQUFBQQ\", \"1111100000\" ],
  \"envs\": { \"ENV1\": [ \"WlpaWg\", \"0000\" ], \"ENV2\": [ \"WVlZWQ\", \"0000\" ] },
  \"encoding\": \"base64\"
}"

let json1 =
"{
  \"args\": {
    \"0\": [ [ \"/bin/cat\", \"00000000\" ], \"regular\" ],
    \"1\": [ [ \"F0F0F0F0\", \"00000000\" ], \"regular\" ]
  },
  \"stdin\": [ \"AAAAAAAAAA\", \"1111100000\" ],
  \"envs\": { \"ENV1\": [ \"ZZZZ\", \"0000\" ], \"ENV2\": [ \"YYYY\", \"0000\" ] }
}"

let json2 =
"{
  \"args\": {
    \"0\": [ [\"/foo/bar\", \"00000000\"], \"regular\"],
    \"1\": [ [\"/path/to/file\", \"0000000000000\"], \"in(/path/to/file)\"]
  },
  \"files\": { \"file\": [ \"AAAAAAAAAAAAAAAAAAAAAAAAAAA\" ] },
  \"envs\": { \"ENV1\": [ \"ZZZZ\", \"0000\" ], \"ENV2\": [ \"YYYY\", \"0000\" ] }
}"

let json3 =
"{
  \"args\": {
    \"0\": [ [\"/foo/bar\", \"00000000\"], \"regular\"],
    \"1\": [ [\"filearg\"], \"in(filearg)\"]
  },
  \"files\": { \"filearg\": { \"path\": \"AUTHOR\" } },
  \"envs\": { \"ENV1\": [ \"ZZZZ\", \"0000\" ], \"ENV2\": [ \"YYYY\", \"0000\" ] }
}"

let json4 =
"{
  \"args\": {
    \"0\": [ [\"/foo/bar\", \"00000000\"], \"regular\"],
    \"1\": [ [\"filearg\"], \"in(filearg)\"]
  },
  \"files\": {
    \"filearg\": { \"path\": [\"AUTHOR\",
                              \"1111111111000000000000000000000000000\"]
                 }
  },
  \"envs\": { \"ENV1\": [ \"ZZZZ\", \"0000\" ], \"ENV2\": [ \"YYYY\", \"0000\" ] }
}"

let empty_vector_test test_ctxt =
  let v = empty_vector in
  assert_equal "" (vector_to_string v false)

let basic_test test_ctxt =
  let v1 = of_json json0 in
  let v2 =
    Libinput_json.create_vector
      [(("/bin/cat", None), ArgReg); (("F0F0F0F0", None), ArgReg)]
      []
      (Some ("AAAAAAAAAA", Some "1111100000"))
      [("ENV1", ("ZZZZ", None)); ("ENV2", ("YYYY", None))]
      []
  in
  let v3 = of_json json1 in
  let v4 = of_json json3 in
  let v5 = of_json json4 in
  assert_equal (find_arg 0 v1 |> Argument.get_name) 0;
  assert_equal (find_arg 1 v2 |> Argument.get_name) 1;
  assert_equal (find_stdin v2 |> Stdin.get_length) 10;
  assert_equal (find_env "ENV1" v1 |> Env.get_length) 4;
  assert_equal (find_arg 0 v1 |> Argument.get_char 0) ('/');
  assert_equal (find_arg 1 v1 |> Argument.get_char 1) ('0');
  assert_equal (vector_to_string v1 false) (vector_to_string v2 false);
  assert_equal (vector_to_string v1 false) (vector_to_string v3 false);
  assert_bool "conc" (find_stdin v1 |> Stdin.is_concrete |> not);
  assert_bool "0" (find_stdin v2 |> Stdin.is_char_concrete 0 |> not);
  assert_bool "1" (find_stdin v2 |> Stdin.is_char_concrete 1 |> not);
  assert_bool "4" (find_stdin v2 |> Stdin.is_char_concrete 4 |> not);
  assert_bool "5" (find_stdin v2 |> Stdin.is_char_concrete 5);
  assert_raises Not_found (fun () -> find_arg 2 v1);
  assert_raises Not_found (fun () -> find_env "ENV3" v2);
  assert_raises Not_found (fun () -> find_file "/bin/cat" v2);
  assert_raises Not_found (fun () -> find_socket "" v1);
  assert_equal (find_file "filearg" v4 |> File.get_length) 37;
  assert_equal (get_binary_target v1) "/bin/cat";
  assert_bool "0" (find_file "filearg" v5 |> File.is_char_concrete 0 |> not);
  assert_bool "9" (find_file "filearg" v5 |> File.is_char_concrete 9 |> not);
  assert_bool "10" (find_file "filearg" v5 |> File.is_char_concrete 10);
  assert_equal (vector_to_string v1 false) (vector_to_string v2 false)

let cmd_test test_ctxt =
  let v = of_json json1 in
  List.iter2 assert_equal (get_commandline v) ["/bin/cat"; "F0F0F0F0"];
  assert_equal ("/bin/cat F0F0F0F0 < stdin") (get_commandline_str v)

let attrib_test test_ctxt =
  let v = of_json json2 in
  let f = FileMap.find "file" v.files in
  let a0 = ArgMap.find 0 v.args in
  let a1 = ArgMap.find 1 v.args in
  assert_equal (File.get_attrib_string f) ("0o766");
  assert_equal (Argument.get_attrib_string a0) ("regular");
  assert_equal (Argument.get_attrib_string a1) ("in(/path/to/file)")

let suite =
  "libInput Test Suite" >:::
  [
    "Empty Vector Test" >:: empty_vector_test;
    "Basic Test" >:: basic_test;
    "Cmd Test" >:: cmd_test;
    "Attribute Test" >:: attrib_test;
  ]

let _ =
  print_endline "Starting Unit Test...";
  run_test_tt_main suite

