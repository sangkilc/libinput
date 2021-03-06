libInput: program input representation in OCaml
===============================================

This library presents a set of APIs to manipulate program inputs, i.e.,
arguments, files, environment variables, network sockets, etc. The primary
motivation of libInput is to provide a general-purpose APIs for handling inputs
for designing a program analysis tool.

Install
-------

Simply run "make" to compile; "make unittest" to perform a unittest.

libInput requires the followings:

- OCaml (>= 4.02.1)
- OCaml Batteries (http://batteries.forge.ocamlcore.org/)
- Yojson (http://mjambon.com/yojson.html)
- OUnit (optional for unittesting)

Documents
---------

Find more information from [docs](docs/README.md).

