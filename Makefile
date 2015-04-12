###############################################################################
# libInput Makefile                                                           #
#                                                                             #
# Copyright (c) 2014, Sang Kil Cha                                            #
# All rights reserved.                                                        #
# This software is free software; you can redistribute it and/or              #
# modify it under the terms of the GNU Library General Public                 #
# License version 2, with the special exception on linking                    #
# described in file LICENSE.                                                  #
#                                                                             #
# This software is distributed in the hope that it will be useful,            #
# but WITHOUT ANY WARRANTY; without even the implied warranty of              #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        #
###############################################################################

all: lib

lib:
	ocamlbuild -no-links -X unittest libinput.cma libinput.cmxa

unittest:
	ocamlbuild -no-links -I unittest unittest/test_input.native
	@_build/unittest/test_input.native

install: lib
	ocamlfind install libinput META \
		_build/libinput.cmi \
		_build/libinput_type.cmi \
		_build/libinput_value.cmi \
		_build/libinput.a \
		_build/*.mli \
		_build/libinput.cmxa \
		_build/libinput.cma

clean:
	ocamlbuild -clean

.PHONY: all lib unittest install clean
