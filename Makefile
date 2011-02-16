OCAMLPATH = ..

PROGRAMS = test.byte
all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

REQUIRES = bricabrac pfds curses

include make.common

.cmo.byte:
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

.cmx.opt:
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

-include .depend
