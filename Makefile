OCAMLPATH = ..

all: otrui
#top.byte otrui.cma
# test.byte otrui.cma otrui.cmxa otrui 

ML_SOURCES = test.ml

REQUIRES = unix bricabrac pfds curses

include make.common

otrui.cma: $(ML_OBJS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $(ML_OBJS)

otrui.cmxa: $(ML_XOBJS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS)
	
otrui: set_rectypes.cmo otrui.cma
	$(OCAMLMKTOP)  -o $@ -package "$(REQUIRES)" -g -custom $^

clean-spec:
	@rm -f otrui

-include .depend
