OCAMLPATH = ..

all: otrui

ML_SOURCES = set_rectypes.ml log.ml term.ml buf.ml view.ml win.ml cmd.ml test.ml

REQUIRES = unix bricabrac pfds curses

include make.common

otrui.cma: $(ML_OBJS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $(ML_OBJS)

otrui.cmxa: $(ML_XOBJS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS)
	
otrui: otrui.cma
	$(OCAMLMKTOP)  -o $@ -package "$(REQUIRES)" -g -custom $^

clean-spec:
	@rm -f otrui

-include .depend
