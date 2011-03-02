OCAMLPATH = ..

all: otrui otrui.byte system.cmo plugins/toto.cmo

OTRUI_SOURCES = set_rectypes.ml log.ml term.ml buf.ml view.ml win.ml cmd.ml otrui.ml
OTHER_SOURCES = system.ml plugins/toto.ml
ML_SOURCES = $(OTRUI_SOURCES) $(OTHER_SOURCES)

REQUIRES = unix bricabrac pfds curses

include make.common

otrui.byte: $(OTRUI_SOURCES:.ml=.cmo)
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) toplevellib.cma $^

otrui.cma: $(OTRUI_SOURCES:.ml=.cmo)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $^

otrui.cmxa: $(OTRUI_SOURCES:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $^
	
otrui: otrui.cma
	$(OCAMLMKTOP)  -o $@ -package "$(REQUIRES)" -g -custom $^

clean-spec:
	@rm -f otrui

-include .depend
