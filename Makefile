OCAMLPATH = ..

all: eval.byte otrui.byte system.cmo plugins/pipe.cmo pipe.cmi

OTRUI_SOURCES = log.ml term.ml buf.ml view.ml win.ml cmd.ml otrui.ml
OTHER_SOURCES = system.ml plugins/pipe.ml
ML_SOURCES = $(OTRUI_SOURCES) $(OTHER_SOURCES)

REQUIRES = unix bricabrac pfds curses

include make.common

pipe.cmi: plugins/pipe.cmi
	ln -s $^ $@

otrui.byte: $(OTRUI_SOURCES:.ml=.cmo)
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg -linkall $(OCAMLFLAGS) toplevellib.cma $^

eval.byte: eval.cmo
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg -linkall $(OCAMLFLAGS) toplevellib.cma $^

otrui.cma: $(OTRUI_SOURCES:.ml=.cmo)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $^

otrui.cmxa: $(OTRUI_SOURCES:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $^
	
otrui: otrui.cma
	$(OCAMLMKTOP)  -o $@ -package "$(REQUIRES)" -g -custom $^

clean-spec:
	@rm -f otrui otrui.log

-include .depend
