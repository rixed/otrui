all: eval.byte otrui.byte system.cmo plugins/pipe.cmo

OTRUI_SOURCES = \
	otrui.ml log.ml \
	term_curses.ml term_impl.ml \
	cmd_impl.ml \
	mark_impl.ml mark_offset.ml mark_lines.ml \
	buf_impl.ml buf_repl.ml buf_file.ml \
	view_impl.ml view_text.ml \
	win_impl.ml \
	editor.ml main.ml

OTHER_SOURCES = system.ml plugins/pipe.ml
ML_SOURCES = $(OTRUI_SOURCES) $(OTHER_SOURCES)

REQUIRES = unix bricabrac pfds curses compiler-libs.toplevel

include make.common

otrui.byte: $(OTRUI_SOURCES:.ml=.cmo)
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg -linkall $(OCAMLFLAGS) $^

eval.byte: eval.cmo
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg -linkall $(OCAMLFLAGS) $^

otrui.cma: $(OTRUI_SOURCES:.ml=.cmo)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $^

otrui.cmxa: $(OTRUI_SOURCES:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $^
	
otrui: otrui.cma
	$(OCAMLMKTOP)  -o $@ -package "$(REQUIRES)" -g -custom $^

clean-spec:
	rm -f otrui otrui.log plugins/pipe.cm[ioxa]

-include .depend
