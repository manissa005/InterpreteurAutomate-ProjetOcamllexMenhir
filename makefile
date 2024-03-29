
MENHIR=menhir
OCAMLC=ocamlc
OCAMLLEX=ocamllex

SOURCES = Type.ml parser.ml lexer.ml main.ml

OBJECTS = $(SOURCES:.ml=.cmo)

.PHONY: clean all 

all: parser

parser: Type.cmo parser.cmi parser.cmo lexer.cmo main.cmo 
	$(OCAMLC) -o $@ $(OBJECTS)

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

%.ml %.mli: %.mly
	rm -f $(<:.mly=.conflicts)
	$(MENHIR) -v --infer $<

%.ml: %.mll
	$(OCAMLLEX) $<

parser.mly: Type.ml

lexer.mll: parser.ml

clean:
	rm -fr parser.mli parser.ml lexer.ml *.cmo parser *.cmi *~ *.automaton *.conflicts

parser.cmo: Type.cmo parser.cmi
lexer.cmo: parser.cmo
main.cmo: parser.cmo lexer.cmo
