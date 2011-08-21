.PHONY: clean distclean pack count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

EXEFILE=alanc$(EXE)
MLFILES= Error.ml Hashcons.ml Identifier.ml Types.ml AlanString.ml  \
  Symbol.ml Printing.ml QuadTypes.ml Semantic.ml Quads.ml Lexer.ml  \
  Parser.ml Blocks.ml OptimizationSupport.ml ControlFlow.ml SSA.ml  \
  CodeElimination.ml CopyPropagation.ml TailRecursion.ml            \
  FinalTypes.ml Optimizations.ml FinalOptimizations.ml Final.ml     \
  Main.ml 
MLIFILES=Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
  QuadTypes.mli Parser.mli Lexer.mli Blocks.mli CopyPropagation.mli \
  CodeElimination.mli TailRecursion.mli ControlFlow.mli Final.mli
CMOFILES=$(patsubst %.ml,%.cmo,$(MLFILES))
CMIFILES=$(patsubst %.ml,%.cmi,$(MLFILES))
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))
OBJFILES=$(patsubst %.ml,%.o,$(MLFILES))
PARSERFILES=Parser.ml Parser.mli Parser.output Lexer.ml
SRCFILES=Makefile extend.ml Lexer.mll Parser.mly \
  $(filter-out Parser.% Lexer.%,$(MLFILES)) \
  $(filter-out Parser.%,$(MLIFILES))

CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"
OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OCAMLC=ocamlc $(OCAMLC_FLAGS)
OCAMLOPT=ocamlopt $(OCAMLOPT_FLAGS)
OCAMLDEP=ocamldep
INCLUDES=

default: alanc$(EXE)

symbtest$(EXE): $(filter-out Lexer.cmo Parser.cmo,$(CMOFILES))
	$(OCAMLC) -o $@ $^

all: $(EXEFILE)

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I +camlp5 -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

.PHONY: all clean count depend

$(EXEFILE): Parser.mli Lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(CMOFILES)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

depend-symbtest: Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml \
                 Symbtest.ml Hashcons.mli Identifier.mli Error.mli Types.mli \
                 Symbol.mli extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILE) \
           extend.cmi extend.cmo \
           $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) symbtest$(EXE) .depend

pack: clean
	tar cvfz gracec.tar.gz $(SRCFILES)

bonus.zip: distclean
	zip bonus.zip README Makefile extend.ml \
	    Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
	    Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml Symbtest.ml

bonus.tgz: distclean
	tar cvfz bonus.tgz README Makefile extend.ml \
	    Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
	    Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml Symbtest.ml

count:
	wc -l $(SRCFILES)
