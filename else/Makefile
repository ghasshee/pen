
OBJS = label.cmo storage.cmo assoc.cmo misc.cmo hexa.cmo syntax.cmo typeEnv.cmo pseudoImm.cmo location.cmo entrypointDatabase.cmo evm.cmo ethereum.cmo locationEnv.cmo layoutInfo.cmo contract.cmo type.cmo codegenEnv.cmo codegen.cmo parser.cmo lexer.cmo support.cmo 
OCAMLC=ocamlfind ocamlc -linkpkg -package rpclib -package batteries -package cryptokit -package hex -package menhirLib -package rope -package ppx_deriving cryptokit.cma batteries.cma hex.cma rope.cma
OCAMLDEP=ocamlfind ocamldep -package batteries -package cryptokit -package hex -package menhirLib -package rope 

all: $(OBJS) 


#include .depend 

depend:: $(DEPEND) 
	${OCAMLDEP} *.mli *.ml > .depend 


%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli 
	menhir parser.mly
	@chmod -w parser.ml parser.mli

%.cmi : %.mli 
	${OCAMLC} -c $<

%.cmo : %.ml
	${OCAMLC} -c $<

pen : $(OBJS) pen.cmo 
	echo Linking $@
	$(OCAMLC)  -o $@ $(OBJS) pen.cmo 

al : 
	${OCAMLC} -c allinone.ml

allinone:
	ocamlbuild \
		-use-ocamlfind \
		-package batteries \
		-package cryptokit \
		-package rope \
		-package rpclib \
		-package unix \
		-package rpclib.json \
		-package ppx_deriving \
		-package ppx_deriving_rpc \
		-package hex \
		-use-menhir \
		allinone.native

doc/spec.pdf: doc/spec.tex
	(cd doc; pdflatex -halt-on-error spec.tex; pdflatex -halt-on-error spec.tex)

##############

clean:
	rm -rf *.cmo *.cmi rm -rf pen _build lexer.ml parser.ml parser.mli 
	unlink allinone.native
